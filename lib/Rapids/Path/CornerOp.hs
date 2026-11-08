-- | fillet and chamfer the corners of a 'Path'
module Rapids.Path.CornerOp where

import Control.Exception (bracket)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Control.Spoon
import Data.Coerce
import Data.Maybe
import Foreign
import Foreign.C.Types
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified Waterfall.Internal.Path as InternalPath
import Waterfall.Path (Path)
import qualified Waterfall.Path as W
import Rapids.Color

import qualified Data.Set as Set
C.context (occtContext <> C.funCtx)
Cpp.include "<ChFi2d_ChamferAPI.hxx>"
Cpp.include "<ChFi2d_FilletAPI.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<GProp_GProps.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<BRepAdaptor_Curve.hxx>"
Cpp.include "<BRepTools_WireExplorer.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopoDS_Edge.hxx>"
Cpp.include "<TopoDS_Vertex.hxx>"
Cpp.include "<TopAbs_Orientation.hxx>"
Cpp.include "<Precision.hxx>"
Cpp.include "<gp.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<algorithm>"
Cpp.include "<cmath>"
Cpp.include "<vector>"

[Cpp.emitBlock|
static bool rapids_is_tangent_join(const TopoDS_Edge& first, const TopoDS_Edge& second) {
  try {
    TopoDS_Vertex firstStart, firstEnd, secondStart, secondEnd;
    TopExp::Vertices(first, firstStart, firstEnd, Standard_True);
    TopExp::Vertices(second, secondStart, secondEnd, Standard_True);
    if (firstEnd.IsNull() || secondStart.IsNull()) return false;
    const Standard_Real joinTolerance = std::max(
      Precision::Confusion(),
      std::max(BRep_Tool::Tolerance(firstEnd), BRep_Tool::Tolerance(secondStart)));
    if (BRep_Tool::Pnt(firstEnd).Distance(BRep_Tool::Pnt(secondStart)) > joinTolerance) return false;

    auto tangentAt = [](const TopoDS_Edge& edge, const bool atEnd) {
      BRepAdaptor_Curve curve(edge);
      const bool reversed = edge.Orientation() == TopAbs_REVERSED;
      const bool useLastParameter = atEnd != reversed;
      const Standard_Real parameter = useLastParameter ? curve.LastParameter() : curve.FirstParameter();
      gp_Pnt point;
      gp_Vec tangent;
      curve.D1(parameter, point, tangent);
      if (reversed) tangent.Reverse();
      return tangent;
    };

    const gp_Vec firstTangent = tangentAt(first, true);
    const gp_Vec secondTangent = tangentAt(second, false);
    // SVG export rounding can perturb otherwise tangent joins by a small angle.
    constexpr Standard_Real tangentTolerance = 2.0e-3;
    return firstTangent.SquareMagnitude() > Precision::SquareConfusion() &&
           secondTangent.SquareMagnitude() > Precision::SquareConfusion() &&
           firstTangent.Dot(secondTangent) > 0.0 &&
           firstTangent.IsParallel(secondTangent, tangentTolerance);
  } catch (Standard_Failure const&) {
    return false;
  }
}
|]

-- | @filletPath r path@ rounds every corner of the path with radius @r@. Colors come from the left edge, see 'filletPathWithColor'.
filletPath :: Double -> Path -> Path
filletPath r = filletPathWithColor leftColor r

-- | @chamferPath r path@ cuts every corner of the path with distance @r@. Colors come from the left edge, see 'chamferPathWithColor'.
chamferPath :: Double -> Path -> Path
chamferPath r = chamferPathWithColor leftColor r

-- | like 'filletPath' with a choice of how the edge colors are propagated
filletPathWithColor :: OpC -> Double -> Path -> Path
filletPathWithColor policy r = applyOpWithColor policy (\_ -> (1, r))

-- | Fillet only the selected 1-based corner indices. Negative-only indices
-- exclude corners; positive indices select only those corners. Colors come
-- from the left edge, as in 'filletPath'.
filletPathWithIndices :: Double -> [Int] -> Path -> Path
filletPathWithIndices radius indices =
  let included = Set.fromList [toInteger index | index <- indices, index > 0]
      excluded =
        Set.fromList
          [ negate (toInteger index)
            | index <- indices,
              index < 0
          ]
      includeAll = Set.null included
      selected index =
        let publicIndex = toInteger index + 1
         in (includeAll || Set.member publicIndex included)
              && not (Set.member publicIndex excluded)
   in applyOpWithColor leftColor \(index, _, _) ->
        (1, if selected index then radius else 0)

-- | like 'chamferPath' with a choice of how the edge colors are propagated
chamferPathWithColor :: OpC -> Double -> Path -> Path
chamferPathWithColor policy r = applyOpWithColor policy (\_ -> (0, r))

-- | @applyOp \(index, leftLength, rightLength) -> (if chamfer then 0 else 1, r)@
applyOp :: ((CInt, Double, Double) -> (CInt, Double)) -> Path -> Path
applyOp = applyOpWithColor leftColor

-- | like 'applyOp' with a choice of how the edge colors are propagated
applyOpWithColor :: OpC -> ((CInt, Double, Double) -> (CInt, Double)) -> Path -> Path
applyOpWithColor policy f path =
  let output = case toCornerOpF f of
        OpConst operation radius -> fromMaybe path (applyOpConst operation (CDouble radius) path)
        OpF callback -> applyOpFromFunction callback path
   in propagatePathEdgeColors policy path output

toCornerOpF :: ((CInt, Double, Double) -> (CInt, Double)) -> OpF
toCornerOpF f
  | Just (op, radius) <- spoon (f (undefined, undefined, undefined)) = OpConst op radius
  | otherwise = OpF (coerce f)

data OpF = OpConst CInt Double | OpF ((CInt, CDouble, CDouble) -> (CInt, CDouble))

applyOpFromFunction ::
  ((CInt, CDouble, CDouble) -> (CInt, CDouble)) ->
  Path ->
  Path
applyOpFromFunction f path = unsafePerformIO $
  bracket
    ($(C.mkFunPtr [t| CInt -> CDouble -> CDouble -> Ptr CDouble -> IO CInt |])
      $ \index left right radiusPtr -> do
          let (operation, radius) = f (index, left, right)
          poke radiusPtr radius
          pure operation)
    freeHaskellFunPtr
    (pure . (`applyOpWithCallback` path))

applyOpWithCallback ::
  FunPtr (CInt -> CDouble -> CDouble -> Ptr CDouble -> IO CInt) ->
  Path ->
  Path
applyOpWithCallback callback path =
  fromMaybe path $ do
    let edgeCount = fromIntegral $ length (InternalPath.allPathEndpoints path)
    guard (edgeCount >= 2)
    Just $
      ownPath $
        [Cpp.block| TopoDS_Wire* {
            TopoDS_Wire* input = $path:path;
            int (*callback)(int, double, double, double*) =
              $(int (*callback)(int, double, double, double*));
            if (input == nullptr || input->IsNull() || callback == nullptr) return nullptr;
            try {
              std::vector<TopoDS_Edge> edges;
              BRepTools_WireExplorer explorer(*input);
              for (; explorer.More(); explorer.Next()) edges.push_back(explorer.Current());
              const int edgeCount = static_cast<int>(edges.size());
              if (edgeCount < 2) return nullptr;
              const int cornerCount = BRep_Tool::IsClosed(*input) ? edgeCount : edgeCount - 1;
              std::vector<TopoDS_Edge> cornerEdges(static_cast<size_t>(cornerCount));
              std::vector<unsigned char> changed(static_cast<size_t>(cornerCount), 0);
              for (int corner = 0; corner < cornerCount; ++corner) {
                const int next = (corner + 1) % edgeCount;
                TopoDS_Edge edge1 = edges[corner];
                TopoDS_Edge edge2 = edges[next];
                if (rapids_is_tangent_join(edge1, edge2)) continue;
                GProp_GProps leftProps;
                GProp_GProps rightProps;
                BRepGProp::LinearProperties(edge1, leftProps);
                BRepGProp::LinearProperties(edge2, rightProps);
                double radius = 0.0;
                const int op = callback(corner, leftProps.Mass(), rightProps.Mass(), &radius);
                if (!(radius > 0.0) || !std::isfinite(radius)) continue;
                try {
                  TopoDS_Edge cornerEdge;
                  if (op == 0) {
                    ChFi2d_ChamferAPI chamfer(edge1, edge2);
                    if (chamfer.Perform()) cornerEdge = chamfer.Result(edge1, edge2, radius, radius);
                  } else if (op == 1) {
                    ChFi2d_FilletAPI fillet(edge1, edge2, gp_Pln(gp::XOY()));
                    if (fillet.Perform(radius)) {
                      Standard_Real first = 0.0, last = 0.0;
                      Handle(Geom_Curve) curve = BRep_Tool::Curve(edge1, first, last);
                      if (!curve.IsNull() && fillet.NbResults(curve->Value(last)) > 0)
                        cornerEdge = fillet.Result(curve->Value(last), edge1, edge2);
                    }
                  }
                  if (!cornerEdge.IsNull()) {
                    edges[corner] = edge1;
                    edges[next] = edge2;
                    cornerEdges[corner] = cornerEdge;
                    changed[corner] = 1;
                  }
                } catch (Standard_Failure const&) {}
              }
              BRepBuilderAPI_MakeWire wireBuilder;
              for (int index = 0; index < edgeCount; ++index) {
                wireBuilder.Add(edges[index]);
                if (index < cornerCount && changed[index] != 0) wireBuilder.Add(cornerEdges[index]);
              }
              if (!wireBuilder.IsDone()) return nullptr;
              return new TopoDS_Wire(wireBuilder.Wire());
            } catch (Standard_Failure const&) { return nullptr;
            } catch (...) { return nullptr; }
          } |]


applyOpConst :: CInt -> CDouble -> Path -> Maybe Path
applyOpConst operation radius path = do
  let edgeCount = fromIntegral $ length (InternalPath.allPathEndpoints path)
  guard (edgeCount >= 2)
  Just $
    ownPath $
      [Cpp.block| TopoDS_Wire* {
          TopoDS_Wire* input = $path:path;
          const int op = $(int operation);
          const double radius = $(double radius);
          if (input == nullptr || input->IsNull() || !(radius > 0.0) || !std::isfinite(radius)) {
            return nullptr;
          }
          try {
            std::vector<TopoDS_Edge> edges;
            BRepTools_WireExplorer explorer(*input);
            for (; explorer.More(); explorer.Next()) edges.push_back(explorer.Current());
            const int edgeCount = static_cast<int>(edges.size());
            if (edgeCount < 2) return nullptr;
            const int cornerCount = BRep_Tool::IsClosed(*input) ? edgeCount : edgeCount - 1;
            std::vector<TopoDS_Edge> cornerEdges(static_cast<size_t>(cornerCount));
            std::vector<unsigned char> changed(static_cast<size_t>(cornerCount), 0);
            for (int corner = 0; corner < cornerCount; ++corner) {
              const int next = (corner + 1) % edgeCount;
              TopoDS_Edge edge1 = edges[corner];
              TopoDS_Edge edge2 = edges[next];
              if (rapids_is_tangent_join(edge1, edge2)) continue;
              try {
                TopoDS_Edge cornerEdge;
                if (op == 0) {
                  ChFi2d_ChamferAPI chamfer(edge1, edge2);
                  if (chamfer.Perform()) cornerEdge = chamfer.Result(edge1, edge2, radius, radius);
                } else if (op == 1) {
                  ChFi2d_FilletAPI fillet(edge1, edge2, gp_Pln(gp::XOY()));
                  if (fillet.Perform(radius)) {
                    Standard_Real first = 0.0, last = 0.0;
                    Handle(Geom_Curve) curve = BRep_Tool::Curve(edge1, first, last);
                    if (!curve.IsNull() && fillet.NbResults(curve->Value(last)) > 0)
                      cornerEdge = fillet.Result(curve->Value(last), edge1, edge2);
                  }
                }
                if (!cornerEdge.IsNull()) {
                  edges[corner] = edge1;
                  edges[next] = edge2;
                  cornerEdges[corner] = cornerEdge;
                  changed[corner] = 1;
                }
              } catch (Standard_Failure const&) {}
            }
            BRepBuilderAPI_MakeWire wireBuilder;
            for (int index = 0; index < edgeCount; ++index) {
              wireBuilder.Add(edges[index]);
              if (index < cornerCount && changed[index] != 0) wireBuilder.Add(cornerEdges[index]);
            }
            if (!wireBuilder.IsDone()) return nullptr;
            return new TopoDS_Wire(wireBuilder.Wire());
          } catch (Standard_Failure const&) { return nullptr;
          } catch (...) { return nullptr; }
        } |]
