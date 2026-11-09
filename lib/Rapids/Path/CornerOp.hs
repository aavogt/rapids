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
filletPathWithIndices = indexedPathCornerOp 1

-- | Chamfer only the selected 1-based corner indices, using the same index
-- and color-propagation rules as 'filletPathWithIndices'.
chamferPathWithIndices :: Double -> [Int] -> Path -> Path
chamferPathWithIndices = indexedPathCornerOp 0

indexedPathCornerOp :: CInt -> Double -> [Int] -> Path -> Path
indexedPathCornerOp operation radius indices input =
  let included = [fromIntegral index :: CLLong | index <- indices, index > 0]
      excluded =
        [ fromInteger magnitude :: CLLong
          | index <- indices,
            index < 0,
            let magnitude = negate (toInteger index),
            magnitude <= toInteger (maxBound :: CLLong)
        ]
      includeAll = if null included then 1 else 0 :: CInt
      radiusValue = CDouble radius
      includedCount = fromIntegral (length included) :: CInt
      excludedCount = fromIntegral (length excluded) :: CInt
      edgeCount = fromIntegral $ length (InternalPath.allPathEndpoints input)
      output = fromMaybe input $ do
        guard (edgeCount >= 2)
        Just $
          ownPath $
            withArray included $ \includedPtr ->
              withArray excluded $ \excludedPtr ->
                [Cpp.block| TopoDS_Wire* {
                  TopoDS_Wire* input = $path:input;
                  if (input == nullptr || input->IsNull()) return nullptr;
                  const double radius = $(double radiusValue);
                  if (!(radius > 0.0) || !std::isfinite(radius)) return nullptr;
                  const long long* included = (const long long*)$(long long* includedPtr);
                  const long long* excluded = (const long long*)$(long long* excludedPtr);
                  const int includedCount = $(int includedCount);
                  const int excludedCount = $(int excludedCount);
                  const bool includeAll = $(int includeAll) != 0;
                  const int op = $(int operation);
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
                      const int publicIndex = corner + 1;
                      bool selected = includeAll;
                      for (int i = 0; i < includedCount; ++i) {
                        if (included[i] == publicIndex) { selected = true; break; }
                      }
                      if (!includeAll && !selected) continue;
                      bool isExcluded = false;
                      for (int i = 0; i < excludedCount; ++i) {
                        if (excluded[i] == publicIndex) { isExcluded = true; break; }
                      }
                      if (isExcluded) continue;
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
                }|]
   in propagatePathEdgeColors leftColor input output

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
