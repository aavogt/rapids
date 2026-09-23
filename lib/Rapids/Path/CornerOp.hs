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

C.context (occtContext <> C.funCtx)
Cpp.include "<ChFi2d_ChamferAPI.hxx>"
Cpp.include "<ChFi2d_FilletAPI.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<GProp_GProps.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<BRepTools_WireExplorer.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopoDS_Vertex.hxx>"
Cpp.include "<gp.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<algorithm>"
Cpp.include "<cmath>"
Cpp.include "<vector>"

-- Calling a foreign export has a lot of overhead: it creates a complete new Haskell thread, for example.
toCornerOpF :: ((CInt, Double, Double) -> (CInt, Double)) -> OpF
toCornerOpF f
  | Just (op, radius) <- spoon (f (undefined, undefined, undefined)) = OpConst op radius
  | otherwise = OpF (coerce f)

data OpF = OpConst CInt Double | OpF ((CInt, CDouble, CDouble) -> (CInt, CDouble))


filletPath r = applyOp \_ -> (1, r)
chamferPath r = applyOp \_ -> (0, r)

-- | applyOp \(index, leftLength, rightLength) -> (0, r)
applyOp ::
  ((CInt, Double, Double) -> (CInt,Double)) ->
  Path ->
  Path
applyOp f path = case toCornerOpF f of
  (OpConst operation radius) -> fromMaybe path (applyOpConst operation (CDouble radius) path)
  OpF f -> applyOpFromFunction f path

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
