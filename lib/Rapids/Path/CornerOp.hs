module Rapids.Path.CornerOp where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Acquire
import Data.Maybe
import Foreign
import Foreign.C.Types
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear
import OpenCascade.TopoDS.Types
import Waterfall.Internal.Path.Common
import Waterfall.Path (Path)
import qualified Waterfall.Path as W
import qualified Waterfall.Internal.Path as InternalPath
import OpenCascade.TopoDS.Internal.Destructors
import Waterfall.Internal.Finalizers

C.context occtContext
Cpp.include "<ChFi2d_ChamferAPI.hxx>"
Cpp.include "<ChFi2d_FilletAPI.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
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

-- | write x instead of having to write [x]
class ToRadii a where
  toRadii :: a -> [CDouble]

-- default
instance {-# INCOHERENT #-} d ~ CDouble => ToRadii d where
  toRadii = (:[])

instance cdouble ~ CDouble => ToRadii [cdouble] where
  toRadii = id

-- | applyCornerOperation op requested radii
--
-- 0 chamfer
-- 1 fillet
-- requested number of operations
applyCornerOperation :: (Monad m, ToRadii a) => CInt -- ^ 0 chamfer 1 fillet
  -> CInt  -- ^ requested number
  -> a -- ^ radii as Double or [Double]
  -> StateT (V3 Double, Path) m ()
applyCornerOperation operation requested radii = modify \es@(e, s) -> fromMaybe es do
  s <- s & applyCornerOperation1 operation requested (toRadii radii)
  Just (maybe e snd (W.pathEndpoints3D s), s)

applyCornerOperation1 :: CInt -- ^ 0 chamfer, 1 fillet
  -> CInt -- ^ number of operations
  -> [CDouble] -- ^ radius/radii
  -> Path -> Maybe Path
applyCornerOperation1 operation requested radii path = do
  let edgeCount = fromIntegral $ length (InternalPath.allPathEndpoints path)
      count = min (max 0 requested) edgeCount
      values = take (fromIntegral count) radii
      requested' :: CInt
      requested' = fromIntegral (max 0 (min requested (fromIntegral (maxBound :: CInt))))
  guard (edgeCount >= 2 && not (null values))
  Just $ ownPath
            $ withArrayLen values $ \(fromIntegral -> nvalues) valuesPtr ->
                [Cpp.block| TopoDS_Wire* {
            TopoDS_Wire* input = $path:path;
            const int op = $(int operation);
            const int requestedCount = $(int requested);
            const int nvalues = $(int nvalues);
            const double* radii = $(double* valuesPtr);

        if (input == nullptr || input->IsNull() || requestedCount <= 0 || nvalues <= 0) {
          return nullptr;
        }

        try {
          std::vector<TopoDS_Edge> edges;
          BRepTools_WireExplorer explorer(*input);
          for (; explorer.More(); explorer.Next()) {
            edges.push_back(explorer.Current());
          }

          const int edgeCount = static_cast<int>(edges.size());
          if (edgeCount < 2) {
            return nullptr;
          }

          const int cornerCount = BRep_Tool::IsClosed(*input) ? edgeCount : edgeCount - 1;
          const int count = std::min(requestedCount, std::min(nvalues, cornerCount));
          if (count <= 0) {
            return nullptr;
          }

          const int firstCorner = cornerCount - count;
          std::vector<TopoDS_Edge> cornerEdges(static_cast<size_t>(cornerCount));
          std::vector<unsigned char> changed(static_cast<size_t>(cornerCount), 0);

          for (int offset = 0; offset < count; ++offset) {
            const int valueIndex = count - 1 - offset;
            const double radius = radii[valueIndex];
            if (!(radius > 0.0) || !std::isfinite(radius)) {
              continue;
            }

            const int corner = firstCorner + valueIndex;
            const int next = (corner + 1) % edgeCount;
            TopoDS_Edge edge1 = edges[corner];
            TopoDS_Edge edge2 = edges[next];

            try {
              TopoDS_Edge cornerEdge;
              if (op == 0) {
                ChFi2d_ChamferAPI chamfer(edge1, edge2);
                if (chamfer.Perform()) {
                  cornerEdge = chamfer.Result(edge1, edge2, radius, radius);
                }
              } else if (op == 1) {
                ChFi2d_FilletAPI fillet(edge1, edge2, gp_Pln(gp::XOY()));
                if (fillet.Perform(radius)) {
                  Standard_Real first = 0.0;
                  Standard_Real last = 0.0;
                  Handle(Geom_Curve) curve = BRep_Tool::Curve(edge1, first, last);
                  if (!curve.IsNull() && fillet.NbResults(curve->Value(last)) > 0) {
                    cornerEdge = fillet.Result(curve->Value(last), edge1, edge2);
                  }
                }
              }

              if (!cornerEdge.IsNull()) {
                edges[corner] = edge1;
                edges[next] = edge2;
                cornerEdges[corner] = cornerEdge;
                changed[corner] = 1;
              }
            } catch (Standard_Failure const&) {
              // Leave this corner unchanged when its radius cannot be constructed.
            }
          }

          BRepBuilderAPI_MakeWire wireBuilder;
          for (int index = 0; index < edgeCount; ++index) {
            wireBuilder.Add(edges[index]);
            if (index < cornerCount && changed[index] != 0) {
              wireBuilder.Add(cornerEdges[index]);
            }
          }
          if (!wireBuilder.IsDone()) {
            return nullptr;
          }
          return new TopoDS_Wire(wireBuilder.Wire());
        } catch (Standard_Failure const&) {
          return nullptr;
        } catch (...) {
          return nullptr;
        }
      } |]
