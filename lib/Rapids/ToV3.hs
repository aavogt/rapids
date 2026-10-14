{-# LANGUAGE QuasiQuotes #-}
module Rapids.ToV3 (ToV3 (..), ToV3s (..)) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Array (allocaArray, peekArray)
import InlineOCCT (occtContext)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear (V3 (..))
import Rapids.Reexports
import System.IO.Unsafe (unsafePerformIO)
import Waterfall.Internal.Path (allPathEndpoints)

C.context occtContext
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopTools_IndexedMapOfShape.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Vertex.hxx>"
Cpp.include "<gp_Pnt.hxx>"

-- | Convert a value to a 3D vector.
class ToV3 a where toV3 :: a -> V3 Double

instance (v ~ V3) => ToV3 (E v) where toV3 (E e) = 0 & e .~ 1
instance {-# INCOHERENT #-} (a ~ V3 Double) => ToV3 a where toV3 = id

-- | Extract the 3D vertices represented by a geometry value.
class ToV3s a where toV3s :: a -> [V3 Double]

instance ToV3s Path where
  toV3s = concatMap (\(start, end) -> [start, end]) . allPathEndpoints

instance ToV3s Path2D where
  toV3s = toV3s . fromPath2D

instance ToV3s Shape where
  toV3s = concatMap toV3s . shapePaths

instance ToV3s Solid where
  toV3s = solidVertices

solidVertices :: Solid -> [V3 Double]
solidVertices solid = unsafePerformIO do
  count <- fromIntegral <$> [Cpp.block| int {
    TopoDS_Shape* input = $solid:solid;
    if (input == nullptr || input->IsNull()) {
      return 0;
    }
    TopTools_IndexedMapOfShape vertices;
    TopExp::MapShapes(*input, TopAbs_VERTEX, vertices);
    return vertices.Extent();
  } |]
  allocaArray (3 * count) $ \output -> do
    liftIO [Cpp.block| void {
      TopoDS_Shape* input = $solid:solid;
      double* output = $(double *output);
      if (input == nullptr || input->IsNull()) {
        return;
      }
      TopTools_IndexedMapOfShape vertices;
      TopExp::MapShapes(*input, TopAbs_VERTEX, vertices);
      for (int i = 1; i <= vertices.Extent(); ++i) {
        gp_Pnt point = BRep_Tool::Pnt(TopoDS::Vertex(vertices(i)));
        output[3 * (i - 1)] = point.X();
        output[3 * (i - 1) + 1] = point.Y();
        output[3 * (i - 1) + 2] = point.Z();
      }
    } |]
    values <- peekArray (3 * count) output
    pure [V3 (realToFrac x) (realToFrac y) (realToFrac z) | [x, y, z] <- triples values]
  where
    triples (x : y : z : rest) = [x, y, z] : triples rest
    triples _ = []
{-# NOINLINE solidVertices #-}
