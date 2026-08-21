module Rapids.AABB where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Array (allocaArray, peekArray)
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Data.List (mapAccumL, sortOn)
import Linear (V3 (..), ez)
import Waterfall (Solid, union, unions)
import Waterfall.Internal.Finalizers (unsafeFromAcquire)

C.context occtContext
Cpp.include "<Bnd_Box.hxx>"
Cpp.include "<BRepBndLib.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"

-- | Return the smallest axis-aligned bounding box containing the solid.
--
-- Unlike waterfall-cad's implementation, this does not use its volume
-- predicate: Rapids.Statistics.volume handles compounds of solids that
-- waterfall-cad may report as having zero volume.
axisAlignedBoundingBox :: Solid -> Maybe (V3 Double, V3 Double)
axisAlignedBoundingBox solid = unsafeFromAcquire $ liftIO $ allocaArray 6 $ \output -> do
  valid <- [Cpp.block| int {
    TopoDS_Shape* input = $solid:solid;
    if (input == nullptr || input->IsNull()) {
      return 0;
    }

    try {
      Bnd_Box bounds;
      BRepBndLib::AddOptimal(*input, bounds, Standard_True, Standard_False);
      if (bounds.IsVoid() || bounds.IsOpen()) {
        return 0;
      }

      Standard_Real xMin, yMin, zMin, xMax, yMax, zMax;
      bounds.Get(xMin, yMin, zMin, xMax, yMax, zMax);
      double* output = $(double *output);
      output[0] = xMin;
      output[1] = yMin;
      output[2] = zMin;
      output[3] = xMax;
      output[4] = yMax;
      output[5] = zMax;
      return 1;
    } catch (Standard_Failure const&) {
      return 0;
    } catch (...) {
      return 0;
    }
  }|]
  if valid == 0
    then pure Nothing
    else do
      [xMin, yMin, zMin, xMax, yMax, zMax] <- map realToFrac <$> peekArray 6 output
      pure $ Just (V3 xMin yMin zMin, V3 xMax yMax zMax)


