-- TODO: 
-- centerOfMass -> Solid -> V3 Double
-- volumes :: Solid -> [Double]
-- centersOfMass :: Solid -> [V3 Double]
-- momentOfInertia
-- momentsOfInertia
module Rapids.Statistics where

import Control.Monad.IO.Class (liftIO)
import Data.Acquire (mkAcquire)
import Data.Either (fromRight)
import Foreign.C.Types (CDouble)
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import OpenCascade.TopoDS (Shape)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Waterfall (Solid)
import Waterfall.Error (WaterfallError)
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Waterfall.Internal.NearZero (nearZero)
import Waterfall.Internal.Solid
  ( acquireSolid,
    emptySolid,
    solidFromAcquireWithCatch
  )

C.context occtContext
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<BRepBuilderAPI_MakeSolid.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<BRepOffset.hxx>"
Cpp.include "<BRepOffsetAPI_MakeOffsetShape.hxx>"
Cpp.include "<GProp_GProps.hxx>"
Cpp.include "<GeomAbs_JoinType.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"

-- | Compute volume by summing the volumes of all solid components.
volume :: Solid -> Double
volume solid = unsafeFromAcquire $ do
  realToFrac <$> liftIO [Cpp.block| double {
        TopoDS_Shape* input = $solid:solid;
        if (input == nullptr || input->IsNull()) {
          return 0.0;
        }

        Standard_Real total = 0.0;
        for (TopExp_Explorer solids(*input, TopAbs_SOLID);
             solids.More(); solids.Next()) {
          GProp_GProps props;
          BRepGProp::VolumeProperties(
            solids.Current(), props, Standard_False, Standard_False, Standard_False);
          total += props.Mass();
        }
        return total;
      }|]
