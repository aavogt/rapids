{-# LANGUAGE QuasiQuotes #-}
{- HLINT ignore "Eta reduce" -}

module Rapids.Offset where

import Control.Monad.IO.Class (liftIO)
import Data.Acquire (mkAcquire)
import Data.Either (fromRight)
import Foreign
import Foreign.C.Types (CDouble, CBool)
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

-- | Offset every solid component and retain a compound when there are many.
offsetShape :: Double -> Double -> CBool -> Solid -> IO (Ptr Shape)
offsetShape tolerance value arcIntersection solid =
  [Cpp.block| TopoDS_Shape* {
    TopoDS_Shape* result = new TopoDS_Shape();
    TopoDS_Shape* input = $solid:solid;
    if (input == nullptr || input->IsNull()) {
      return result;
    }

    try {
      TopoDS_Compound compound;
      BRep_Builder compoundBuilder;
      compoundBuilder.MakeCompound(compound);
      TopoDS_Shape first;
      Standard_Integer count = 0;

      for (TopExp_Explorer solids(*input, TopAbs_SOLID);
           solids.More(); solids.Next()) {
        BRepOffsetAPI_MakeOffsetShape offset;
        offset.PerformByJoin(
          solids.Current(),
          $(double value'),
          $(double tolerance'),
          BRepOffset_Skin,
          Standard_False,
          Standard_False,
          $(bool arcIntersection) ? GeomAbs_Arc : GeomAbs_Intersection,
          Standard_False);

        TopoDS_Shape offsetShape = offset.Shape();
        BRepBuilderAPI_MakeSolid solidBuilder;
        Standard_Integer shellCount = 0;
        for (TopExp_Explorer shells(offsetShape, TopAbs_SHELL);
             shells.More(); shells.Next()) {
          solidBuilder.Add(TopoDS::Shell(shells.Current()));
          ++shellCount;
        }
        if (shellCount == 0 || !solidBuilder.IsDone()) {
          return result;
        }

        TopoDS_Shape component = solidBuilder.Solid();
        if (count == 0) {
          first = component;
        }
        compoundBuilder.Add(compound, component);
        ++count;
      }

      if (count == 0) {
        return result;
      }
      if (count == 1) {
        *result = first;
      } else {
        *result = compound;
      }
    } catch (Standard_Failure const&) {
      return result;
    } catch (...) {
      return result;
    }
    return result;
  }|]
  where
    value' :: CDouble
    value' = realToFrac value
    tolerance' :: CDouble
    tolerance' = realToFrac tolerance

offsetWithTolerance :: Double -> Double -> CBool -> Solid -> Solid
offsetWithTolerance tolerance value arcIntersection solid
  | nearZero value = solid
  | otherwise =
      fromRight emptySolid $
        solidFromAcquireWithCatch $
          mkAcquire (offsetShape tolerance value arcIntersection solid) deleteShape


class Offset a where
  -- |
  --
  -- > offset amount solid
  -- > offset amount 1 solid -- same
  -- > offset amount 0 solid -- sharp corners
  offset :: Double -> a

-- | rounded corners by default
instance {-# INCOHERENT #-} (a ~ Solid, a ~ a') => Offset (a -> a') where
  offset amount solid = offsetWithTolerance 1e-6 amount 1 solid

-- |
--
-- > offset amount 0 -- sharp
-- > offset amount 1 -- rounded
instance (b ~ CBool, a ~ Solid, a ~ a') => Offset (b -> a -> a') where
  offset = offsetWithTolerance 1e-6

tryOffsetWithTolerance :: Double -> Double -> CBool -> Solid -> Either WaterfallError Solid
tryOffsetWithTolerance tolerance value arcIntersection solid
  | nearZero value = Right solid
  | otherwise =
      solidFromAcquireWithCatch $
        mkAcquire (offsetShape tolerance value arcIntersection solid) deleteShape

tryOffset :: Double -> CBool -> Solid -> Either WaterfallError Solid
tryOffset = tryOffsetWithTolerance 1e-6
