{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Eta reduce" -}

module Rapids.Offset where

import Control.Monad.IO.Class (liftIO)
import Data.Acquire (mkAcquire)
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Foreign (Ptr)
import Foreign.C (CDouble (..), CInt (..))
import Foreign.C.Types (CBool, CDouble)
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Rapids.Path.Offset (offsetPath, offsetPath2D, offsetShape)
import Rapids.Reexports
  ( Path,
    Path2D,
    Shape,
    Solid,
    emptySolid,
    nearZero,
    (&),
  )
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Waterfall.Internal.Solid
  ( acquireSolid,
    emptySolid,
    solidFromAcquireWithCatch,
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
offsetSolidWithTolerance :: CDouble -> CDouble -> CInt -> Solid -> Solid
offsetSolidWithTolerance tolerance value join solid
  | coerce nearZero value = solid
  | otherwise =
      [Cpp.block| TopoDS_Shape* {
    TopoDS_Shape* result = new TopoDS_Shape();
    TopoDS_Shape* input = $solid:solid;
    if (input == nullptr || input->IsNull()) {
      return result;
    }

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
        $(double value),
        $(double tolerance),
        BRepOffset_Skin,
        Standard_False,
        Standard_False,
        static_cast<GeomAbs_JoinType>($(int join)),
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
  return result;
  }|]
        & ownSolid

offsetSolid :: CDouble -> CInt -> Solid -> Solid
offsetSolid = offsetSolidWithTolerance 1e-6

class Offset a where
  -- |
  -- > offset amount <join> solid|shape|path|path2d
  --
  -- > offset amount solid   -- round corners
  -- > offset amount 1 solid -- round corners
  -- > offset amount 0 solid -- sharp corners
  offset :: Double -> a

-- | sharp corners by default
instance {-# INCOHERENT #-} (OffsetJoin a, a ~ a') => Offset (a -> a') where
  offset amount solid = offsetJoin (coerce amount) 1 solid

-- |
--
-- > offset amount 0 -- sharp
-- > offset amount 1 -- rounded
instance (OffsetJoin a, b ~ CInt, a ~ a') => Offset (b -> a -> a') where offset amount = offsetJoin (coerce amount)

-- | used to define 'offset'
class OffsetJoin a where offsetJoin :: CDouble -> CInt -> a -> a

instance OffsetJoin Solid where offsetJoin = offsetSolid

instance OffsetJoin Shape where offsetJoin = offsetShape

instance OffsetJoin Path where offsetJoin = offsetPath

instance OffsetJoin Path2D where offsetJoin = offsetPath2D
