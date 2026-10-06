{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Eta reduce" -}

-- | 'offset' grows (positive amount) or shrinks (negative amount) 'Solid', 'Shape', 'Path' and 'Path2D'
module Rapids.Offset where

import Data.Coerce (coerce)
import Foreign (Ptr)
import Foreign.C (CDouble (..), CInt (..))
import Foreign.C.Types (CBool, CDouble)
import Foreign.Marshal.Array (withArray)
import InlineOCCT ( occtContext, ownSolid )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified OpenCascade.TopoDS as TopoDS
import Rapids.Path.Offset (offsetPath, offsetPath2D, offsetShape)
import Rapids.Color ( propagateSolidColors )
import Rapids.Reexports.Lens ( (&) )
import Rapids.Reexports.Waterfall
    ( Path2D, Path, nearZero, Solid, Shape )
import Data.Maybe ( fromMaybe )

C.context occtContext
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<BRepBuilderAPI_MakeSolid.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<BRepOffset.hxx>"
Cpp.include "<BRepOffsetAPI_MakeOffsetShape.hxx>"
Cpp.include "<BRepOffsetAPI_MakeThickSolid.hxx>"
Cpp.include "<GProp_GProps.hxx>"
Cpp.include "<GeomAbs_JoinType.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopTools_IndexedMapOfShape.hxx>"
Cpp.include "<TopTools_ListOfShape.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"

class Offset a where
  -- | @join@ is optional (default 0) and selects how corners are treated
  -- (see <https://occt3d.com/dev/doc/refman/html/_geom_abs___join_type_8hxx.html GeomAbs_JoinType>):
  --
  -- > offset amount <join> <openfaces> solid|shape|path|path2d
  --
  -- @openfaces :: [CInt]@ contains 1-based indices of faces (for solid) to
  -- remove from each solid component. Currently ignored for shape|path|path2d.
  --
  -- > offset amount solid   -- round corners
  -- > offset amount 0 solid -- round corners (Arc)
  -- > offset amount 1 solid -- Tangent
  -- > offset amount 2 solid -- sharp corners (Intersection)
  --
  -- > offset 0.1 [1,2] unitCube -- round box without top and bottom
  -- > offset 0.1 2 [1,2] unitCube -- sharp box without top and bottom
  offset :: Double -> a

-- | the default is @join@ 0, round corners
instance {-# OVERLAPPABLE #-} (OffsetJoin a, a ~ a') => Offset (a -> a') where
  offset amount solid = offsetJoin (coerce amount) 0 [] solid

-- | @offset amount join solid|shape|path|path2d@
instance {-# INCOHERENT #-} (OffsetJoin a, b ~ CInt, a ~ a') => Offset (b -> a -> a') where
  offset amount join = offsetJoin (coerce amount) join []

-- | @offset amount openfaces solid|shape|path|path2d@
instance {-# INCOHERENT #-} (OffsetJoin a, b ~ CInt, a ~ a') => Offset ([b] -> a -> a') where
  offset amount openfaces = offsetJoin (coerce amount) 0 openfaces

-- | @offset amount join openfaces solid|shape|path|path2d@
instance {-# INCOHERENT #-} (OffsetJoin a, b ~ CInt, c ~ CInt, a ~ a') => Offset (b -> [c] -> a -> a') where
  offset amount join openfaces = offsetJoin (coerce amount) join openfaces

-- | @offsetJoin amount join openfaces@, used to define 'offset'. @join@ is 0 (Arc), 1 (Tangent) or 2 (Intersection)
class OffsetJoin a where offsetJoin :: CDouble -> CInt -> [CInt] -> a -> a

instance OffsetJoin Shape where offsetJoin amount join _ = offsetShape amount join

instance OffsetJoin Path where offsetJoin amount join _ = offsetPath amount join

instance OffsetJoin Path2D where offsetJoin amount join _ = offsetPath2D amount join

-- | Offset a 'Solid' while leaving the listed 1-based face indices open.
-- The indices use OpenCascade's face ordering and apply separately to each
-- solid when the input contains multiple solid components.
instance (join ~ CInt, face ~ CInt) => OffsetJoin Solid where
  offsetJoin amount join openfaces = offsetSolidWithOpenFaces (coerce amount) join openfaces

-- | 1e-6 tolerance, see 'offsetSolidWithTolerance'
offsetSolid :: CDouble -> CInt -> Solid -> Solid
offsetSolid = offsetSolidWithTolerance 1e-6

-- | @offsetSolidWithTolerance tolerance amount join solid@ offsets every solid component and retains a compound when there are many.
offsetSolidWithTolerance :: CDouble -> CDouble -> CInt -> Solid -> Solid
offsetSolidWithTolerance tolerance value join = offsetSolid' tolerance value join Nothing

-- | @offsetSolidWithOpenFaces amount join openfaces solid@ leaves the selected
-- faces open while building a thick solid.
offsetSolidWithOpenFaces :: CDouble -> CInt -> [CInt] -> Solid -> Solid
offsetSolidWithOpenFaces value join openfaces = offsetSolid' 1e-6 value join (Just openfaces)

offsetSolid' :: CDouble -> CDouble -> CInt -> Maybe [CInt] -> Solid -> Solid
offsetSolid' tolerance value join openfaces solid
  | coerce nearZero value = solid
  | otherwise =
      let faceIndices = fromMaybe [] openfaces :: [CInt]
          hasOpenFaces = maybe 0 (const 1) openfaces :: CInt
          faceCount = fromIntegral (length faceIndices) :: CInt
       in withArray faceIndices (\faceIndexArray -> [Cpp.block| TopoDS_Shape* {
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
      TopoDS_Shape offsetShape;
      if ($(int hasOpenFaces) != 0) {
        TopTools_IndexedMapOfShape faces;
        TopExp::MapShapes(solids.Current(), TopAbs_FACE, faces);
        TopTools_ListOfShape closingFaces;
        const int* indices = $(int* faceIndexArray);
        for (int i = 0; i < $(int faceCount); ++i) {
          const int index = indices[i];
          if (index > 0 && index <= faces.Extent()) {
            closingFaces.Append(faces(index));
          }
        }
        BRepOffsetAPI_MakeThickSolid thickSolid;
        thickSolid.MakeThickSolidByJoin(
          solids.Current(),
          closingFaces,
          $(double value),
          $(double tolerance),
          BRepOffset_Skin,
          Standard_False,
          Standard_False,
          static_cast<GeomAbs_JoinType>($(int join)),
          Standard_False);
        offsetShape = thickSolid.Shape();
      } else {
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
        offsetShape = offset.Shape();
      }

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
  }|])
          & ownSolid
          & propagateSolidColors solid
