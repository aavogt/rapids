-- | Offsets of planar wires and faces.
--
-- This module uses 'BRepOffsetAPI_MakeOffset'.  A 'Path' or 'Path2D' is
-- represented by a planar @TopoDS_Wire@, while a 'Shape' is expected to be a
-- planar @TopoDS_Face@.
module Rapids.Path.Offset where

import Control.Monad.IO.Class (liftIO)
import Data.Acquire (Acquire, mkAcquire)
import Data.List (foldl')
import Foreign (Ptr, castPtr, withArray)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Marshal
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified OpenCascade.TopoDS as TopoDS
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.TwoD.Internal.Shape (Shape (..))
import Data.Coerce (coerce)

C.context occtContext
Cpp.include "<BRepOffsetAPI_MakeOffset.hxx>"
Cpp.include "<GeomAbs_JoinType.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Face.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"

-- | Offset a planar @TopoDS_Wire@, preserving its plane.
offsetPath :: CInt -- ^ 0 arc; 1 tangent; 2 intersection
  -> CDouble  -- ^ amount
  -> Path -> Path
offsetPath join' amount input = unsafeFromAcquire $ liftIO $ Path . ComplexRawPath <$>
  [Cpp.block| TopoDS_Wire* {
      TopoDS_Wire* spine = $path:input;
      if (spine == nullptr || spine->IsNull()) {
        return new TopoDS_Wire();
      }

      try {
        BRepOffsetAPI_MakeOffset offset(
            *spine,
            static_cast<GeomAbs_JoinType>($(int join')),
            Standard_False);
        offset.Perform($(double amount));
        if (!offset.IsDone()) {
          return new TopoDS_Wire();
        }

        const TopoDS_Shape& result = offset.Shape();
        if (result.IsNull() || result.ShapeType() != TopAbs_WIRE) {
          return new TopoDS_Wire();
        }
        return new TopoDS_Wire(TopoDS::Wire(result));
      } catch (...) {
        return new TopoDS_Wire();
      }
    } |]

-- | Offset a planar @TopoDS_Face@, optionally adding more boundary wires.
--
-- Each wire is passed to OCCT's 'BRepOffsetAPI_MakeOffset::AddWire' before
-- 'Perform' is called.  This is how holes and islands are added to a
-- face-based offset.
offsetTopoDSFace :: CInt -> Double -> Ptr TopoDS.Face -> [Ptr TopoDS.Wire] -> Acquire (Ptr TopoDS.Face)
offsetTopoDSFace join' amount input wires =
  mkAcquire
    ( liftIO $ withArrayLen wires $ \(fromIntegral -> wireCount) wireArray -> do
        let amount' = realToFrac amount :: CDouble
            input' = castPtr input :: Ptr ()
            wireArray' = castPtr wireArray :: Ptr ()
        castPtr
          <$> [Cpp.block| void* {
            TopoDS_Shape* shape = (TopoDS_Shape*)$(void* input');
            void** addedWires = (void**)$(void* wireArray');
            const int nWires = $(int wireCount);
            if (shape == nullptr || shape->IsNull()) {
              return new TopoDS_Face();
            }

            try {
              BRepOffsetAPI_MakeOffset offset(
                  TopoDS::Face(*shape),
                  static_cast<GeomAbs_JoinType>($(int join')),
                  Standard_False);
              for (int index = 0; index < nWires; ++index) {
                TopoDS_Wire* wire = (TopoDS_Wire*)addedWires[index];
                if (wire != nullptr && !wire->IsNull()) {
                  offset.AddWire(*wire);
                }
              }
              offset.Perform($(double amount'));
              if (!offset.IsDone()) {
                return new TopoDS_Face();
              }

              const TopoDS_Shape& result = offset.Shape();
              if (result.IsNull()) {
                return new TopoDS_Face();
              }

              TopExp_Explorer explorer(result, TopAbs_WIRE);
              if (!explorer.More()) {
                return new TopoDS_Face();
              }
              BRepBuilderAPI_MakeFace faceBuilder(
                  TopoDS::Wire(explorer.Current()));
              if (!faceBuilder.IsDone()) {
                return new TopoDS_Face();
              }
              for (explorer.Next(); explorer.More(); explorer.Next()) {
                faceBuilder.Add(TopoDS::Wire(explorer.Current()));
              }
              if (!faceBuilder.IsDone()) {
                return new TopoDS_Face();
              }
              return new TopoDS_Face(faceBuilder.Face());
            } catch (...) {
              return new TopoDS_Face();
            }
          } |]
    )
    ( \ptr ->
        let ptr' = castPtr ptr :: Ptr ()
         in [Cpp.block| void {
          delete (TopoDS_Face*)$(void* ptr');
        } |]
    )

-- | Offset a planar 'Path2D', preserving the @z = 0@ plane.
offsetPath2D :: CInt -> CDouble -> Path2D -> Path2D
offsetPath2D = coerce offsetPath

-- | Offset a planar 'Shape' whose underlying shape is a @TopoDS_Face@.
offsetFace :: CInt -> Double -> Shape -> Shape
offsetFace join amount (Shape face) =
  Shape . unsafeFromAcquire $ do
    result <- offsetTopoDSFace join amount (castPtr face) []
    pure (castPtr result)

-- | Run a face offset after all desired wires have been added.
performOffset :: Double -> CInt -> Shape -> [Path2D] -> Shape
performOffset amount join (Shape face) paths =
  Shape . unsafeFromAcquire $ do
    let wires = [wire | Path2D (ComplexRawPath wire) <- reverse paths]
    result <- offsetTopoDSFace join amount (castPtr face) wires
    pure (castPtr result)

-- | Arc-joined planar wire offset.
offsetPathArc :: CDouble -> Path -> Path
offsetPathArc = offsetPath 0

-- | Arc-joined planar 2D wire offset.
offsetPath2DArc :: CDouble -> Path2D -> Path2D
offsetPath2DArc = offsetPath2D 0

-- | Arc-joined planar face offset.
offsetFaceArc :: Double -> Shape -> Shape
offsetFaceArc = offsetFace 0
