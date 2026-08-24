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
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified OpenCascade.TopoDS as TopoDS
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.TwoD.Internal.Shape (Shape (..))

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
--
-- The returned pointer owns a new @TopoDS_Wire@ through the 'Acquire'
-- finalizer.  The input wire must remain alive while the acquire action is
-- used.
offsetTopoDSWire :: CInt -> Double -> Ptr TopoDS.Wire -> Acquire (Ptr TopoDS.Wire)
offsetTopoDSWire join' amount input =
  mkAcquire
    ( liftIO $ do
        let amount' = realToFrac amount :: CDouble
            input' = castPtr input :: Ptr ()
        castPtr
          <$> [Cpp.block| void* {
            TopoDS_Wire* spine = (TopoDS_Wire*)$(void* input');
            if (spine == nullptr || spine->IsNull()) {
              return new TopoDS_Wire();
            }

            try {
              BRepOffsetAPI_MakeOffset offset(
                  *spine,
                  static_cast<GeomAbs_JoinType>($(int join')),
                  Standard_False);
              offset.Perform($(double amount'));
              if (!offset.IsDone()) {
                return new TopoDS_Wire();
              }

              const TopoDS_Shape& result = offset.Shape();
              if (result.IsNull() || result.ShapeType() != TopAbs_WIRE) {
                return new TopoDS_Wire();
              }
              return new TopoDS_Wire(TopoDS::Wire(result));
            } catch (Standard_Failure const&) {
              return new TopoDS_Wire();
            } catch (...) {
              return new TopoDS_Wire();
            }
          } |]
    )
    ( \ptr ->
        let ptr' = castPtr ptr :: Ptr ()
         in [Cpp.block| void {
          delete (TopoDS_Wire*)$(void* ptr');
        } |]
    )

-- | Offset a planar @TopoDS_Face@, optionally adding more boundary wires.
--
-- Each wire is passed to OCCT's 'BRepOffsetAPI_MakeOffset::AddWire' before
-- 'Perform' is called.  This is how holes and islands are added to a
-- face-based offset.
offsetTopoDSFace :: CInt -> Double -> Ptr TopoDS.Face -> [Ptr TopoDS.Wire] -> Acquire (Ptr TopoDS.Face)
offsetTopoDSFace join' amount input wires =
  mkAcquire
    ( liftIO $ withArray wires $ \wireArray -> do
        let amount' = realToFrac amount :: CDouble
            wireCount = fromIntegral (length wires) :: CInt
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
            } catch (Standard_Failure const&) {
              return new TopoDS_Face();
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

-- | Offset a 'Path' in its existing plane (normally the @z = 0@ plane).
--
-- A path without a wire, such as 'EmptyRawPath' or a single point, cannot be
-- offset and is returned unchanged.
offsetPath :: CInt -> Double -> Path -> Path
offsetPath join amount path@(Path (ComplexRawPath wire)) =
  Path . ComplexRawPath $ unsafeFromAcquire (offsetTopoDSWire join amount wire)
offsetPath _ _ path = path

-- | Offset a planar 'Path2D', preserving the @z = 0@ plane.
offsetPath2D :: CInt -> Double -> Path2D -> Path2D
offsetPath2D join amount (Path2D (ComplexRawPath wire)) =
  Path2D . ComplexRawPath $ unsafeFromAcquire (offsetTopoDSWire join amount wire)
offsetPath2D _ _ path = path

-- | Offset a planar 'Shape' whose underlying shape is a @TopoDS_Face@.
offsetFace :: CInt -> Double -> Shape -> Shape
offsetFace join amount (Shape face) =
  Shape . unsafeFromAcquire $ do
    result <- offsetTopoDSFace join amount (castPtr face) []
    pure (castPtr result)

-- | A face offset under construction.  Wires added with 'addWire' are passed
-- to OCCT as holes or islands when 'performOffset' is called.
data FaceOffset = FaceOffset
  { faceOffsetJoin :: CInt ,
    faceOffsetFace :: Shape,
    faceOffsetWires :: [Path2D]
  }

-- | Start a face offset that may receive additional boundary wires.
newFaceOffset :: CInt -> Shape -> FaceOffset
newFaceOffset join face = FaceOffset join face []

-- | Add a hole or island to a face offset.
--
-- Non-wire paths are ignored.  The wires are retained in insertion order and
-- each is supplied to OCCT through 'BRepOffsetAPI_MakeOffset::AddWire'.
addWire path state = state {faceOffsetWires = path : faceOffsetWires state}

-- | Run a face offset after all desired wires have been added.
performOffset :: Double -> FaceOffset -> Shape
performOffset amount (FaceOffset join (Shape face) paths) =
  Shape . unsafeFromAcquire $ do
    let wires = [wire | Path2D (ComplexRawPath wire) <- reverse paths]
    result <- offsetTopoDSFace join amount (castPtr face) wires
    pure (castPtr result)

-- | Offset a face and add all of its extra boundary wires in one expression.
offsetFaceWithWires :: CInt -> Double -> Shape -> [Path2D] -> Shape
offsetFaceWithWires join amount face paths =
  performOffset amount (foldl' (flip addWire) (newFaceOffset join face) paths)

-- | Offset a planar 'Path', selecting the corner join mode.
offsetWire :: CInt -> Double -> Path -> Path
offsetWire = offsetPath

-- | Offset a planar 2D wire, selecting the corner join mode.
offsetWire2D :: CInt -> Double -> Path2D -> Path2D
offsetWire2D = offsetPath2D

-- | Arc-joined planar wire offset.
offsetWireArc :: Double -> Path -> Path
offsetWireArc = offsetPath 0

-- | Arc-joined planar 2D wire offset.
offsetWire2DArc :: Double -> Path2D -> Path2D
offsetWire2DArc = offsetPath2D 0

-- | Arc-joined planar face offset.
offsetFaceArc :: Double -> Shape -> Shape
offsetFaceArc = offsetFace 0
