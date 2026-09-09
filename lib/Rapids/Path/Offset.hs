-- | Planar offsets of planar wires and faces.
module Rapids.Path.Offset where

import Control.Monad.IO.Class (liftIO)
import Data.Acquire (mkAcquire)
import Data.Coerce (coerce)
import Data.Function
import Foreign (Ptr)
import Foreign.C.Types (CDouble, CInt)
import InlineOCCT
  ( c_deleteTopoDSShape,
    c_deleteTopoDSWire,
    occtContext,
    ownPath,
    ownShape,
  )
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
Cpp.include "<BRepAdaptor_Curve.hxx>"
Cpp.include "<BRepBuilderAPI_MakeEdge.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Face.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"

-- | Offset a planar @TopoDS_Wire@, preserving its plane.
offsetPath ::
  -- | amount
  CDouble ->
  -- | 0 arc; 1 tangent; 2 intersection
  CInt ->
  Path ->
  Path
offsetPath amount join input =
  [Cpp.block| TopoDS_Wire* {
    TopoDS_Wire* spine = $path:input;
    if (spine == nullptr || spine->IsNull()) {
      return nullptr;
    }

    try {
      TopExp_Explorer edgeExplorer(*spine, TopAbs_EDGE);
      if (!edgeExplorer.More()) {
        return nullptr;
      }
      TopoDS_Edge edge = TopoDS::Edge(edgeExplorer.Current());
      edgeExplorer.Next();
      if (!edgeExplorer.More()) {
        BRepAdaptor_Curve curve(edge);
        gp_Pnt start = curve.Value(curve.FirstParameter());
        gp_Pnt end = curve.Value(curve.LastParameter());
        gp_Vec tangent(start, end);
        if (tangent.SquareMagnitude() <= 1e-24) {
          return nullptr;
        }

        gp_Vec planeNormal(0.0, 0.0, 1.0);
        if (tangent.Crossed(planeNormal).SquareMagnitude() <= 1e-24) {
          planeNormal = gp_Vec(1.0, 0.0, 0.0);
        }
        gp_Vec side = planeNormal.Crossed(tangent);
        side.Normalize();
        side *= $(double amount);

        gp_Pnt startLeft = start.Translated(side);
        gp_Pnt endLeft = end.Translated(side);
        gp_Pnt startRight = start.Translated(-side);
        gp_Pnt endRight = end.Translated(-side);
        BRepBuilderAPI_MakeWire wire;
        wire.Add(BRepBuilderAPI_MakeEdge(startLeft, endLeft).Edge());
        wire.Add(BRepBuilderAPI_MakeEdge(endLeft, endRight).Edge());
        wire.Add(BRepBuilderAPI_MakeEdge(endRight, startRight).Edge());
        wire.Add(BRepBuilderAPI_MakeEdge(startRight, startLeft).Edge());
        if (!wire.IsDone()) {
          return nullptr;
        }
        return new TopoDS_Wire(wire.Wire());
      }

      BRepOffsetAPI_MakeOffset offset(
          *spine,
          static_cast<GeomAbs_JoinType>($(int join)),
          Standard_False);
      offset.Perform($(double amount));
      if (!offset.IsDone()) {
        return nullptr;
      }

      const TopoDS_Shape& result = offset.Shape();
      if (result.IsNull() || result.ShapeType() != TopAbs_WIRE) {
        return nullptr;
      }
      return new TopoDS_Wire(TopoDS::Wire(result));
    } catch (...) {
      return nullptr;
    }
  } |]
    & ownPath

-- | Offset a planar @TopoDS_Face@
offsetShape :: CDouble -> CInt -> Shape -> Shape
offsetShape amount join input =
  ownShape
    [Cpp.block| TopoDS_Shape* {
      TopoDS_Shape* shape = $shape:input;
      if (shape == nullptr || shape->IsNull()) {
        return new TopoDS_Face();
      }

      try {
        BRepOffsetAPI_MakeOffset offset(
            TopoDS::Face(*shape),
            static_cast<GeomAbs_JoinType>($(int join)),
            Standard_False);
        offset.Perform($(double amount));
        if (!offset.IsDone()) {
          return new TopoDS_Face();
        }

        return new TopoDS_Shape(offset.Shape());
      } catch (...) {
        return new TopoDS_Face();
      }
    } |]

-- | @offsetPath join amount path2d@
offsetPath2D :: CDouble -> CInt -> Path2D -> Path2D
offsetPath2D = coerce offsetPath

-- | @offsetPathArc amount path = offsetPath 0 amount path@
offsetPathArc :: CDouble -> Path -> Path
offsetPathArc = flip offsetPath 0

-- | @offsetPath2DArc amount path2d = offsetPath2D 0 amount path2d@
offsetPath2DArc :: CDouble -> Path2D -> Path2D
offsetPath2DArc = flip offsetPath2D 0

-- | @offsetShapeArc amount shape = offsetShape 0 amount shape@
offsetShapeArc :: CDouble -> Shape -> Shape
offsetShapeArc = flip offsetShape 0
