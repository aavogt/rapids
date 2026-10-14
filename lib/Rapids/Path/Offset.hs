-- | Planar offsets of planar wires and faces.
module Rapids.Path.Offset
  ( offsetPath,
    offsetPathSide,
    offsetPathSides,
    offsetShape,
    offsetPath2D,
    offsetPathArc,
    offsetPath2DArc,
    offsetShapeArc,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Acquire (mkAcquire)
import Data.Coerce (coerce)
import Data.Function
import Foreign (Ptr)
import Foreign.Marshal.Array (withArray)
import Foreign.C.Types (CDouble, CInt, CBool)
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
import Rapids.Color
import Waterfall.TwoD.Internal.Shape (Shape (..))

C.context occtContext
Cpp.include "<BRepOffsetAPI_MakePipeShell.hxx>"
Cpp.include "<BRepBuilderAPI_TransitionMode.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<GProp_GProps.hxx>"
Cpp.include "<Law_Interpol.hxx>"
Cpp.include "<TColgp_Array1OfPnt2d.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopTools_ListIteratorOfListOfShape.hxx>"
Cpp.include "<Precision.hxx>"
Cpp.include "<gp_Dir.hxx>"
Cpp.include "<gp_Pnt2d.hxx>"
Cpp.include "<BRepOffsetAPI_MakeOffset.hxx>"
Cpp.include "<GeomAbs_JoinType.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<GC_MakeArcOfCircle.hxx>"
Cpp.include "<cmath>"
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
offsetPath :: CDouble -> CInt -> Path -> Path
offsetPath amount join = offsetPathWithOpenResult amount join 0

-- | Offset one side of an open planar wire. The sign of @amount@ selects the
-- side; if OCCT returns a compound, its first wire is used. @join@ selects
-- how corners are treated (see 'offsetPath').
offsetPathSide :: CDouble -> CInt -> Path -> Path
offsetPathSide amount join = offsetPathWithOpenResult amount join 1

-- | Offset one side of an open planar wire using thicknesses sampled evenly
-- along its length. At least two values define an interpolated law from the
-- start to the end of the path. A single value is equivalent to
-- 'offsetPathSide'; an empty list produces an empty path.
offsetPathSides :: [CDouble] -> CInt -> Path -> Path
offsetPathSides [] _ _ = mempty
offsetPathSides [amount] join input = offsetPathSide amount join input
offsetPathSides amounts join input =
  propagateSideOffsetColors input $ ownPath $ withArray amounts $ \amountArray ->
    [Cpp.block| TopoDS_Wire* {
      TopoDS_Wire* spine = $path:input;
      const double* thicknesses = $(double *amountArray);
      const int count = $(int sampleCount);
      if (spine == nullptr || spine->IsNull() || count < 2) {
        return nullptr;
      }

      try {
        TopoDS_Vertex spineStart, spineEnd;
        TopExp::Vertices(*spine, spineStart, spineEnd);
        if (spineStart.IsNull() || spineEnd.IsNull()) {
          return nullptr;
        }

        GProp_GProps properties;
        BRepGProp::LinearProperties(*spine, properties);
        const double spineLength = properties.Mass();
        if (!(spineLength > Precision::Confusion())) {
          return nullptr;
        }

        TColgp_Array1OfPnt2d points(1, count);
        for (int i = 0; i < count; ++i) {
          const double parameter = spineLength * i / (count - 1);
          points.SetValue(i + 1, gp_Pnt2d(parameter, thicknesses[i]));
        }

        Handle(Law_Interpol) law = new Law_Interpol();
        law->Set(points, Standard_False);

        const TopoDS_Edge profileEdge = BRepBuilderAPI_MakeEdge(
            gp_Pnt(0.0, 0.0, 0.0), gp_Pnt(0.0, 1.0, 0.0)).Edge();
        TopoDS_Vertex profileStart, profileEnd;
        TopExp::Vertices(profileEdge, profileStart, profileEnd);
        if (profileEnd.IsNull()) {
          return nullptr;
        }

        BRepBuilderAPI_MakeWire profileBuilder(profileEdge);
        if (!profileBuilder.IsDone()) {
          return nullptr;
        }
        const TopoDS_Wire profile = profileBuilder.Wire();

        BRepOffsetAPI_MakePipeShell pipe(*spine);
        pipe.SetMode(gp_Dir(0.0, 0.0, 1.0));
        pipe.SetTransitionMode(BRepBuilderAPI_RoundCorner);
        pipe.SetLaw(profile, law, spineStart, Standard_False, Standard_False);
        pipe.Build();
        if (!pipe.IsDone()) {
          return nullptr;
        }

        const TopTools_ListOfShape& generated = pipe.Generated(profileEnd);
        BRepBuilderAPI_MakeWire wire;
        for (TopTools_ListIteratorOfListOfShape it(generated); it.More(); it.Next()) {
          if (it.Value().ShapeType() == TopAbs_EDGE) {
            wire.Add(TopoDS::Edge(it.Value()));
          }
        }
        if (!wire.IsDone()) {
          return nullptr;
        }
        return new TopoDS_Wire(wire.Wire());
      } catch (...) {
        return nullptr;
      }
    } |]
  where
    sampleCount = fromIntegral (length amounts) :: CInt

propagateSideOffsetColors :: Path -> Path -> Path
propagateSideOffsetColors input output = case output of
  Path (ComplexRawPath _) -> propagatePathEdgeColors leftColor input output
  _ -> output

offsetPathWithOpenResult :: CDouble -> CInt -> CBool -> Path -> Path
offsetPathWithOpenResult amount join openResult input = propagatePathEdgeColors leftColor input $
  [Cpp.block| TopoDS_Wire* {
    TopoDS_Wire* spine = $path:input;
    if (spine == nullptr || spine->IsNull()) {
      return nullptr;
    }

    try {
      const bool isOpenResult = $(bool openResult);
      if (!isOpenResult) {
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

          const double radius = std::fabs($(double amount));
          if (radius <= 1e-12) {
            return new TopoDS_Wire(*spine);
          }

          gp_Vec travel = tangent;
          travel.Normalize();
          gp_Vec planeNormal(0.0, 0.0, 1.0);
          if (tangent.Crossed(planeNormal).SquareMagnitude() <= 1e-24) {
            planeNormal = gp_Vec(1.0, 0.0, 0.0);
          }
          gp_Vec offsetVector = planeNormal.Crossed(tangent);
          offsetVector.Normalize();
          offsetVector *= radius;

          gp_Pnt startLeft = start.Translated(offsetVector);
          gp_Pnt endLeft = end.Translated(offsetVector);
          gp_Pnt startRight = start.Translated(-offsetVector);
          gp_Pnt endRight = end.Translated(-offsetVector);
          gp_Pnt endCapMid = end.Translated(travel * radius);
          gp_Pnt startCapMid = start.Translated(-travel * radius);
          BRepBuilderAPI_MakeWire wire;
          wire.Add(BRepBuilderAPI_MakeEdge(startLeft, endLeft).Edge());
          wire.Add(BRepBuilderAPI_MakeEdge(
              GC_MakeArcOfCircle(endLeft, endCapMid, endRight).Value()).Edge());
          wire.Add(BRepBuilderAPI_MakeEdge(endRight, startRight).Edge());
          wire.Add(BRepBuilderAPI_MakeEdge(
              GC_MakeArcOfCircle(startRight, startCapMid, startLeft).Value()).Edge());
          if (!wire.IsDone()) {
            return nullptr;
          }
          return new TopoDS_Wire(wire.Wire());
        }
      }

      BRepOffsetAPI_MakeOffset offset;
      offset.Init(
          static_cast<GeomAbs_JoinType>($(int join)),
          isOpenResult ? Standard_True : Standard_False);
      offset.AddWire(*spine);
      offset.Perform($(double amount));
      if (!offset.IsDone()) {
        return nullptr;
      }

      const TopoDS_Shape& result = offset.Shape();
      if (result.IsNull()) {
        return nullptr;
      }
      if (result.ShapeType() == TopAbs_WIRE) {
        return new TopoDS_Wire(TopoDS::Wire(result));
      }
      if (result.ShapeType() != TopAbs_COMPOUND) {
        return nullptr;
      }
      // Path represents a single wire; use the first wire from compounds.
      TopExp_Explorer wireExplorer(result, TopAbs_WIRE);
      if (!wireExplorer.More()) {
        return nullptr;
      }
      return new TopoDS_Wire(TopoDS::Wire(wireExplorer.Current()));
    } catch (...) {
      return nullptr;
    }
  } |]
    & ownPath

-- | @offsetShape amount join shape@ offsets a planar @TopoDS_Face@, see 'offsetPath' for @join@
offsetShape :: CDouble -> CInt -> Shape -> Shape
offsetShape amount join input = propagateShapeColorsToShape input $ ownShape
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

-- | @offsetPath2D amount join path2d@, see 'offsetPath'
offsetPath2D :: CDouble -> CInt -> Path2D -> Path2D
offsetPath2D = coerce offsetPath

-- | @offsetPathArc amount path = offsetPath amount 0 path@
offsetPathArc :: CDouble -> Path -> Path
offsetPathArc = flip offsetPath 0

-- | @offsetPath2DArc amount path2d = offsetPath2D amount 0 path2d@
offsetPath2DArc :: CDouble -> Path2D -> Path2D
offsetPath2DArc = flip offsetPath2D 0

-- | @offsetShapeArc amount shape = offsetShape amount 0 shape@
offsetShapeArc :: CDouble -> Shape -> Shape
offsetShapeArc = flip offsetShape 0
