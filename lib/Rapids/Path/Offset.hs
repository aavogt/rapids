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
Cpp.include "<Law_Constant.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<BRepTools_WireExplorer.hxx>"
Cpp.include "<TopoDS_Vertex.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<TopTools_ListIteratorOfListOfShape.hxx>"
Cpp.include "<Precision.hxx>"
Cpp.include "<gp_Dir.hxx>"
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

-- | Return a closed one-sided offset of an open planar wire. The sign of
-- @amount@ selects the side; @join@ controls corners and end caps. Arc joins
-- make rounded caps; other join modes make straight caps.
offsetPathSide :: CDouble -> CInt -> Path -> Path
offsetPathSide amount _ _ | amount == 0 = mempty
offsetPathSide amount join input =
  closeSideOffset join input $ sideOffsetCurve [amount] join input

-- | Return a closed one-sided offset with a constant thickness on each spine
-- edge. A single value, or repeated equal values, applies that thickness to
-- the whole spine. Otherwise the list length must match the edge count, and
-- thickness changes occur at edge joins. An empty list returns an empty path.
-- @join@ controls corners and end caps as in 'offsetPathSide'.
offsetPathSides :: [CDouble] -> CInt -> Path -> Path
offsetPathSides [] _ _ = mempty
offsetPathSides [amount] join input = offsetPathSide amount join input
offsetPathSides amounts@(first:_) join input
  | all (== first) amounts = offsetPathSide first join input
offsetPathSides amounts join input = closeSideOffset join input $ sideOffsetCurve amounts join input

sideOffsetCurve :: [CDouble] -> CInt -> Path -> Path
sideOffsetCurve amounts join input = propagateSideOffsetColors input $ ownPath $ withArray amounts $ \amountArray ->
    [Cpp.block| TopoDS_Wire* {
      TopoDS_Wire* spine = $path:input;
      const double* thicknesses = $(double *amountArray);
      const int count = $(int sampleCount);
      const int joinType = $(int join);
      if (spine == nullptr || spine->IsNull() || count < 1) {
        return nullptr;
      }

      try {
        int edgeCount = 0;
        for (BRepTools_WireExplorer explorer(*spine); explorer.More(); explorer.Next()) {
          ++edgeCount;
        }
        if (count != 1 && count != edgeCount) {
          return nullptr;
        }

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

        BRepBuilderAPI_MakeWire result;
        gp_Pnt previousOffsetEnd;
        int edgeIndex = 0;
        for (BRepTools_WireExplorer spineExplorer(*spine); spineExplorer.More(); spineExplorer.Next()) {
          const TopoDS_Edge spineEdge = TopoDS::Edge(spineExplorer.Current());
          TopoDS_Vertex spineStart, spineEnd;
          TopExp::Vertices(spineEdge, spineStart, spineEnd);
          if (spineStart.IsNull() || spineEnd.IsNull()) {
            return nullptr;
          }

          BRepAdaptor_Curve spineCurve(spineEdge);
          Handle(Law_Constant) law = new Law_Constant();
          law->Set(thicknesses[count == 1 ? 0 : edgeIndex],
                   spineCurve.FirstParameter(), spineCurve.LastParameter());

          BRepBuilderAPI_MakeWire edgeSpineBuilder(spineEdge);
          if (!edgeSpineBuilder.IsDone()) {
            return nullptr;
          }
          BRepOffsetAPI_MakePipeShell pipe(edgeSpineBuilder.Wire());
          pipe.SetMode(gp_Dir(0.0, 0.0, 1.0));
          pipe.SetTransitionMode(BRepBuilderAPI_RoundCorner);
          pipe.SetLaw(profile, law, spineStart, Standard_False, Standard_False);
          pipe.Build();
          if (!pipe.IsDone()) {
            return nullptr;
          }

          const TopTools_ListOfShape& generated = pipe.Generated(profileEnd);
          BRepBuilderAPI_MakeWire segmentBuilder;
          for (TopTools_ListIteratorOfListOfShape it(generated); it.More(); it.Next()) {
            if (it.Value().ShapeType() == TopAbs_EDGE) {
              segmentBuilder.Add(TopoDS::Edge(it.Value()));
            }
          }
          if (!segmentBuilder.IsDone()) {
            return nullptr;
          }
          TopoDS_Wire segment = segmentBuilder.Wire();
          TopoDS_Vertex offsetStart, offsetEnd;
          TopExp::Vertices(segment, offsetStart, offsetEnd);
          if (offsetStart.IsNull() || offsetEnd.IsNull()) {
            return nullptr;
          }
          const gp_Pnt spineStartPoint = BRep_Tool::Pnt(spineStart);
          const gp_Pnt spineEndPoint = BRep_Tool::Pnt(spineEnd);
          gp_Pnt offsetStartPoint = BRep_Tool::Pnt(offsetStart);
          gp_Pnt offsetEndPoint = BRep_Tool::Pnt(offsetEnd);
          const double forwardDistance = spineStartPoint.Distance(offsetStartPoint)
              + spineEndPoint.Distance(offsetEndPoint);
          const double reverseDistance = spineStartPoint.Distance(offsetEndPoint)
              + spineEndPoint.Distance(offsetStartPoint);
          if (reverseDistance < forwardDistance) {
            segment.Reverse();
            TopoDS_Vertex vertex = offsetStart;
            offsetStart = offsetEnd;
            offsetEnd = vertex;
            offsetStartPoint = BRep_Tool::Pnt(offsetStart);
            offsetEndPoint = BRep_Tool::Pnt(offsetEnd);
          }

          if (edgeIndex > 0 && previousOffsetEnd.Distance(offsetStartPoint) > Precision::Confusion()) {
            TopoDS_Edge connector;
            if (joinType == GeomAbs_Arc) {
              const gp_Pnt corner = spineStartPoint;
              gp_Vec firstRadius(corner, previousOffsetEnd);
              gp_Vec secondRadius(corner, offsetStartPoint);
              const double firstLength = firstRadius.Magnitude();
              const double secondLength = secondRadius.Magnitude();
              if (firstLength > Precision::Confusion() && secondLength > Precision::Confusion()) {
                firstRadius.Normalize();
                secondRadius.Normalize();
                gp_Vec bisector = firstRadius + secondRadius;
                if (bisector.SquareMagnitude() > 1e-24) {
                  bisector.Normalize();
                  bisector *= (firstLength + secondLength) * 0.5;
                  const gp_Pnt middle = corner.Translated(bisector);
                  GC_MakeArcOfCircle arc(previousOffsetEnd, middle, offsetStartPoint);
                  if (arc.IsDone()) {
                    connector = BRepBuilderAPI_MakeEdge(arc.Value()).Edge();
                  }
                }
              }
            }
            if (connector.IsNull()) {
              connector = BRepBuilderAPI_MakeEdge(previousOffsetEnd, offsetStartPoint).Edge();
            }
            if (connector.IsNull()) {
              return nullptr;
            }
            result.Add(connector);
          }
          for (BRepTools_WireExplorer segmentExplorer(segment); segmentExplorer.More(); segmentExplorer.Next()) {
            result.Add(TopoDS::Edge(segmentExplorer.Current()));
          }
          if (!result.IsDone()) {
            return nullptr;
          }
          previousOffsetEnd = offsetEndPoint;
          ++edgeIndex;
        }
        return new TopoDS_Wire(result.Wire());
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

closeSideOffset :: CInt -> Path -> Path -> Path
closeSideOffset join input sideOffset = propagateSideOffsetColors input $ ownPath
  [Cpp.block| TopoDS_Wire* {
    TopoDS_Wire* spine = $path:input;
    TopoDS_Wire* offset = $path:sideOffset;
    const int joinType = $(int join);
    if (spine == nullptr || spine->IsNull() || offset == nullptr || offset->IsNull()) {
      return nullptr;
    }

    try {
      TopoDS_Vertex spineStart, spineEnd, offsetStart, offsetEnd;
      TopExp::Vertices(*spine, spineStart, spineEnd);
      TopExp::Vertices(*offset, offsetStart, offsetEnd);
      if (spineStart.IsNull() || spineEnd.IsNull() || offsetStart.IsNull() || offsetEnd.IsNull()) {
        return nullptr;
      }

      const gp_Pnt spineStartPoint = BRep_Tool::Pnt(spineStart);
      const gp_Pnt spineEndPoint = BRep_Tool::Pnt(spineEnd);
      gp_Pnt offsetStartPoint = BRep_Tool::Pnt(offsetStart);
      gp_Pnt offsetEndPoint = BRep_Tool::Pnt(offsetEnd);
      const double forwardDistance = spineStartPoint.Distance(offsetStartPoint)
          + spineEndPoint.Distance(offsetEndPoint);
      const double reverseDistance = spineStartPoint.Distance(offsetEndPoint)
          + spineEndPoint.Distance(offsetStartPoint);
      TopoDS_Wire offsetWire = *offset;
      if (reverseDistance < forwardDistance) {
        offsetWire.Reverse();
        TopoDS_Vertex vertex = offsetStart;
        offsetStart = offsetEnd;
        offsetEnd = vertex;
        offsetStartPoint = BRep_Tool::Pnt(offsetStart);
        offsetEndPoint = BRep_Tool::Pnt(offsetEnd);
      }

      TopoDS_Edge firstEdge, lastEdge;
      for (BRepTools_WireExplorer explorer(*spine); explorer.More(); explorer.Next()) {
        if (firstEdge.IsNull()) {
          firstEdge = TopoDS::Edge(explorer.Current());
        }
        lastEdge = TopoDS::Edge(explorer.Current());
      }
      if (firstEdge.IsNull() || lastEdge.IsNull()) {
        return nullptr;
      }

      auto tangentAt = [](const TopoDS_Edge& edge) {
        BRepAdaptor_Curve curve(edge);
        TopoDS_Vertex firstVertex, lastVertex;
        TopExp::Vertices(edge, firstVertex, lastVertex);
        const Standard_Real firstParameter = curve.FirstParameter();
        const Standard_Real lastParameter = curve.LastParameter();
        const gp_Pnt firstPoint = curve.Value(firstParameter);
        const gp_Pnt lastPoint = curve.Value(lastParameter);
        const bool forward = firstPoint.Distance(BRep_Tool::Pnt(firstVertex))
            <= lastPoint.Distance(BRep_Tool::Pnt(firstVertex));
        const Standard_Real parameter = forward ? firstParameter : lastParameter;
        gp_Pnt point;
        gp_Vec tangent;
        curve.D1(parameter, point, tangent);
        if (!forward) {
          tangent.Reverse();
        }
        if (tangent.SquareMagnitude() <= 1e-24) {
          return gp_Vec(0.0, 0.0, 0.0);
        }
        tangent.Normalize();
        return tangent;
      };
      gp_Vec startTangent = tangentAt(firstEdge);
      gp_Vec endTangent = tangentAt(lastEdge);
      if (startTangent.SquareMagnitude() <= 1e-24 || endTangent.SquareMagnitude() <= 1e-24) {
        return nullptr;
      }

      auto makeCap = [joinType](const gp_Pnt& base, const gp_Pnt& side,
                                gp_Vec tangent, const bool atEnd, TopoDS_Edge& cap) {
        const double width = base.Distance(side);
        if (width <= Precision::Confusion()) {
          cap.Nullify();
          return true;
        }
        const gp_Pnt first = atEnd ? side : base;
        const gp_Pnt last = atEnd ? base : side;
        if (joinType != GeomAbs_Arc) {
          cap = BRepBuilderAPI_MakeEdge(first, last).Edge();
          return !cap.IsNull();
        }
        gp_Pnt middle((base.X() + side.X()) * 0.5,
                      (base.Y() + side.Y()) * 0.5,
                      (base.Z() + side.Z()) * 0.5);
        tangent *= (atEnd ? width : -width) * 0.5;
        middle.Translate(tangent);
        cap = BRepBuilderAPI_MakeEdge(
            GC_MakeArcOfCircle(first, middle, last).Value()).Edge();
        return !cap.IsNull();
      };

      BRepBuilderAPI_MakeWire result;
      for (BRepTools_WireExplorer explorer(offsetWire); explorer.More(); explorer.Next()) {
        result.Add(TopoDS::Edge(explorer.Current()));
      }
      TopoDS_Edge endCap;
      if (!makeCap(spineEndPoint, offsetEndPoint, endTangent, true, endCap)) {
        return nullptr;
      }
      if (!endCap.IsNull()) {
        result.Add(endCap);
      }
      const TopoDS_Wire reversedSpine = TopoDS::Wire(spine->Reversed());
      for (BRepTools_WireExplorer explorer(reversedSpine); explorer.More(); explorer.Next()) {
        result.Add(TopoDS::Edge(explorer.Current()));
      }
      TopoDS_Edge startCap;
      if (!makeCap(spineStartPoint, offsetStartPoint, startTangent, false, startCap)) {
        return nullptr;
      }
      if (!startCap.IsNull()) {
        result.Add(startCap);
      }
      if (!result.IsDone()) {
        return nullptr;
      }
      const TopoDS_Wire closed = result.Wire();
      if (!BRep_Tool::IsClosed(closed)) {
        return nullptr;
      }
      return new TopoDS_Wire(result.Wire());
    } catch (...) {
      return nullptr;
    }
    } |]

offsetPathWithOpenResult :: CDouble -> CInt -> CBool -> Path -> Path
offsetPathWithOpenResult amount join openResult input = propagateSideOffsetColors input $
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
          if ($(int join) == GeomAbs_Arc) {
            wire.Add(BRepBuilderAPI_MakeEdge(
                GC_MakeArcOfCircle(endLeft, endCapMid, endRight).Value()).Edge());
          } else {
            wire.Add(BRepBuilderAPI_MakeEdge(endLeft, endRight).Edge());
          }
          wire.Add(BRepBuilderAPI_MakeEdge(endRight, startRight).Edge());
          if ($(int join) == GeomAbs_Arc) {
            wire.Add(BRepBuilderAPI_MakeEdge(
                GC_MakeArcOfCircle(startRight, startCapMid, startLeft).Value()).Edge());
          } else {
            wire.Add(BRepBuilderAPI_MakeEdge(startRight, startLeft).Edge());
          }
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
      if (isOpenResult && result.ShapeType() == TopAbs_EDGE) {
        BRepBuilderAPI_MakeWire wire(TopoDS::Edge(result));
        if (!wire.IsDone()) {
          return nullptr;
        }
        return new TopoDS_Wire(wire.Wire());
      }
      if (result.ShapeType() != TopAbs_COMPOUND) {
        return nullptr;
      }
      // Path represents a single wire; use the first wire from compounds.
      TopExp_Explorer wireExplorer(result, TopAbs_WIRE);
      if (!wireExplorer.More()) {
        if (!isOpenResult) {
          return nullptr;
        }
        BRepBuilderAPI_MakeWire wire;
        for (TopExp_Explorer edgeExplorer(result, TopAbs_EDGE); edgeExplorer.More(); edgeExplorer.Next()) {
          wire.Add(TopoDS::Edge(edgeExplorer.Current()));
        }
        if (!wire.IsDone()) {
          return nullptr;
        }
        return new TopoDS_Wire(wire.Wire());
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
