{-# LANGUAGE QuasiQuotes #-}

module Rapids.Silhouette
  ( Silhouette(silhouette),
    silhouetteSolid,
    silhouetteShape,
    silhouettePaths,
  )
where

import Data.Maybe (mapMaybe)
import Foreign (Ptr, castPtr)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (withArray)
import InlineOCCT (occtContext, ownShape)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Waterfall (Path2D, Shape, Solid)
import Waterfall.Internal.Path
import Waterfall.Internal.Path.Common (rawPathWire)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))

C.context occtContext
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<BRepLib.hxx>"
Cpp.include "<Bnd_Box.hxx>"
Cpp.include "<BRepBndLib.hxx>"
Cpp.include "<cmath>"
Cpp.include "<BRepAlgo_FaceRestrictor.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<BRepBuilderAPI_MakeEdge.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<GeomProjLib.hxx>"
Cpp.include "<Geom_Plane.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<gp.hxx>"
Cpp.include "<ShapeAnalysis_FreeBounds.hxx>"
Cpp.include "<TopTools_HSequenceOfShape.hxx>"
Cpp.include "<ShapeUpgrade_UnifySameDomain.hxx>"

Cpp.include "<BRepCheck_Analyzer.hxx>"
Cpp.include "<HLRAlgo_Projector.hxx>"
Cpp.include "<HLRBRep_Algo.hxx>"
Cpp.include "<HLRBRep_HLRToShape.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<gp_Ax2.hxx>"
Cpp.include "<gp_Dir.hxx>"
Cpp.include "<gp_Pnt.hxx>"


class Silhouette a where
  silhouette :: a -> Shape

instance Silhouette Solid where silhouette = silhouetteSolid
instance Silhouette Shape where silhouette = silhouetteShape
instance Silhouette [Path2D] where silhouette = silhouettePaths

-- | Return the visible silhouette of a solid projected along +Z.
silhouetteSolid :: Solid -> Shape
silhouetteSolid solid = ownShape [Cpp.block| TopoDS_Shape* {
  TopoDS_Shape* input = $solid:solid;
  if (input == nullptr || input->IsNull()) {
    return nullptr;
  }

  try {
    HLRAlgo_Projector projector(
        gp_Ax2(gp_Pnt(0.0, 0.0, 0.0), gp_Dir(0.0, 0.0, 1.0)));
    Handle(HLRBRep_Algo) hlr = new HLRBRep_Algo();
    ShapeUpgrade_UnifySameDomain unifier(*input, Standard_True, Standard_True);
    unifier.Build();
    hlr->Add(unifier.Shape());
    hlr->Projector(projector);
    hlr->Update();
    hlr->Hide();

    HLRBRep_HLRToShape extractor(hlr);
    TopoDS_Shape visible = extractor.VCompound();
    if (visible.IsNull()) {
      return nullptr;
    }
    BRepLib::BuildCurves3d(visible, 1.0e-9, GeomAbs_C2, 14, 100);
    Handle(TopTools_HSequenceOfShape) edges = new TopTools_HSequenceOfShape;
    for (TopExp_Explorer edgeExplorer(visible, TopAbs_EDGE);
         edgeExplorer.More(); edgeExplorer.Next()) {
      edges->Append(edgeExplorer.Current());
    }
    if (edges->Length() == 0) {
      return nullptr;
    }
    Handle(TopTools_HSequenceOfShape) connectedWires =
        new TopTools_HSequenceOfShape;
    ShapeAnalysis_FreeBounds::ConnectEdgesToWires(
        edges, 1.0e-7, Standard_False, connectedWires);
    if (connectedWires->Length() == 0) {
      return nullptr;
    }

    Handle(TopTools_HSequenceOfShape) projectedWires =
        new TopTools_HSequenceOfShape;
    Handle(Geom_Plane) plane = new Geom_Plane(gp::XOY());
    gp_Dir projectionDir(0.0, 0.0, 1.0);
    for (Standard_Integer i = 1; i <= connectedWires->Length(); ++i) {
      BRepBuilderAPI_MakeWire projectedWire;
      for (TopExp_Explorer edgeExplorer(
               connectedWires->Value(i), TopAbs_EDGE);
           edgeExplorer.More(); edgeExplorer.Next()) {
        TopoDS_Edge edge = TopoDS::Edge(edgeExplorer.Current());
        Standard_Real first = 0.0;
        Standard_Real last = 0.0;
        Handle(Geom_Curve) curve = BRep_Tool::Curve(edge, first, last);
        if (curve.IsNull()) {
          continue;
        }
        Handle(Geom_Curve) projected = GeomProjLib::ProjectOnPlane(
            curve, plane, projectionDir, Standard_True);
        if (projected.IsNull()) {
          continue;
        }
        BRepBuilderAPI_MakeEdge projectedEdge(projected, first, last);
        if (projectedEdge.IsDone()) {
          projectedWire.Add(projectedEdge.Edge());
        }
      }
      if (projectedWire.IsDone()) {
        projectedWires->Append(projectedWire.Wire());
      }
    }
    if (projectedWires->Length() == 0) {
      return nullptr;
    }

    gp_Pln projectionPlane;
    TopoDS_Face planeFace = BRepBuilderAPI_MakeFace(projectionPlane);
    BRepAlgo_FaceRestrictor restrictor;
    restrictor.Init(planeFace, Standard_True, Standard_True);
    for (Standard_Integer i = 1; i <= projectedWires->Length(); ++i) {
      TopoDS_Wire wire = TopoDS::Wire(projectedWires->Value(i));
      restrictor.Add(wire);
    }
    restrictor.Perform();

    TopoDS_Compound result;
    BRep_Builder resultBuilder;
    resultBuilder.MakeCompound(result);
    while (restrictor.More()) {
      resultBuilder.Add(result, restrictor.Current());
      restrictor.Next();
    }
    return new TopoDS_Shape(result);
  } catch (...) {
    return nullptr;
  }
} |]

-- | Return the visible silhouette of a shape projected along +Z.
silhouetteShape :: Shape -> Shape
silhouetteShape shape = ownShape [Cpp.block| TopoDS_Shape* {
  TopoDS_Shape* input = $shape:shape;
  if (input == nullptr || input->IsNull()) {
    return nullptr;
  }
    Bnd_Box bounds;
    BRepBndLib::Add(*input, bounds);
    Standard_Real xMin = 0.0;
    Standard_Real yMin = 0.0;
    Standard_Real zMin = 0.0;
    Standard_Real xMax = 0.0;
    Standard_Real yMax = 0.0;
    Standard_Real zMax = 0.0;
    bounds.Get(xMin, yMin, zMin, xMax, yMax, zMax);
    if (std::fabs(zMin) <= 1.0e-7 && std::fabs(zMax) <= 1.0e-7) {
      return new TopoDS_Shape(*input);
    }

  try {
    TopExp_Explorer faceExplorer(*input, TopAbs_FACE);
    if (!faceExplorer.More()) {
      TopoDS_Wire wire;
      BRep_Builder builder;
      builder.MakeWire(wire);
      bool hasEdge = false;
      for (TopExp_Explorer edges(*input, TopAbs_EDGE); edges.More(); edges.Next()) {
        builder.Add(wire, edges.Current());
        hasEdge = true;
      }
      return hasEdge ? new TopoDS_Wire(wire) : nullptr;
    }
    BRepCheck_Analyzer validity(*input);
    if (!validity.IsValid()) {
      TopoDS_Wire wire;
      BRep_Builder builder;
      builder.MakeWire(wire);
      bool hasEdge = false;
      for (TopExp_Explorer edges(*input, TopAbs_EDGE); edges.More(); edges.Next()) {
        builder.Add(wire, edges.Current());
        hasEdge = true;
      }
      return hasEdge ? new TopoDS_Wire(wire) : nullptr;
    }
    HLRAlgo_Projector projector(
        gp_Ax2(gp_Pnt(0.0, 0.0, 0.0), gp_Dir(0.0, 0.0, 1.0)));
    Handle(HLRBRep_Algo) hlr = new HLRBRep_Algo();
    hlr->Add(*input);
    hlr->Projector(projector);
    hlr->Update();
    hlr->Hide();

    HLRBRep_HLRToShape extractor(hlr);
    TopoDS_Shape outline = extractor.OutLineVCompound();
    bool hasOutline = false;
    if (!outline.IsNull()) {
      for (TopExp_Explorer edges(outline, TopAbs_EDGE); edges.More(); edges.Next()) {
        hasOutline = true;
        break;
      }
    }
    if (!hasOutline) {
      // Planar faces have no tangency outline; their visible boundary is the silhouette.
      outline = extractor.VCompound();
    }
    if (outline.IsNull()) {
      return nullptr;
    }

    TopoDS_Wire wire;
    BRep_Builder builder;
    builder.MakeWire(wire);
    bool hasEdge = false;
    for (TopExp_Explorer edges(outline, TopAbs_EDGE); edges.More(); edges.Next()) {
      builder.Add(wire, edges.Current());
      hasEdge = true;
    }
    return hasEdge ? new TopoDS_Shape(wire) : nullptr;
  } catch (...) {
    return nullptr;
  }
} |]

-- | Build a permissive shape from paths, preserving open and disconnected edges.
--
-- Unlike 'toShape', this does not try to make faces, so it accepts edge drawings
-- such as SVG output containing many independent path fragments.
silhouettePaths :: [Path2D] -> Shape
silhouettePaths paths =
  let wires = mapMaybe pathWire paths
      wireCount = fromIntegral (length wires) :: CInt
   in ownShape $ withArray wires $ \wireArray ->
        [Cpp.block| TopoDS_Shape* {
          void** pathPtrs = $(void** wireArray);
          const int pathCount = $(int wireCount);
          if (pathCount == 0) {
            return new TopoDS_Shape();
          }

          try {
            Handle(TopTools_HSequenceOfShape) edges = new TopTools_HSequenceOfShape;
            for (int i = 0; i < pathCount; ++i) {
              TopoDS_Wire* path = (TopoDS_Wire*)pathPtrs[i];
              if (path == nullptr || path->IsNull()) {
                continue;
              }
              for (TopExp_Explorer edgeExplorer(*path, TopAbs_EDGE);
                   edgeExplorer.More(); edgeExplorer.Next()) {
                edges->Append(edgeExplorer.Current());
              }
            }
            if (edges->Length() == 0) {
              return new TopoDS_Shape();
            }

            Handle(TopTools_HSequenceOfShape) connectedWires =
                new TopTools_HSequenceOfShape;
            ShapeAnalysis_FreeBounds::ConnectEdgesToWires(
                edges, 1.0e-7, Standard_False, connectedWires);

            TopoDS_Compound result;
            BRep_Builder resultBuilder;
            resultBuilder.MakeCompound(result);
            for (Standard_Integer i = 1; i <= connectedWires->Length(); ++i) {
              resultBuilder.Add(result, connectedWires->Value(i));
            }
            return new TopoDS_Shape(result);
          } catch (...) {
            return new TopoDS_Shape();
          }
        } |]
  where
    pathWire (Path2D raw) = castPtr <$> rawPathWire raw
