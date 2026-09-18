{-# LANGUAGE QuasiQuotes #-}

module Rapids.Silhouette
  ( silhouetteSolid,
    silhouetteShape,
    silhouettePaths,
  )
where

import Data.Coerce (coerce)
import Data.Maybe (mapMaybe)
import Foreign (Ptr, castPtr)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (withArray)
import InlineOCCT (occtContext, ownPath, ownShape)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Waterfall (Path2D, Shape, Solid)
import Waterfall.Internal.Path
import Waterfall.Internal.Path.Common (rawPathWire)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))

C.context occtContext
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<ShapeAnalysis_FreeBounds.hxx>"
Cpp.include "<TopTools_HSequenceOfShape.hxx>"

Cpp.include "<BRepCheck_Analyzer.hxx>"
Cpp.include "<HLRAlgo_Projector.hxx>"
Cpp.include "<HLRBRep_Algo.hxx>"
Cpp.include "<HLRBRep_HLRToShape.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<gp_Ax2.hxx>"
Cpp.include "<gp_Dir.hxx>"
Cpp.include "<gp_Pnt.hxx>"

-- | Return the visible silhouette of a solid projected along +Z.
silhouetteSolid :: Solid -> Path2D
silhouetteSolid solid = coerce $ ownPath [Cpp.block| TopoDS_Wire* {
  TopoDS_Shape* input = $solid:solid;
  if (input == nullptr || input->IsNull()) {
    return nullptr;
  }

  try {
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
    return hasEdge ? new TopoDS_Wire(wire) : nullptr;
  } catch (...) {
    return nullptr;
  }
} |]

-- | Return the visible silhouette of a shape projected along +Z.
silhouetteShape :: Shape -> Path2D
silhouetteShape shape = coerce $ ownPath [Cpp.block| TopoDS_Wire* {
  TopoDS_Shape* input = $shape:shape;
  if (input == nullptr || input->IsNull()) {
    return nullptr;
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
    return hasEdge ? new TopoDS_Wire(wire) : nullptr;
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
