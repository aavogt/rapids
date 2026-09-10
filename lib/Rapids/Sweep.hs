{-# LANGUAGE QuasiQuotes #-}

-- | Sweep a profile while allowing each profile vertex to have its own path.
module Rapids.Sweep (sweepRuled) where

import Control.Monad.IO.Class (liftIO)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (Ptr, castPtr)
import InlineOCCT (occtContext, ownSolid)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear (V2 (..))
import Rapids.ToPath (ToPath (..))
import Rapids.ToShape (ToShape (..))
import System.IO.Unsafe (unsafePerformIO)
import Waterfall (Path, Shape, Solid, emptySolid)
import qualified Waterfall.Internal.Path as InternalPath
import Waterfall.Internal.Path.Common (rawPathWire)

C.context occtContext
Cpp.include "<BRepFill.hxx>"
Cpp.include "<BRepBuilderAPI_MakeEdge.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepBuilderAPI_MakeSolid.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<BRepBuilderAPI_Sewing.hxx>"
Cpp.include "<BRepTools.hxx>"
Cpp.include "<BRepTools_WireExplorer.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<GProp_GProps.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopTools_IndexedMapOfShape.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Edge.hxx>"
Cpp.include "<TopoDS_Face.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Shell.hxx>"
Cpp.include "<TopoDS_Solid.hxx>"
Cpp.include "<TopoDS_Vertex.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<gp_Pnt.hxx>"

-- | Sweep a profile when its vertices do not share one common spine.
--
-- Each profile vertex is mapped to a path by the first argument.  Every
-- profile edge is then made into a ruled surface between the two mapped
-- paths.  The profile and the resulting end profile are sewn to those
-- surfaces and returned as a solid when the resulting shell is closed.
sweepRuled :: (ToPath path, ToShape shape) => (V2 Double -> path) -> shape -> Solid
sweepRuled pathForVertex input =
  let profile = toShape input
      vertices = shapeVertices profile
      paths = map (toPath . pathForVertex) vertices
   in case pathWires paths of
        Nothing -> emptySolid
        Just wires ->
          buildSweep2 profile wires

shapeVertices :: Shape -> [V2 Double]
shapeVertices profile = unsafePerformIO $ do
  count <- fromIntegral <$> [Cpp.block| int {
    TopoDS_Shape* input = $shape:profile;
    if (input == nullptr || input->IsNull()) {
      return 0;
    }
    TopTools_IndexedMapOfShape vertices;
    TopExp::MapShapes(*input, TopAbs_VERTEX, vertices);
    return vertices.Extent();
  } |]
  allocaArray (2 * count) $ \output -> do
    liftIO [Cpp.block| void {
      TopoDS_Shape* input = $shape:profile;
      double* output = $(double *output);
      if (input == nullptr || input->IsNull()) {
        return;
      }
      TopTools_IndexedMapOfShape vertices;
      TopExp::MapShapes(*input, TopAbs_VERTEX, vertices);
      for (int i = 1; i <= vertices.Extent(); ++i) {
        gp_Pnt point = BRep_Tool::Pnt(TopoDS::Vertex(vertices(i)));
        output[2 * (i - 1)] = point.X();
        output[2 * (i - 1) + 1] = point.Y();
      }
    } |]
    values <- peekArray (2 * count) output
    pure [V2 (realToFrac x) (realToFrac y) | [x, y] <- pairs values]
  where
    pairs (x : y : rest) = [x, y] : pairs rest
    pairs _ = []
{-# NOINLINE shapeVertices #-}

pathWires :: [Path] -> Maybe [Ptr ()]
pathWires = traverse pathWire
  where
    pathWire (InternalPath.Path raw) = castPtr <$> rawPathWire raw

buildSweep2 :: Shape -> [Ptr ()] -> Solid
buildSweep2 profile wires =
  let wireCount = fromIntegral (length wires) :: CInt
   in ownSolid $ withArray wires $ \wireArray ->
        [Cpp.block| TopoDS_Shape* {
          TopoDS_Shape* result = new TopoDS_Shape();
          TopoDS_Shape* input = $shape:profile;
          void** pathPtrs = $(void** wireArray);
          const int pathCount = $(int wireCount);
          if (input == nullptr || input->IsNull() || pathCount == 0) {
            return result;
          }

          try {
            TopTools_IndexedMapOfShape vertices;
            TopExp::MapShapes(*input, TopAbs_VERTEX, vertices);
            if (vertices.Extent() != pathCount) {
              return result;
            }

            std::vector<TopoDS_Wire> paths;
            std::vector<gp_Pnt> pathEnds;
            paths.reserve((size_t)pathCount);
            pathEnds.reserve((size_t)pathCount);
            for (int i = 0; i < pathCount; ++i) {
              TopoDS_Wire* path = (TopoDS_Wire*)pathPtrs[i];
              if (path == nullptr || path->IsNull()) {
                return result;
              }
              TopoDS_Wire wire = *path;
              TopoDS_Vertex first, last;
              TopExp::Vertices(wire, first, last);
              if (first.IsNull() || last.IsNull()) {
                return result;
              }
              paths.push_back(wire);
              pathEnds.push_back(BRep_Tool::Pnt(last));
            }

            BRepBuilderAPI_Sewing sewing(1.0e-7);

            // Keep the original profile as the start cap.  This also keeps
            // curved profile edges instead of replacing them with chords.
            for (TopExp_Explorer faces(*input, TopAbs_FACE);
                 faces.More(); faces.Next()) {
              sewing.Add(faces.Current());
            }

            // BRepFill::Shell applies BRepFill::Face to corresponding edges
            // of the two paths.  Thus each profile edge becomes a ruled
            // patch whose boundaries follow the two vertex paths.
            for (TopExp_Explorer edges(*input, TopAbs_EDGE);
                 edges.More(); edges.Next()) {
              TopoDS_Edge edge = TopoDS::Edge(edges.Current());
              TopoDS_Vertex first, last;
              TopExp::Vertices(edge, first, last, Standard_True);
              const int firstIndex = vertices.FindIndex(first);
              const int lastIndex = vertices.FindIndex(last);
              if (firstIndex <= 0 || lastIndex <= 0) {
                return result;
              }
              TopoDS_Shell side = BRepFill::Shell(
                paths[(size_t)(firstIndex - 1)],
                paths[(size_t)(lastIndex - 1)]);
              if (side.IsNull()) {
                return result;
              }
              sewing.Add(side);
            }

            // Rebuild the end cap from the terminal points of the paths.
            // The profile is planar in the public ToShape API, so a planar
            // wire made from those points is sufficient here.
            for (TopExp_Explorer faces(*input, TopAbs_FACE);
                 faces.More(); faces.Next()) {
              TopoDS_Face face = TopoDS::Face(faces.Current());
              TopoDS_Wire outer = BRepTools::OuterWire(face);
              BRepBuilderAPI_MakeWire outerBuilder;
              BRepTools_WireExplorer outerExplorer(outer);
              for (; outerExplorer.More(); outerExplorer.Next()) {
                TopoDS_Vertex first, last;
                TopExp::Vertices(
                  outerExplorer.Current(), first, last, Standard_True);
                const int firstIndex = vertices.FindIndex(first);
                const int lastIndex = vertices.FindIndex(last);
                if (firstIndex <= 0 || lastIndex <= 0) {
                  return result;
                }
                outerBuilder.Add(BRepBuilderAPI_MakeEdge(
                  pathEnds[(size_t)(firstIndex - 1)],
                  pathEnds[(size_t)(lastIndex - 1)]).Edge());
              }
              if (!outerBuilder.IsDone()) {
                return result;
              }
              BRepBuilderAPI_MakeFace endFaceBuilder(outerBuilder.Wire());
              if (!endFaceBuilder.IsDone()) {
                return result;
              }

              for (TopExp_Explorer holes(face, TopAbs_WIRE);
                   holes.More(); holes.Next()) {
                TopoDS_Wire hole = TopoDS::Wire(holes.Current());
                if (hole.IsSame(outer)) {
                  continue;
                }
                BRepBuilderAPI_MakeWire holeBuilder;
                BRepTools_WireExplorer holeExplorer(hole);
                for (; holeExplorer.More(); holeExplorer.Next()) {
                  TopoDS_Vertex first, last;
                  TopExp::Vertices(
                    holeExplorer.Current(), first, last, Standard_True);
                  const int firstIndex = vertices.FindIndex(first);
                  const int lastIndex = vertices.FindIndex(last);
                  if (firstIndex <= 0 || lastIndex <= 0) {
                    return result;
                  }
                  holeBuilder.Add(BRepBuilderAPI_MakeEdge(
                    pathEnds[(size_t)(firstIndex - 1)],
                    pathEnds[(size_t)(lastIndex - 1)]).Edge());
                }
                if (!holeBuilder.IsDone()) {
                  return result;
                }
                endFaceBuilder.Add(holeBuilder.Wire());
              }
              sewing.Add(endFaceBuilder.Face());
            }

            sewing.Perform();
            TopoDS_Shape sewed = sewing.SewedShape();
            if (sewed.IsNull()) {
              return result;
            }
            if (sewed.ShapeType() == TopAbs_SOLID) {
              *result = sewed;
              return result;
            }

            BRepBuilderAPI_MakeSolid solidBuilder;
            int shellCount = 0;
            for (TopExp_Explorer shells(sewed, TopAbs_SHELL);
                 shells.More(); shells.Next()) {
              solidBuilder.Add(TopoDS::Shell(shells.Current()));
              ++shellCount;
            }
            if (shellCount > 0 && solidBuilder.IsDone()) {
              TopoDS_Solid solid = solidBuilder.Solid();
              GProp_GProps props;
              BRepGProp::VolumeProperties(
                solid, props, Standard_False, Standard_False, Standard_False);
              if (props.Mass() < 0.0) {
                solid.Reverse();
              }
              *result = solid;
            } else {
              *result = sewed;
            }
            return result;
          } catch (...) {
            return result;
          }
        } |]
