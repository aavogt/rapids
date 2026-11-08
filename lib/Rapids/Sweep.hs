{-# LANGUAGE QuasiQuotes #-}

-- | Sweep a shape while allowing each shape vertex to have its own path.
module Rapids.Sweep (sweepRuled) where

import Data.Acquire (Acquire, withAcquire)
import Control.Monad.IO.Class (liftIO)
import Rapids.Color
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

-- | @sweepRuled (\\ _shapeVertex -> path) shape = sweep path shape@ at least for shapes with straight edges.
--
-- sweepRuled creates ruled surfaces between adjacent paths.
-- The path-start and path-end cross-sections are sewn to those surfaces and
-- returned as a solid when the resulting shell is closed. If all paths start
-- at the input vertices, the original start face is kept to preserve curved
-- edges; otherwise it is rebuilt from the path starts.
sweepRuled :: (ToPath path, ToShape shape) => (V2 Double -> path) -> shape -> Solid
sweepRuled pathForVertex input =
  let shape = toShape input
      vertices = shapeVertices shape
      paths = map (toPath . pathForVertex) vertices
   in unsafePerformIO $
        withAcquire (pathWires paths) $ \maybeWires ->
          case maybeWires of
            Nothing -> pure emptySolid
            Just wires -> pure $ propagateShapeColors shape (buildSweep2 shape wires)

shapeVertices :: Shape -> [V2 Double]
shapeVertices shape = unsafePerformIO $ do
  count <- fromIntegral <$> [Cpp.block| int {
    TopoDS_Shape* input = $shape:shape;
    if (input == nullptr || input->IsNull()) {
      return 0;
    }
    TopTools_IndexedMapOfShape vertices;
    TopExp::MapShapes(*input, TopAbs_VERTEX, vertices);
    return vertices.Extent();
  } |]
  allocaArray (2 * count) $ \output -> do
    liftIO [Cpp.block| void {
      TopoDS_Shape* input = $shape:shape;
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

pathWires :: [Path] -> Acquire (Maybe [Ptr ()])
pathWires paths = sequence <$> traverse pathWire paths
  where
    pathWire (InternalPath.Path raw) = fmap (fmap castPtr) (rawPathWire raw)

buildSweep2 :: Shape -> [Ptr ()] -> Solid
buildSweep2 shape wires =
  let wireCount = fromIntegral (length wires) :: CInt
   in ownSolid $ withArray wires $ \wireArray ->
        [Cpp.block| TopoDS_Shape* {
          TopoDS_Shape* result = new TopoDS_Shape();
          TopoDS_Shape* input = $shape:shape;
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
            std::vector<gp_Pnt> pathStarts;
            std::vector<gp_Pnt> pathEnds;
            paths.reserve((size_t)pathCount);
            pathStarts.reserve((size_t)pathCount);
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
              pathStarts.push_back(BRep_Tool::Pnt(first));
              pathEnds.push_back(BRep_Tool::Pnt(last));
            }

            auto makeCapFace = [&](const TopoDS_Face& source,
                                   const std::vector<gp_Pnt>& points) -> TopoDS_Face {
              auto makeWire = [&](const TopoDS_Wire& wire) -> TopoDS_Wire {
                BRepBuilderAPI_MakeWire wireBuilder;
                BRepTools_WireExplorer explorer(wire);
                for (; explorer.More(); explorer.Next()) {
                  TopoDS_Vertex first, last;
                  TopExp::Vertices(explorer.Current(), first, last, Standard_True);
                  const int firstIndex = vertices.FindIndex(first);
                  const int lastIndex = vertices.FindIndex(last);
                  if (firstIndex <= 0 || lastIndex <= 0) {
                    return TopoDS_Wire();
                  }
                  wireBuilder.Add(BRepBuilderAPI_MakeEdge(
                    points[(size_t)(firstIndex - 1)],
                    points[(size_t)(lastIndex - 1)]).Edge());
                }
                return wireBuilder.IsDone() ? wireBuilder.Wire() : TopoDS_Wire();
              };

              TopoDS_Wire outer = BRepTools::OuterWire(source);
              TopoDS_Wire rebuiltOuter = makeWire(outer);
              if (rebuiltOuter.IsNull()) {
                return TopoDS_Face();
              }
              BRepBuilderAPI_MakeFace faceBuilder(rebuiltOuter);
              if (!faceBuilder.IsDone()) {
                return TopoDS_Face();
              }
              for (TopExp_Explorer wires(source, TopAbs_WIRE);
                   wires.More(); wires.Next()) {
                TopoDS_Wire wire = TopoDS::Wire(wires.Current());
                if (wire.IsSame(outer)) {
                  continue;
                }
                TopoDS_Wire rebuiltHole = makeWire(wire);
                if (rebuiltHole.IsNull()) {
                  return TopoDS_Face();
                }
                faceBuilder.Add(rebuiltHole);
              }
              return faceBuilder.Face();
            };

            bool startsOnShape = true;
            for (int i = 1; i <= vertices.Extent(); ++i) {
              const gp_Pnt inputPoint = BRep_Tool::Pnt(TopoDS::Vertex(vertices(i)));
              if (inputPoint.Distance(pathStarts[(size_t)(i - 1)]) > 1.0e-7) {
                startsOnShape = false;
                break;
              }
            }

            BRepBuilderAPI_Sewing sewing(1.0e-7);

            // Keep the original face when the paths start at its vertices,
            // preserving curved edges. Otherwise cap the actual path starts.
            for (TopExp_Explorer faces(*input, TopAbs_FACE);
                 faces.More(); faces.Next()) {
              TopoDS_Face face = TopoDS::Face(faces.Current());
              if (startsOnShape) {
                sewing.Add(face);
              } else {
                TopoDS_Face startFace = makeCapFace(face, pathStarts);
                if (startFace.IsNull()) {
                  return result;
                }
                sewing.Add(startFace);
              }
            }
            // BRepFill::Shell applies BRepFill::Face to corresponding edges
            // of the two paths.  Thus each shape edge becomes a ruled
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

            // Cap the terminal cross-section from the path endpoints.
            for (TopExp_Explorer faces(*input, TopAbs_FACE);
                 faces.More(); faces.Next()) {
              TopoDS_Face endFace = makeCapFace(
                TopoDS::Face(faces.Current()), pathEnds);
              if (endFace.IsNull()) {
                return result;
              }
              sewing.Add(endFace);
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
