{-# LANGUAGE QuasiQuotes #-}
module Rapids.Section where

import Control.Monad
import Data.Acquire (Acquire)
import Data.Maybe
import Foreign hiding (rotate)
import Foreign.C.Types
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear hiding (rotate)
import System.Random
import Waterfall
import Waterfall.Internal.Edges
import Waterfall.Internal.Finalizers
import Waterfall.Internal.Path
import Waterfall.Internal.Path.Common
import Control.Monad.IO.Class
import Language.Haskell.TH (unsafe)
import System.IO.Unsafe (unsafePerformIO)

C.context occtContext
Cpp.include "<BRepExtrema_DistShapeShape.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<BRepAlgo_FaceRestrictor.hxx>"
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<ShapeAnalysis_FreeBounds.hxx>"
Cpp.include "<TopTools_HSequenceOfShape.hxx>"
Cpp.include "<TopTools_ListOfShape.hxx>"
Cpp.include "<TopTools_ListIteratorOfListOfShape.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<BRepAlgoAPI_Section.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<BRepGProp_Cinert.hxx>"

-- | @p = sectionPerimeter s@
--
-- section a solid @s@ with the xy plane
-- giving the perimeter @p@ (strictly speaking, the total length of all wires in that plane)
sectionPerimeter :: Solid -> CDouble
sectionPerimeter solid = unsafePerformIO
  [Cpp.block| double {
    gp_Pln pl;
    TopoDS_Face planeFace = BRepBuilderAPI_MakeFace(pl);
    BRepAlgoAPI_Section section(* $solid:solid,planeFace);
    section.Build();

    if (!section.IsDone()) {
        return 0.0;
    }

    TopoDS_Shape result = section.Shape();
    Standard_Real totalLength = 0.0;
    TopExp_Explorer edgeExplorer(result, TopAbs_EDGE);
    for (; edgeExplorer.More(); edgeExplorer.Next()) {
        TopoDS_Edge edge = TopoDS::Edge(edgeExplorer.Current());
        Standard_Real first, last;
        Handle(Geom_Curve) curve = BRep_Tool::Curve(edge, first, last);
        gp_Pnt start, end;
        curve->D0(first, start);
        curve->D0(last, end);
        auto props = BRepGProp_Cinert();
        if (pl.Distance(start) < 1e-6 && pl.Distance(end) < 1e-6) {
            BRepGProp::LinearProperties(edge, props);
            totalLength += props.Mass();
        }
    }
  return totalLength;
 }
|]

-- | @shape = section s@
--
-- section a solid @s@ with the xy plane
-- returning a planar shape whose inner contours are holes
section :: Solid -> Shape
section solid = ownShape [Cpp.block| TopoDS_Shape* {
    gp_Pln pl;
    TopoDS_Face planeFace = BRepBuilderAPI_MakeFace(pl);
    BRepAlgoAPI_Section section(* $solid:solid,planeFace);
    section.Build();

    if (!section.IsDone()) {
        return new TopoDS_Shape();
    }

    TopTools_ListOfShape edges;
    TopExp_Explorer edgeExplorer(section.Shape(), TopAbs_EDGE);
    for (; edgeExplorer.More(); edgeExplorer.Next()) {
        edges.Append(edgeExplorer.Current());
    }

    Handle(TopTools_HSequenceOfShape) edgeSequence = new TopTools_HSequenceOfShape;
    for (TopTools_ListIteratorOfListOfShape it(edges); it.More(); it.Next()) {
        edgeSequence->Append(it.Value());
    }

    Handle(TopTools_HSequenceOfShape) wires = new TopTools_HSequenceOfShape;
    ShapeAnalysis_FreeBounds::ConnectEdgesToWires(
        edgeSequence, 1e-7, Standard_False, wires);

    BRepAlgo_FaceRestrictor restrictor;
    restrictor.Init(planeFace, Standard_True, Standard_True);
    for (Standard_Integer i = 1; i <= wires->Length(); ++i) {
        TopoDS_Wire wire = TopoDS::Wire(wires->Value(i));
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
  } |]

testNested :: IO Bool
testNested = do
  let [a, b, c, d, e] = unitSphere : [uScale n unitSphere | n <- [2, 3, 4, 5]]
      abcde = e -- unions [ difference e d,  difference c b, a ]
  let sec = section abcde
  print (map pathEndpoints (shapePaths sec))
  return True

testPerimetersEqual :: IO Bool
testPerimetersEqual = do
  let base = unitCube `union` unitSphere
      samples = 200
      tol = 1e-6

  and <$> replicateM samples (oneCase base tol)

oneCase :: Solid -> Double -> IO Bool
oneCase base0 tol = do
  p <- randomVec3
  n0 <- randomNonZeroVec3
  let n = normalize n0

  let base = translate p $ rotate n (norm n0) base0
  let secPaths = section base
      per1 = sectionPerimeter base

  let per2 = sum (map Waterfall.pathLength (shapePaths secPaths))

  let ok = approx tol (realToFrac per1) per2
  unless ok $ do
    print (per1, per2)
  return ok

randomVec3 :: IO (V3 Double)
randomVec3 = do
  x <- randomRIO (-1, 1)
  y <- randomRIO (-1, 1)
  z <- randomRIO (-1, 1)
  pure (V3 x y z)

randomNonZeroVec3 :: IO (V3 Double)
randomNonZeroVec3 = do
  v <- randomVec3
  if norm v < 1e-9 then randomNonZeroVec3 else pure v

approx :: Double -> Double -> Double -> Bool
approx tol a b =
  let s = max 1 (max (abs a) (abs b))
   in abs (a - b) <= tol * s
