{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Rapids.Section where

import Control.Monad
import Data.Functor
import Foreign
import Foreign.C.Types
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear
import Linear.V3 (V3 (..))
import qualified OpenCascade.GP.Vec as GPVec
import System.Random
import Waterfall
import Waterfall.Internal.Path
import Waterfall.Internal.Path.Common (RawPath (ComplexRawPath))
import Waterfall.Internal.Solid
import Waterfall.TwoD.Internal.Shape

C.context occtContext
Cpp.include "<BRepExtrema_DistShapeShape.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<BRepAlgoAPI_Section.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<BRepGProp_Cinert.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"
Cpp.include "<TopoDS_Builder.hxx>"

-- | @p = sectionPerimeter1 s n x@
--
-- section a solid @s@ with the plane defined by normal @n@ and point @x@,
-- giving the perimeter @p@ (strictly speaking, the total length of all wires in that plane)
sectionPerimeter :: Solid -> V3 Double -> V3 Double -> IO CDouble
sectionPerimeter solid n p =
  [Cpp.block| double { 
    gp_Pln pl = gp_Pln(* $pnt:p,* $dir:n);
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

section :: Solid -> V3 Double -> V3 Double -> IO Path
section solid n p =
  [Cpp.block| void* {
    gp_Pln pl = gp_Pln(* $pnt:p,* $dir:n);
    TopoDS_Face planeFace = BRepBuilderAPI_MakeFace(pl);
    BRepAlgoAPI_Section section(* $solid:solid,planeFace);
    section.Build();

    if (!section.IsDone()) {
        return NULL;
    }

    TopoDS_Shape result = section.Shape();

    // Waterfall.shapePaths explores TopAbs_WIRE. BRepAlgoAPI_Section usually
    // returns a compound of edges, so wrap each edge into a wire first.
    TopoDS_Compound wires;
    TopoDS_Builder builder;
    builder.MakeCompound(wires);

    TopExp_Explorer edgeExplorer(result, TopAbs_EDGE);
    for (; edgeExplorer.More(); edgeExplorer.Next()) {
        TopoDS_Edge edge = TopoDS::Edge(edgeExplorer.Current());
        BRepBuilderAPI_MakeWire makeWire;
        makeWire.Add(edge);
        if (makeWire.IsDone()) {
            builder.Add(wires, makeWire.Wire());
        }
    }

    return new TopoDS_Shape(wires);
  } |]
    <&> Path . ComplexRawPath . castPtr

testSection :: IO Bool
testSection = do
  let base = union unitCube unitSphere
      samples = 200
      tol = 1e-6

  and <$> replicateM samples (oneCase base tol)

oneCase :: Solid -> Double -> IO Bool
oneCase base tol = do
  p <- randomVec3
  n0 <- randomNonZeroVec3
  let n = normalize n0

  sec <- section base n p
  per1 <- sectionPerimeter base n p
  
  let per2 = Waterfall.pathLength3D sec

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
