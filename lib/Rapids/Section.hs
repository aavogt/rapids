{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Rapids.Section where

import Control.Monad
import Control.Monad.IO.Class
import Data.Acquire
import Data.Foldable
import Data.Functor
import Foreign
import Foreign.C.Types
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.BRepAdaptor.Curve as BRepAdaptor.Curve
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import qualified OpenCascade.BRepGProp as BRepGProp
import qualified OpenCascade.BRepLib as BRepLib
import qualified OpenCascade.BRepTools.WireExplorer as WireExplorer
import qualified OpenCascade.GCPnts.AbscissaPoint as AbscissaPoint
import qualified OpenCascade.GP.Vec as GPVec
import qualified OpenCascade.GProp.GProps as GProps
import qualified OpenCascade.Geom.Curve as Geom.Curve
import OpenCascade.GeomAbs.Shape as GeomAbs.Shape hiding (Shape)
import OpenCascade.Inheritance (unsafeDowncast, upcast)
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopExp.Explorer as Explorer
import qualified OpenCascade.TopTools.ShapeMapHasher as TopTools.ShapeMapHasher
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import System.Random
import Waterfall
import Waterfall.Internal.Edges
import Waterfall.Internal.Finalizers (unsafeFromAcquire, unsafeFromAcquireT)
import Waterfall.Internal.FromOpenCascade (gpPntToV3, gpVecToV3)
import Waterfall.Internal.Path
import Waterfall.Internal.Path.Common (RawPath (ComplexRawPath))
import Waterfall.Internal.ToOpenCascade (v3ToPnt)

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
Cpp.include "<ShapeAnalysis_FreeBounds.hxx>"
Cpp.include "<TopTools_HSequenceOfShape.hxx>"
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"

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

-- | @paths = section s n x@
--
-- section a solid @s@ with the plane defined by normal @n@ and point @x@,
-- returning all paths
section :: Solid -> V3 Double -> V3 Double -> IO [Path]
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

    Handle(TopTools_HSequenceOfShape) edges = new TopTools_HSequenceOfShape();
    for (TopExp_Explorer edgeExplorer(result, TopAbs_EDGE); edgeExplorer.More(); edgeExplorer.Next()) {
        edges->Append(edgeExplorer.Current());
    }

    Handle(TopTools_HSequenceOfShape) wires = new TopTools_HSequenceOfShape();
    // Use geometric proximity (not shared-vertex identity), since section edges
    // often have coincident endpoints that are not topologically shared.
    ShapeAnalysis_FreeBounds::ConnectEdgesToWires(edges, 1e-6, Standard_False, wires);

    TopoDS_Compound out;
    BRep_Builder builder;
    builder.MakeCompound(out);

    if (wires->Length() > 0) {
        for (Standard_Integer i = 1; i <= wires->Length(); ++i) {
            builder.Add(out, wires->Value(i));
        }
    } else {
        // Fallback: preserve section content as individual edges if no wire could be built.
        for (Standard_Integer i = 1; i <= edges->Length(); ++i) {
            builder.Add(out, edges->Value(i));
        }
    }

    return new TopoDS_Shape(out);
  } |]
    <&> \raw ->
      if raw == nullPtr
        then []
        else
          [ Path $ ComplexRawPath wire
            | wire <- unsafeFromAcquireT $ allWiresCopy (castPtr raw)
          ]

allWiresCopy :: Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Wire]
allWiresCopy s = do
  ws <- traverse (liftIO . unsafeDowncast) =<< allSubShapesWithCopy ShapeEnum.Wire s
  if null ws -- always null!
    then mapM edgeToWire =<< allEdges s
    else pure ws

-- copy waterfall-cad-0.6.2.1/src/Waterfall/Internal/Edges.hs
allSubShapesWithCopy :: ShapeEnum.ShapeEnum -> Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Shape]
allSubShapesWithCopy t s = do
  explorer <- Explorer.new s t
  let go visited = do
        isMore <- liftIO $ Explorer.more explorer
        if isMore
          then do
            v <- liftIO $ Explorer.value explorer
            hash <- liftIO $ TopTools.ShapeMapHasher.hash v
            add <-
              if hash `elem` visited
                then pure id
                else do
                  v' <- TopoDS.Shape.copy v
                  return (v' :)
            liftIO $ Explorer.next explorer
            add <$> go visited
          else return []
  go []

testNested :: IO Bool
testNested = do
  let [a, b, c, d, e] = unitSphere : [uScale n unitSphere | n <- [2, 3, 4, 5]]
      abcde = e -- unions [ difference e d,  difference c b, a ]
  sec <- section abcde (V3 1 0 0) 0
  print (map pathEndpoints sec)
  return True

testPerimetersEqual :: IO Bool
testPerimetersEqual = do
  let base = union unitCube unitSphere
      samples = 200
      tol = 1e-6

  and <$> replicateM samples (oneCase base tol)

oneCase :: Solid -> Double -> IO Bool
oneCase base tol = do
  p <- randomVec3
  n0 <- randomNonZeroVec3
  let n = normalize n0

  secPaths <- section base n p
  per1 <- sectionPerimeter base n p

  let per2 = sum (map Waterfall.pathLength3D secPaths)

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
