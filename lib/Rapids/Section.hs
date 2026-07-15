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
import Rapids.Path.Project

C.context occtContext
Cpp.include "<BRepExtrema_DistShapeShape.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS.hxx>"
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

-- | @paths = section s@
--
-- section a solid @s@ with the xy plane
-- returning all paths that intersect
section :: Solid -> [Path2D]
section solid = fmap projectPath . unsafeFromAcquireT $
  liftIO [Cpp.block| void* {
    gp_Pln pl;
    TopoDS_Face planeFace = BRepBuilderAPI_MakeFace(pl);
    BRepAlgoAPI_Section section(* $solid:solid,planeFace);
    section.Build();

    if (!section.IsDone()) {
        return NULL;
    }

    return new TopoDS_Shape(section.Shape());
  } |]
    >>= \raw ->
      if raw == nullPtr then return [] else recombine 1e-7 <$> allEdgesAsPaths raw

-- | Waterfall.Internal.Edges.'allWires' doesn't find anything, allEdges finds the edges without connectivity,
allEdgesAsPaths :: Ptr () -> Acquire [Path]
allEdgesAsPaths raw = mapM (fmap (Path . ComplexRawPath) . edgeToWire) =<< allEdges (castPtr raw)

-- | @ps2 = recombine tol ps@ combines paths that share endpoints. Paths will be reversed if two starts are the same.
-- tol applies to the Linear.'distance'.
--
--
-- this one will be slow with many edges. n^2 ish if the edges are randomly ordered
-- we could probably sort by Linear.angle around a mean(?) after projecting into the sectioning plane
--
-- alternatives
-- bucket grids intmap^3 or array (lookup adjacent buckets if we're close to the edge)
-- kd tree
-- plane sweep
recombine :: Double -> [Path] -> [Path]
recombine tol ps = map fst $ foldl (go same) [] (mapMaybe (\p -> (p,) <$> pathEndpoints p) ps)
  where
    same x y = distance x y <= tol

go :: (b -> b -> Bool) -> [(Path, (b, b))] -> (Path, (b, b)) -> [(Path, (b, b))]
go same accum pft@(p, (p1, p2)) =
  fromMaybe (pft : accum) $ listToMaybe $ mapMaybe ($ accum) [tryL, tryR, revL, revR]
  where
    tryL = tryp (same p2 . fst) (p <>) ((p1,) . snd)
    tryR = tryp (same p1 . snd) (<> p) ((,p2) . fst)
    revL = tryp (same b2 . fst) (b <>) ((b1,) . snd)
    revR = tryp (same b1 . snd) (<> b) ((,b2) . fst)
    b = reversePath p
    b1 = p2
    b2 = p1

tryp :: ((s, t) -> Bool) -> (a -> a) -> ((s, t) -> (s, t)) -> [(a, (s, t))] -> Maybe [(a, (s, t))]
tryp g f h = setFirst (g . snd) (\(q, q1q2) -> let r = f q in (r, h q1q2))

setFirst :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
setFirst p f (x : xs)
  | p x = Just (f x : xs)
  | otherwise = (x :) <$> setFirst p f xs
setFirst _ _ [] = Nothing

testNested :: IO Bool
testNested = do
  let [a, b, c, d, e] = unitSphere : [uScale n unitSphere | n <- [2, 3, 4, 5]]
      abcde = e -- unions [ difference e d,  difference c b, a ]
  let sec = section abcde
  print (map pathEndpoints sec)
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

  let per2 = sum (map Waterfall.pathLength2D secPaths)

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
