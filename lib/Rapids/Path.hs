{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
module Rapids.Path (
  module Rapids.Path,
  -- * reexports
  execState, zoom,
  _xy, _xz, _yz,
  _yx, _zx, _zy,
  ) where

import qualified Waterfall.Path as W
import Control.Monad.Trans.State
import Linear
import Data.Maybe (fromMaybe)
import qualified Waterfall.TwoD.Path2D as W
import Control.Lens

type PathState = State (V3 Double, W.Path)

appendPath :: W.Path -> PathState ()
appendPath path = do
  (current, acc) <- get
  let newEnd = fromMaybe current (snd <$> W.pathEndpoints3D path)
  put (newEnd, acc <> path)

appendSegment :: (V3 Double, W.Path) -> PathState ()
appendSegment (newEnd, seg) = do
  (_, acc) <- get
  put (newEnd, acc <> seg)

line3D :: V3 Double -> PathState ()
line3D end = do
  (start, _) <- get
  appendPath (W.line3D start end)

lineTo3D :: V3 Double -> PathState ()
lineTo3D end = do
  (start, _) <- get
  appendSegment (W.lineTo3D start end)

lineRelative3D :: V3 Double -> PathState ()
lineRelative3D delta = do
  (start, _) <- get
  appendSegment (W.lineRelative3D start delta)

arcVia3D :: V3 Double -> V3 Double -> PathState ()
arcVia3D via end = do
  (start, _) <- get
  appendPath (W.arcVia3D start via end)

arcViaTo3D :: V3 Double -> V3 Double -> PathState ()
arcViaTo3D via end = do
  (start, _) <- get
  appendSegment (W.arcViaTo3D start via end)

arcViaRelative3D :: V3 Double -> V3 Double -> PathState ()
arcViaRelative3D viaDelta endDelta = do
  (start, _) <- get
  appendSegment (W.arcViaRelative3D start viaDelta endDelta)

bezier3D :: V3 Double -> V3 Double -> V3 Double -> PathState ()
bezier3D c1 c2 end = do
  (start, _) <- get
  appendPath (W.bezier3D start c1 c2 end)

bezierTo3D :: V3 Double -> V3 Double -> V3 Double -> PathState ()
bezierTo3D c1 c2 end = do
  (start, _) <- get
  appendSegment (W.bezierTo3D start c1 c2 end)

bezierRelative3D :: V3 Double -> V3 Double -> V3 Double -> PathState ()
bezierRelative3D c1 c2 endDelta = do
  (start, _) <- get
  appendSegment (W.bezierRelative3D start c1 c2 endDelta)

pathFrom3D :: [V3 Double -> (V3 Double, W.Path)] -> PathState ()
pathFrom3D steps = do
  (start, _) <- get
  appendPath (W.pathFrom3D start steps)

pathFromTo3D :: [V3 Double -> (V3 Double, W.Path)] -> PathState ()
pathFromTo3D steps = do
  (start, _) <- get
  appendSegment (W.pathFromTo3D steps start)

pathEndpoints3D :: PathState (Maybe (V3 Double, V3 Double))
pathEndpoints3D = gets (W.pathEndpoints3D . snd)

closeLoop3D :: PathState ()
closeLoop3D = modify (\(p, acc) -> (p, W.closeLoop3D acc))

reversePath3D :: PathState ()
reversePath3D = modify (\(p, acc) -> (p, W.reversePath3D acc))

splice3D :: V3 Double -> PathState ()
splice3D point = do
  (current, acc) <- get
  let (newPoint, newPath) = W.splice3D acc point
  put (newPoint, newPath)

splitPath3D :: PathState [W.Path]
splitPath3D = gets (W.splitPath3D . snd)

pathLength3D :: PathState Double
pathLength3D = gets (W.pathLength3D . snd)

takePathFraction3D :: Double -> PathState ()
takePathFraction3D fraction =
  modify (\(p, acc) -> (p, W.takePathFraction3D fraction acc))


type PathState2 = State (V2 Double, W.Path2D)

appendPath2D :: W.Path2D -> PathState2 ()
appendPath2D path = do
  (current, acc) <- get
  let newEnd = fromMaybe current (snd <$> W.pathEndpoints2D path)
  put (newEnd, acc <> path)

appendSegment2D :: (V2 Double, W.Path2D) -> PathState2 ()
appendSegment2D (newEnd, seg) = do
  (_, acc) <- get
  put (newEnd, acc <> seg)

arc :: W.Sense -> Double -> V2 Double -> PathState2 ()
arc sense radius end = do
  (start, _) <- get
  appendPath2D (W.arc sense radius start end)

arcTo :: W.Sense -> Double -> V2 Double -> PathState2 ()
arcTo sense radius end = do
  (start, _) <- get
  appendSegment2D (W.arcTo sense radius start end)

arcRelative :: W.Sense -> Double -> V2 Double -> PathState2 ()
arcRelative sense radius endDelta = do
  (start, _) <- get
  appendSegment2D (W.arcRelative sense radius start endDelta)

repeatLooping :: PathState2 ()
repeatLooping = modify (\(p, acc) -> (p, W.repeatLooping acc))

line2D :: V2 Double -> PathState2 ()
line2D end = do
  (start, _) <- get
  appendPath2D (W.line2D start end)

lineTo2D :: V2 Double -> PathState2 ()
lineTo2D end = do
  (start, _) <- get
  appendSegment2D (W.lineTo2D start end)

lineRelative2D :: V2 Double -> PathState2 ()
lineRelative2D delta = do
  (start, _) <- get
  appendSegment2D (W.lineRelative2D delta start)

arcVia2D :: V2 Double -> V2 Double -> PathState2 ()
arcVia2D via end = do
  (start, _) <- get
  appendPath2D (W.arcVia2D start via end)

arcViaTo2D :: V2 Double -> V2 Double -> PathState2 ()
arcViaTo2D via end = do
  (start, _) <- get
  appendSegment2D (W.arcViaTo2D start via end)

arcViaRelative2D :: V2 Double -> V2 Double -> PathState2 ()
arcViaRelative2D viaDelta endDelta = do
  (start, _) <- get
  appendSegment2D (W.arcViaRelative2D start viaDelta endDelta)

bezier2D :: V2 Double -> V2 Double -> V2 Double -> PathState2 ()
bezier2D c1 c2 end = do
  (start, _) <- get
  appendPath2D (W.bezier2D start c1 c2 end)

bezierTo2D :: V2 Double -> V2 Double -> V2 Double -> PathState2 ()
bezierTo2D c1 c2 end = do
  (start, _) <- get
  appendSegment2D (W.bezierTo2D start c1 c2 end)

bezierRelative2D :: V2 Double -> V2 Double -> V2 Double -> PathState2 ()
bezierRelative2D c1 c2 endDelta = do
  (start, _) <- get
  appendSegment2D (W.bezierRelative2D start c1 c2 endDelta)

pathFrom2D :: [V2 Double -> (V2 Double, W.Path2D)] -> PathState2 ()
pathFrom2D steps = do
  (start, _) <- get
  appendPath2D (W.pathFrom2D start steps)

pathFromTo2D :: [V2 Double -> (V2 Double, W.Path2D)] -> PathState2 ()
pathFromTo2D steps = do
  (start, _) <- get
  appendSegment2D (W.pathFromTo2D steps start)

pathEndpoints2D :: PathState2 (Maybe (V2 Double, V2 Double))
pathEndpoints2D = gets (W.pathEndpoints2D . snd)

closeLoop2D :: PathState2 ()
closeLoop2D = modify (\(p, acc) -> (p, W.closeLoop2D acc))

reversePath2D :: PathState2 ()
reversePath2D = modify (\(p, acc) -> (p, W.reversePath2D acc))

splice2D :: V2 Double -> PathState2 ()
splice2D point = do
  (current, acc) <- get
  let (newPoint, newPath) = W.splice2D acc point
  put (newPoint, newPath)

splitPath2D :: PathState2 [W.Path2D]
splitPath2D = gets (W.splitPath2D . snd)

pathLength2D :: PathState2 Double
pathLength2D = gets (W.pathLength2D . snd)

takePathFraction2D :: Double -> PathState2 ()
takePathFraction2D fraction =
  modify (\(p, acc) -> (p, W.takePathFraction2D fraction acc))
