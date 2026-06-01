{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

-- |  Waterfall suggests
--
-- 'pathFrom' :: Monoid path => point -> [point -> (point, path)] -> path 
--
-- here is another take on that with the transformers package "Control.Monad.Trans.State".'State' type
-- so that:
--
-- > pathFrom p0 [line3D v, line3D w]
--
-- > do
-- >     line3D v
-- >     line3D w
-- >  `execState` p0 & snd
module Rapids.Path
  ( module Rapids.Path,

    -- * reexports
    execState,
    zoom,
    _xy,
    _xz,
    _yz,
    _yx,
    _zx,
    _zy,
  )
where

import Control.Lens
import Control.Monad.Trans.State
import Linear
import qualified Waterfall.Path as W
import qualified Waterfall.TwoD.Path2D as W

type PathState = State (V3 Double, W.Path)

appendPath :: W.Path -> PathState ()
appendPath path = do
  (current, acc) <- get
  let newEnd = maybe current snd $ W.pathEndpoints3D path
  put (newEnd, acc <> path)

appendSegment :: (V3 Double, W.Path) -> PathState ()
appendSegment (newEnd, seg) = do
  (_, acc) <- get
  put (newEnd, acc <> seg)

line3D :: V3 Double -> PathState ()
line3D end = do
  (start, _) <- get
  appendPath (W.line3D end start)

lineTo3D :: V3 Double -> PathState ()
lineTo3D end = do
  (start, _) <- get
  appendSegment (W.lineTo3D end start)

lineRelative3D :: V3 Double -> PathState ()
lineRelative3D delta = do
  (start, _) <- get
  appendSegment (W.lineRelative3D delta start)

arcVia3D :: V3 Double -> V3 Double -> PathState ()
arcVia3D via end = do
  (start, _) <- get
  appendPath (W.arcVia3D via end start)

arcViaTo3D :: V3 Double -> V3 Double -> PathState ()
arcViaTo3D via end = do
  (start, _) <- get
  appendSegment (W.arcViaTo3D via end start)

arcViaRelative3D :: V3 Double -> V3 Double -> PathState ()
arcViaRelative3D viaDelta endDelta = do
  (start, _) <- get
  appendSegment (W.arcViaRelative3D viaDelta endDelta start)

bezier3D :: V3 Double -> V3 Double -> V3 Double -> PathState ()
bezier3D c1 c2 end = do
  (start, _) <- get
  appendPath (W.bezier3D c1 c2 end start)

bezierTo3D :: V3 Double -> V3 Double -> V3 Double -> PathState ()
bezierTo3D c1 c2 end = do
  (start, _) <- get
  appendSegment (W.bezierTo3D c1 c2 end start)

bezierRelative3D :: V3 Double -> V3 Double -> V3 Double -> PathState ()
bezierRelative3D c1 c2 endDelta = do
  (start, _) <- get
  appendSegment (W.bezierRelative3D c1 c2 endDelta start)

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
closeLoop3D = _2 %= W.closeLoop3D

reversePath3D :: PathState ()
reversePath3D = _2 %= W.reversePath3D

splice3D :: V3 Double -> PathState ()
splice3D pt = modify (\(_, acc) -> W.splice3D acc pt)

splitPath3D :: PathState [W.Path]
splitPath3D = gets (W.splitPath3D . snd)

pathLength3D :: PathState Double
pathLength3D = gets (W.pathLength3D . snd)

takePathFraction3D :: Double -> PathState ()
takePathFraction3D fraction = _2 %= W.takePathFraction3D fraction

type PathState2 = State (V2 Double, W.Path2D)

appendPath2D :: W.Path2D -> PathState2 ()
appendPath2D path = do
  (current, acc) <- get
  let newEnd = maybe current snd $ W.pathEndpoints2D path
  put (newEnd, acc <> path)

appendSegment2D :: (V2 Double, W.Path2D) -> PathState2 ()
appendSegment2D (newEnd, seg) = do
  (_, acc) <- get
  put (newEnd, acc <> seg)

arc :: W.Sense -> Double -> V2 Double -> PathState2 ()
arc sense radius end = do
  (start, _) <- get
  appendPath2D (W.arc sense radius end start)

arcTo :: W.Sense -> Double -> V2 Double -> PathState2 ()
arcTo sense radius end = do
  (start, _) <- get
  appendSegment2D (W.arcTo sense radius end start)

arcRelative :: W.Sense -> Double -> V2 Double -> PathState2 ()
arcRelative sense radius endDelta = do
  (start, _) <- get
  appendSegment2D (W.arcRelative sense radius endDelta start)

repeatLooping :: PathState2 ()
repeatLooping = _2 %= W.repeatLooping

line2D :: V2 Double -> PathState2 ()
line2D end = do
  (start, _) <- get
  appendPath2D (W.line2D end start)

lineTo2D :: V2 Double -> PathState2 ()
lineTo2D end = do
  (start, _) <- get
  appendSegment2D (W.lineTo2D end start)

lineRelative2D :: V2 Double -> PathState2 ()
lineRelative2D delta = do
  (start, _) <- get
  appendSegment2D (W.lineRelative2D delta start)

arcVia2D :: V2 Double -> V2 Double -> PathState2 ()
arcVia2D via end = do
  (start, _) <- get
  appendPath2D (W.arcVia2D via end start)

arcViaTo2D :: V2 Double -> V2 Double -> PathState2 ()
arcViaTo2D via end = do
  (start, _) <- get
  appendSegment2D (W.arcViaTo2D via end start)

arcViaRelative2D :: V2 Double -> V2 Double -> PathState2 ()
arcViaRelative2D viaDelta endDelta = do
  (start, _) <- get
  appendSegment2D (W.arcViaRelative2D viaDelta endDelta start)

bezier2D :: V2 Double -> V2 Double -> V2 Double -> PathState2 ()
bezier2D c1 c2 end = do
  (start, _) <- get
  appendPath2D (W.bezier2D c1 c2 end start)

bezierTo2D :: V2 Double -> V2 Double -> V2 Double -> PathState2 ()
bezierTo2D c1 c2 end = do
  (start, _) <- get
  appendSegment2D (W.bezierTo2D c1 c2 end start)

bezierRelative2D :: V2 Double -> V2 Double -> V2 Double -> PathState2 ()
bezierRelative2D c1 c2 endDelta = do
  (start, _) <- get
  appendSegment2D (W.bezierRelative2D c1 c2 endDelta start)

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
closeLoop2D = _2 %= W.closeLoop2D

reversePath2D :: PathState2 ()
reversePath2D = _2 %= W.reversePath2D

splice2D :: V2 Double -> PathState2 ()
splice2D pt = modify (\(_, acc) -> W.splice2D acc pt)

splitPath2D :: PathState2 [W.Path2D]
splitPath2D = gets (W.splitPath2D . snd)

pathLength2D :: PathState2 Double
pathLength2D = gets (W.pathLength2D . snd)

takePathFraction2D :: Double -> PathState2 ()
takePathFraction2D fraction = _2 %= W.takePathFraction2D fraction
