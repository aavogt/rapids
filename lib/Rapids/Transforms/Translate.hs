{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Translate where

import Rapids.Transforms.Translate.Go
import qualified Waterfall as W

-- *** translate
-- **** 3D

-- |
-- > translate
-- >    x y z
-- >    v3
-- >    ex x
-- >    ey y
-- >    ez z
-- >  :: Transformable t => t -> t
--
-- t is Solid, V3 Double, Path
translate :: (TranslateGo t r) => r
translate = translateGo 0 W.translate

-- | @translated ... a = a + translate ... a@
--
-- doesn't make much with Transformable (V3 Double)
translated :: (Num t, TranslateGo t r) => r
translated = translateGo 0 \v x -> x + W.translate v x

-- | @_translated@ produces an 'Control.Lens.Iso.Iso' using the same arguments as 'translate'
_translated :: (TranslatedGo a b r) => r
_translated = translatedGo 0

-- | @_translated' ... = 'simple' (_translated ...) . 'simple'@ requires the forward and backward objects
-- to have the same type.
_translated' :: (TranslatedGo a a r) => r
_translated' = _translated

-- **** 2D
-- |
-- > translate2D
-- >    x y
-- >    (V2 x y)
-- >    ex x
-- >    ey y
-- >  :: Transformable2D t => t -> t
--
-- t is Shape, V2 Double, Path2D
translate2D :: (Translate2DGo t r) => r
translate2D = translate2DGo 0 W.translate2D

-- | @translated2D ... a = a + translate2D ... a@
translated2D :: (Num t, Translate2DGo t r) => r
translated2D = translate2DGo 0 \v x -> x + W.translate2D v x

-- | @_translated2D@ produces an 'Control.Lens.Iso.Iso' using the same arguments as 'translate2D'
_translated2D :: (Translated2DGo a b r) => r
_translated2D = translated2DGo 0

-- | @_translated2D' ... = 'simple' (_translated2D ...) . 'simple'@ requires the forward and backward objects
-- to have the same type.
_translated2D' :: (Translated2DGo a a r) => r
_translated2D' = _translated2D
