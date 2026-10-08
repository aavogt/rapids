{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Translate where

import Rapids.Transforms.Translate.Go
import qualified Waterfall as W
import Data.Data

-- *** translate
-- **** 3D

-- |
-- > translate
-- >    x y z
-- >    v3
-- >    ex x
-- >    ey y
-- >    ez z
-- >  solid|v3|path
translate :: (TranslateGo t r) => r
translate = translateGo 0 W.translate

-- | @translated ... a = a + translate ... a@
--
-- doesn't make much with Transformable (V3 Double)
translated :: (Num t, TranslateGo t r) => r
translated = translateGo 0 \v x -> x + W.translate v x

-- | @_translated ... :: Iso a b a b@ where ... is as in @'translate' ...@
_translated :: (TranslatedGo a b False r) => r
_translated = translatedGo (Proxy @False) 0

-- | @_translated' ... :: Iso a a a a@ where ... is as in @'translate' ...@
_translated' :: (TranslatedGo a b True r) => r
_translated' = translatedGo (Proxy @True) 0

-- **** 2D
-- |
-- > translate2D
-- >    x y
-- >    v2
-- >    ex x
-- >    ey y
-- >  shape|v2|path2d
translate2D :: (Translate2DGo t r) => r
translate2D = translate2DGo 0 W.translate2D

-- | @translated2D ... a = a + translate2D ... a@
translated2D :: (Num t, Translate2DGo t r) => r
translated2D = translate2DGo 0 \v x -> x + W.translate2D v x

-- | @_translated2D ... :: Iso a b a b@ where ... is as in @'translate2D' ...@
_translated2D :: (Translated2DGo a b False r) => r
_translated2D = translated2DGo (Proxy @False) 0

-- | @_translated2D' ... :: Iso a a a a@ where ... is as in @'translate2D' ...@
-- to have the same type.
_translated2D' :: (Translated2DGo a b True r) => r
_translated2D' = translated2DGo (Proxy @True) 0
