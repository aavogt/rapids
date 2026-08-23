module Rapids.Translate where
import Control.Lens hiding (prism)
import Linear
import Rapids.Color
import Waterfall (Transformable2D)
import qualified Waterfall as W

-- * interface

-- |
-- > translate
-- >    x y z
-- >    (V3 x y z)
-- >    ex x
-- >    ey y
-- >    ez z
-- >  :: Transformable t => t -> t
--
-- t is Solid, V3 Double, Path
translate :: (TranslateGo r t) => r
translate = translateGo False (id :: t -> t)

-- | 'translate' except it also returns the original
--
-- doesn't make much sense for V3 Double
translated :: (TranslateGo r t) => r
translated = translateGo True (id :: t -> t)

-- | '_translated' is 'translate' returning an 'Iso''
_translated :: TranslatedGo r t => r
_translated = translatedGo id id

-- |
-- > translate2D
-- >    x y
-- >    (V2 x y)
-- >    ex x
-- >    ey y
-- >  :: Transformable2D t => t -> t
--
-- t is Shape, V2 Double, Path2D
translate2D :: (Translate2DGo r t) => r
translate2D = translate2DGo False (id :: t -> t)

translated2D :: (Translate2DGo r t) => r
translated2D = translate2DGo True (id :: t -> t)

-- | '_translated2D' is 'translate2D' returning an 'Iso''
_translated2D :: TranslatedGo r t => r
_translated2D = translatedGo id id

-- * implementation

class W.Transformable t => TranslateGo r t | r -> t where
  translateGo :: Bool -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Num t, PropagateColor a, a' ~ a, a ~ t) => TranslateGo (a -> a') t where
  translateGo False acc x = propagateColor acc x
  translateGo True acc x = x + propagateColor acc x
  -- should intermediates be kept too?

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, TranslateGo r t) => TranslateGo (d -> e -> f -> r) t where
  translateGo keep acc x y z = translateGo keep (acc . W.translate (V3 x y z))

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, TranslateGo r t) => TranslateGo (E v -> amt -> r) t where
  translateGo keep acc (E e) amt = translateGo keep (acc . W.translate (0 & e .~ amt))

instance {-# OVERLAPPABLE #-} (v ~ Double, TranslateGo r t) => TranslateGo (V3 v -> r) t where
  translateGo keep acc v = translateGo keep (acc . W.translate v)

class W.Transformable t => TranslatedGo r t | r -> t where
  translatedGo :: (t -> t) -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, a' ~ a, a ~ t) => TranslatedGo (Optic' p g a a') t where
  translatedGo forward backward = iso (propagateColor forward :: a' -> a) (propagateColor backward :: a -> a')

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, TranslatedGo r t) => TranslatedGo (d -> e -> f -> r) t where
  translatedGo forward backward x y z =
    let translation = W.translate (V3 x y z)
        inverse = W.translate (V3 (-x) (-y) (-z))
     in translatedGo (forward . translation) (inverse . backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, TranslatedGo r t) => TranslatedGo (V3 d -> r) t where
  translatedGo forward backward v =
    let translation = W.translate v
        inverse = W.translate (-v)
     in translatedGo (forward . translation) (inverse . backward)

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, TranslatedGo r t) => TranslatedGo (E v -> amt -> r) t where
  translatedGo forward backward (E e) amt =
    let translation = W.translate (0 & e .~ amt)
        inverse = W.translate (0 & e .~ -amt)
     in translatedGo (forward . translation) (inverse . backward)

class W.Transformable2D t => Translate2DGo r t | r -> t where
  translate2DGo :: Bool -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Num t, Transformable2D a, a' ~ a, a ~ t) => Translate2DGo (a -> a') t where
  translate2DGo keep acc a = if keep then a + acc a else acc a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, Translate2DGo r t) => Translate2DGo (d -> e -> r) t where
  translate2DGo keep acc x y = translate2DGo keep (acc . W.translate2D (V2 x y))

instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Translate2DGo r t) => Translate2DGo (E v -> amt -> r) t where
  translate2DGo keep acc (E e) amt = translate2DGo keep (acc . W.translate2D (0 & e .~ amt))

instance {-# OVERLAPPABLE #-} (v ~ Double, Translate2DGo r t) => Translate2DGo (V2 v -> r) t where
  translate2DGo keep acc v = translate2DGo keep (acc . W.translate2D v)

class W.Transformable2D t => Translated2DGo r t | r -> t where
  translated2DGo :: (t -> t) -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Transformable2D t, Profunctor p, Functor g, a' ~ a, a ~ t) => Translated2DGo (Optic' p g a a') t where
  translated2DGo forward backward = iso (forward :: a' -> a) (backward :: a -> a')

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, Translated2DGo r t) => Translated2DGo (d -> e -> r) t where
  translated2DGo forward backward x y =
    let translation = W.translate2D (V2 x y)
        inverse = W.translate2D (V2 (-x) (-y))
     in translated2DGo (forward . translation) (inverse . backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, Translated2DGo r t) => Translated2DGo (V2 d -> r) t where
  translated2DGo forward backward v =
    let translation = W.translate2D v
        inverse = W.translate2D (-v)
     in translated2DGo (forward . translation) (inverse . backward)

instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Translated2DGo r t) => Translated2DGo (E v -> amt -> r) t where
  translated2DGo forward backward (E e) amt =
    let translation = W.translate2D (0 & e .~ amt)
        inverse = W.translate2D (0 & e .~ -amt)
     in translated2DGo (forward . translation) (inverse . backward)
