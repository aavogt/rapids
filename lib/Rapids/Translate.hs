module Rapids.Translate where
import Control.Lens hiding (prism)
import Linear
import Rapids.Color
import Waterfall (Transformable2D)
import qualified Waterfall as W

-- | Translate a 'Transformable' ( 'Path'/'Solid'/'V3' Double) in one or more directions.
class Translate r where
  -- | @translate@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > translate x y z
  -- > translate (v :: V3 Double)
  -- > translate ex x
  -- > translate ey y
  -- > translate ez z
  translate :: r

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, TranslateGo r t) => Translate (d -> e -> f -> r) where
  translate x y z = translateGo (W.translate (V3 x y z))

instance {-# INCOHERENT #-} (d ~ Double, PropagateColor a, a ~ a') => Translate (V3 d -> a -> a') where
  translate v a = propagateColor (W.translate v) a

class W.Transformable t => TranslateGo r t | r -> t where
  translateGo :: (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (PropagateColor a, a' ~ a, a ~ t) => TranslateGo (a -> a') t where
  translateGo acc = propagateColor acc
instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, TranslateGo r t) => TranslateGo (d -> e -> f -> r) t where
  translateGo acc x y z = translateGo (acc . W.translate (V3 x y z))

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, TranslateGo r t) => TranslateGo (E v -> amt -> r) t where
  translateGo acc (E e) amt = translateGo (acc . W.translate (0 & e .~ amt))

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, TranslateGo r t) => Translate (E v -> amt -> r) where
  translate (E e) amt = translateGo (W.translate (0 & e .~ amt))



-- | Translate a 'Transformable' in one or more directions through an 'Iso'.
class Translated r where
  _translated :: r

instance {-# OVERLAPPABLE #-} (TranslatedGo r t) => Translated r where
  _translated = translatedGo id id

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

class Translate2D a where
  translate2D :: a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, Transformable2D a, a' ~ a) => Translate2D (d -> e -> a -> a') where
  translate2D x y a = W.translate2D (V2 x y) a

instance {-# OVERLAPPABLE #-} (d ~ Double, Transformable2D a, a ~ a') => Translate2D (V2 d -> a -> a') where
  translate2D v a = W.translate2D v a

-- | Linear defines 'ex' 'ey' 'ez'
--
-- > transform 'ex' 3 solid
instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Translate2D (E v -> amt -> a -> a') where
  translate2D (E e) amt a = W.translate2D (0 & e .~ amt) a

class Translated2D a where
  _translated2D :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, d ~ Double, e ~ Double, Transformable2D a, a' ~ a) => Translated2D (d -> e -> Optic' p g a a') where
  _translated2D x y = iso (translate2D x y :: a' -> a) (translate2D (-x) (-y) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, Transformable2D a, a ~ a') => Translated2D (V2 d -> Optic' p g a a') where
  _translated2D v = iso (translate2D v :: a' -> a) (translate2D (-v) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Translated2D (E v -> amt -> Optic' p g a a') where
  _translated2D (E e) amt = iso (translate2D (0 & e .~ amt) :: a' -> a) (translate2D (0 & e .~ -amt) :: a -> a')
