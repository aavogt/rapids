module Rapids.Transforms.Translate.Go where

import Control.Lens hiding (prism)
import Linear
import Rapids.Color
import Waterfall (Transformable2D)
import qualified Waterfall as W

class TranslatedGo a b r | r -> a b where
  translatedGo :: V3 Double -> r

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, PropagateColor b) => TranslatedGo a b (Optic p g a b a b) where
  translatedGo v = iso (propagateColor (W.translate v)) (propagateColor (W.translate (-v)))

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, TranslatedGo a b r) => TranslatedGo a b (d -> e -> f -> r) where
  translatedGo v x y z = translatedGo (v + V3 x y z)

instance {-# OVERLAPPABLE #-} (d ~ Double, TranslatedGo a b r) => TranslatedGo a b (V3 d -> r) where
  translatedGo v w = translatedGo (v+w)

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, TranslatedGo a b r) => TranslatedGo a b (E v -> amt -> r) where
  translatedGo v (E e) amount = translatedGo (v & e +~ amount)

class Translated2DGo a b r | r -> a b where
  translated2DGo :: V2 Double -> r

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, Transformable2D a, Transformable2D b) => Translated2DGo a b (Optic p g a b a b) where
  translated2DGo v = iso (W.translate2D v) (W.translate2D (-v))

instance {-# OVERLAPPING #-} (v ~ V2, amt ~ Double, Translated2DGo a b r) => Translated2DGo a b (E v -> amt -> r) where
  translated2DGo v (E e) amount = translated2DGo (v & e +~ amount)

instance {-# OVERLAPPABLE #-} (Double ~ d, Translated2DGo a b r) => Translated2DGo a b (V2 d -> r) where
  translated2DGo v w = translated2DGo (v + w)

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, Translated2DGo a b r) => Translated2DGo a b (d -> e -> r) where
  translated2DGo v x y = translated2DGo (v + V2 x y)

class (W.Transformable t) => TranslateGo t r | r -> t where
  translateGo :: V3 Double -> (V3 Double -> t -> t) -> r

instance {-# OVERLAPPABLE #-} (Num t, PropagateColor a, a' ~ a, a ~ t) => TranslateGo t (a -> a') where
  translateGo v f x = propagateColor (f v) x

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, TranslateGo t r) => TranslateGo t (d -> e -> f -> r) where
  translateGo v f x y z = translateGo (v + V3 x y z) f

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, TranslateGo t r) => TranslateGo t (E v -> amt -> r) where
  translateGo v f (E e) amt = translateGo (v & e +~ amt) f

instance {-# OVERLAPPABLE #-} (v ~ Double, TranslateGo t r) => TranslateGo t (V3 v -> r) where
  translateGo v f w = translateGo (v + w) f

class (W.Transformable2D t) => Translate2DGo t r | r -> t where
  translate2DGo :: V2 Double -> (V2 Double -> t -> t) -> r

instance {-# OVERLAPPABLE #-} (Num t, Transformable2D a, a' ~ a, a ~ t) => Translate2DGo t (a -> a') where
  translate2DGo v f a = f v a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, Translate2DGo t r) => Translate2DGo t (d -> e -> r) where
  translate2DGo v f x y = translate2DGo (v + V2 x y) f

instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Translate2DGo t r) => Translate2DGo t (E v -> amt -> r) where
  translate2DGo v f (E e) amt = translate2DGo (v & e +~ amt) f

instance {-# OVERLAPPABLE #-} (v ~ Double, Translate2DGo t r) => Translate2DGo t (V2 v -> r) where
  translate2DGo v f w = translate2DGo (v + w) f
