module Rapids.Transforms.Translate.Go where

import Control.Lens hiding (prism)
import Data.Data
import Linear
import Rapids.Color
import Waterfall (Transformable2D)
import qualified Waterfall as W

type family TypesEq (eq :: Bool) a b where
  TypesEq True a b = a ~ b
  TypesEq _ a b = ()

class TranslatedGo a b (c :: Bool) r | r -> a b where
  translatedGo :: Proxy c -> V3 Double -> r

instance {-# INCOHERENT #-} (TypesEq c a b, s ~ a, t ~ b, Profunctor p, Functor g, PropagateColor a, PropagateColor b) => TranslatedGo a b c (Optic p g s t a b) where
  translatedGo _ v = iso (propagateColor (W.translate v)) (propagateColor (W.translate (-v)))

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, TranslatedGo a b c r) => TranslatedGo a b c (d -> e -> f -> r) where
  translatedGo c v x y z = translatedGo c (v + V3 x y z)

instance {-# OVERLAPS #-} (d ~ Double, TranslatedGo a b c r) => TranslatedGo a b c (V3 d -> r) where
  translatedGo c v w = translatedGo c (v + w)

instance {-# OVERLAPS #-} (v ~ V3, amt ~ Double, TranslatedGo a b c r) => TranslatedGo a b c (E v -> amt -> r) where
  translatedGo c v (E e) amount = translatedGo c (v & e +~ amount)

class Translated2DGo a b (c :: Bool) r | r -> a b where
  translated2DGo :: Proxy c -> V2 Double -> r

instance {-# INCOHERENT #-} (Profunctor p, Functor g, Transformable2D a, Transformable2D b, TypesEq c a b, s ~ a, t ~ b) => Translated2DGo a b c (Optic p g s t a b) where
  translated2DGo _ v = iso (W.translate2D v) (W.translate2D (-v))

instance {-# OVERLAPS #-} (v ~ V2, amt ~ Double, Translated2DGo a b c r) => Translated2DGo a b c (E v -> amt -> r) where
  translated2DGo c v (E e) amount = translated2DGo c (v & e +~ amount)

instance {-# OVERLAPS #-} (Double ~ d, Translated2DGo a b c r) => Translated2DGo a b c (V2 d -> r) where
  translated2DGo c v w = translated2DGo c (v + w)

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
