module Vector where

import           Data.Foldable (Foldable)

newtype Vec2D a =
  Vec2D (a, a)
  deriving (Eq)

instance Show a => Show (Vec2D a) where
  show (Vec2D (x, y)) = show (x, y)

vecX :: Vec2D a -> a
vecX (Vec2D t) = fst t

vecY :: Vec2D a -> a
vecY (Vec2D t) = snd t

liftVec :: (a -> a -> b) -> Vec2D a -> Vec2D a -> Vec2D b
liftVec f (Vec2D (x1, y1)) (Vec2D (x2, y2)) = Vec2D (f x1 x2, f y1 y2)

dot :: Num a => Vec2D a -> Vec2D a -> a
v1 `dot` v2 = sum $ liftVec (*) v1 v2

norm :: Floating a => Vec2D a -> a
norm v = sqrt . sum $ fmap (** 2) v

scalMult :: Num a => Vec2D a -> a -> Vec2D a
v `scalMult` a = fmap (* a) v

scalDiv :: Fractional a => Vec2D a -> a -> Vec2D a
v `scalDiv` a = fmap (/ a) v

instance Functor Vec2D where
  fmap f (Vec2D t) = Vec2D (mapTup f t)
    where
      mapTup f (a, b) = (f a, f b)

instance Foldable Vec2D where
  foldr f init (Vec2D (a, b)) = f b $ f a init
  foldr1 f (Vec2D (a, b)) = f b a

instance Num a => Num (Vec2D a) where
  v1 + v2 = liftVec (+) v1 v2
  v1 - v2 = liftVec (-) v1 v2
  (*) = undefined
  abs v = abs `fmap` v
  signum = undefined
  fromInteger v = undefined -- breaks here - why?

instance Fractional a => Fractional (Vec2D a) where
  fromRational = undefined
  (/) = undefined

instance Floating a => Floating (Vec2D a) where
  v1 ** v2 = undefined
  pi = undefined
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  logBase = undefined
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh
