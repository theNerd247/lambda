module Data.Hylo where

data Fix f = Fix { unFix :: f (Fix f) }

type Alg f a = f a -> a

type CoAlg f a = a -> f a

ana :: (Functor f) => Alg f a -> Fix f -> a
ana alg = alg . fmap (ana alg) . unFix

cata :: (Functor f) => CoAlg f a -> a -> Fix f
cata coalg = Fix . fmap (cata coalg) . coalg

hylo :: (Functor f) => Alg f b -> CoAlg f a -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg

data L a = 
  L Int a
  | N Int
  deriving (Show)

instance Functor L where
  fmap f (L i a) = L i (f a)
  fmap f (N i) = N i
