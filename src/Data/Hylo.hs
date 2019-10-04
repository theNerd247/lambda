module Data.Hylo where

data Fix f = Fix { unFix :: f (Fix f) }

type Alg f a = f a -> a

type CoAlg f a = a -> f a

cata :: (Functor f) => Alg f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: (Functor f) => CoAlg f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: (Functor f) => Alg f b -> CoAlg f a -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg
