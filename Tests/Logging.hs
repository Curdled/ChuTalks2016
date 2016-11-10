
import Data.Monoid

import Control.Monad.Writer

import Data.Array

import Control.Comonad

type Logger i o l = i -> (o, l)

addOne :: Logger Int Int [String]
addOne x = (x+1,["Added 1 to " ++ show x])

applyLogger :: Monoid l => (v,l) -> Logger v o l -> (o,l)
applyLogger (input,log) f = (o, log `mappend` l)
  where (o, l) = f input

logNum :: Integer  ->  Writer [String] Integer
logNum x = writer (x+1, ["Written " ++ (show x)])

logApplication :: (Show a, Show b) =>  (a -> b) -> a -> Writer [String] b
logApplication f input = writer(out, [(show input) ++ " applied to f gives " ++ (show out)])
  where out = f input

f x = x + 2

runThing =
  do x <- logApplication f 2
     y <- logNum 2
     tell ["done"]
     return (x*y)

class ContravariantFunctor f where
  contrafmap :: (b ->  a) -> f a -> f b


newtype Test a = Test { runTest :: a -> Bool }


instance ContravariantFunctor Test where
  contrafmap f (Test g) = Test (g . f)


isZero :: Test Int
isZero = Test (==0)

isEmpty :: Test [a]
isEmpty = contrafmap length isZero

result :: Bool
result = runTest isEmpty "Hello"

data CArray i a = CA (Array i a) i deriving Show

instance Ix i => Functor (CArray i) where
  fmap f = extend (f . extract)

data PArray i a = PA (Array i a) i deriving Show

instance Ix i => Comonad (CArray i) where
  extract (CA arr c) = arr!c
  extend f (CA x c)  =
    let  es' = map (\i -> (i, f (CA x i))) (indices x)
    in   CA (array (bounds x) es') c

laplace1D :: Num a => CArray Integer a -> a
laplace1D a = (a ? (-1)) + (a ? 1)  - 2 * (a ? 0)

(?) :: (Ix i, Num a, Num i) => CArray i a -> i -> a
(CA a i) ? i' = if (inRange (bounds a) (i+i')) then a ! (i+i') else 0

x :: CArray Integer Integer
x = CA (array (0,3) [(0,5),(1,7),(2,10),(3,-12)]) 2
(CA y i) = x

