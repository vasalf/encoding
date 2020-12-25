{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module My.Arithmetic (
  Binary(..),
  Additive(..),
  (.*.),
  PrimeFinite,
) where

import Control.Monad.Random (Random(..))
import Data.Bifunctor (first)
import Data.Euclidean (GcdDomain, Euclidean(..))
import Data.Field (Field(..))
import Data.Ratio (numerator, denominator)
import Data.Semiring (Semiring(..), Ring(..))
import GHC.TypeLits (Nat, KnownNat(..))
import My.TypeUtils (natValue)

import qualified Data.Vector as V


class Additive a where
  infixl 6 .+.
  infixl 6 .-.
  (.+.) :: a -> a -> a
  (.-.) :: a -> a -> a


instance Num a => Additive (V.Vector a) where
  (.+.) = V.zipWith (+)
  (.-.) = V.zipWith (-)


infixl 7 .*.
(.*.) :: Num a => a -> V.Vector a -> V.Vector a
(.*.) a = V.map (a *)


newtype PrimeFinite (n :: Nat) = PrimeFinite Integer
  deriving (Eq, Ord, Enum)


instance forall n. KnownNat n => Num (PrimeFinite n) where
  (PrimeFinite a) + (PrimeFinite b) = PrimeFinite $ (a + b) `mod` natValue @n
  (PrimeFinite a) * (PrimeFinite b) = PrimeFinite $ (a * b) `mod` natValue @n

  abs = id
  signum = id

  fromInteger = PrimeFinite . (`mod` natValue @n)

  negate (PrimeFinite 0) = PrimeFinite 0
  negate (PrimeFinite x) = PrimeFinite $ natValue @n - x


instance Show (PrimeFinite n) where
  show (PrimeFinite x) = show x


instance KnownNat n => Read (PrimeFinite n) where
  readsPrec p = map (first fromInteger) . readsPrec p


instance KnownNat n => Real (PrimeFinite n) where 
  toRational (PrimeFinite x) = toRational x


instance forall n. KnownNat n => Fractional (PrimeFinite n) where
  recip (PrimeFinite 0) = error "zero division"
  recip q = q ^ (natValue @n - 2)

  fromRational q = fromInteger (numerator q) Prelude./ fromInteger (denominator q)


instance KnownNat n => Integral (PrimeFinite n) where
  x `quotRem` y = (x Prelude./ y, 0)
  toInteger (PrimeFinite x) = x


instance forall n. KnownNat n => Random (PrimeFinite n) where
  randomR (l, r) g = (fromInteger x, g')
    where
      (x, g') = randomR (toInteger l, toInteger r) g

  random = randomR (0, fromInteger $ natValue @n - 1)


instance KnownNat n => Additive (PrimeFinite n) where
  (.+.) = (+)
  (.-.) = (-)


instance KnownNat n => Semiring (PrimeFinite n) where
  plus = (+)
  times = (*)
  fromNatural = fromInteger . toInteger


instance KnownNat n => GcdDomain (PrimeFinite n)


instance KnownNat n => Euclidean (PrimeFinite n) where
  quotRem = Prelude.quotRem
  degree = fromIntegral


instance KnownNat n => Ring (PrimeFinite n) where
  negate = Prelude.negate


instance KnownNat n => Field (PrimeFinite n)


type Binary = PrimeFinite 2
