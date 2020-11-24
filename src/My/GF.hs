{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module My.GF (
  TPoly(..),
  GF(gfPoly),
  fromCoefs,
) where

import My.Arithmetic

import Data.Euclidean (GcdDomain, Euclidean(..))
import Data.Field (Field)
import Data.Poly (VPoly)
import Data.Proxy
import Data.Ratio (numerator, denominator)
import Data.Semiring (Semiring(..), Ring(..))
import GHC.TypeLits (Nat, KnownNat(..), natVal)

import qualified Data.Vector as Vector
import qualified Data.Poly as P
import qualified Data.Euclidean as Euclidean


newtype TPoly (q :: Nat) (p :: [Nat]) = TPoly { getTPoly :: [PrimeFinite q] }


class KnownNat q => KnownPoly (q :: Nat) (p :: [Nat]) where
  polySing :: TPoly q p


instance KnownNat q => KnownPoly q '[] where
  polySing = TPoly []


instance forall q h t. (KnownNat q, KnownNat h, KnownPoly q t) => KnownPoly q (h:t) where
  polySing :: TPoly q (h:t)
  polySing = TPoly (fromInteger hh : tt)
    where
      hh = natVal (Proxy :: Proxy h)
      tt = getTPoly (polySing :: TPoly q t)


polyVal :: forall q p proxy. (KnownPoly q p) => proxy p -> VPoly (PrimeFinite q)
polyVal _ = P.toPoly $ Vector.fromList $ getTPoly (polySing :: TPoly q p)


newtype GF (q :: Nat) (ip :: [Nat]) = GF { gfPoly :: VPoly (PrimeFinite q) }
  deriving Eq


fromCoefs :: KnownNat q => [PrimeFinite q] -> GF q ip
fromCoefs = GF . P.toPoly . Vector.fromList


instance (KnownNat q, KnownPoly q ip) => Num (GF q ip) where
  (GF a) + (GF b) = GF $ (a + b) `Euclidean.rem` polyVal (Proxy :: Proxy ip)
  (GF a) * (GF b) = GF $ (a * b) `Euclidean.rem` polyVal (Proxy :: Proxy ip)
  abs = id
  signum = id
  fromInteger = fromCoefs . (:[]) . fromInteger
  negate = GF . P.toPoly . Vector.map Prelude.negate . P.unPoly . gfPoly


instance Show (GF q ip) where
  show = show . gfPoly


instance forall q ip. (KnownNat q, KnownPoly q ip) => Fractional (GF q ip) where
  recip x = x ^ (ord - 2)
    where
      ord = natVal (Proxy :: Proxy q) ^ n
      n = Vector.length $ P.unPoly (polyVal (Proxy :: Proxy ip) :: VPoly (PrimeFinite q))

  fromRational q = fromInteger (numerator q) Prelude./ fromInteger (denominator q)


instance (KnownNat q, KnownPoly q ip) => Additive (GF q ip) where
  (.+.) = (+)
  (.-.) = (-)


instance (KnownNat q, KnownPoly q ip) => Semiring (GF q ip) where
  plus = (+)
  times = (*)
  fromNatural = fromInteger . toInteger


instance (KnownNat q, KnownPoly q ip) => GcdDomain (GF q ip)


instance (KnownNat q, KnownPoly q ip) => Euclidean (GF q ip) where
  a `quotRem` b = (a / b, 0)
  degree = fromIntegral . Vector.length . P.unPoly . gfPoly


instance (KnownNat q, KnownPoly q ip) => Ring (GF q ip) where
  negate = Prelude.negate


instance (KnownNat q, KnownPoly q ip) => Field (GF q ip)
