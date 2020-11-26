{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module My.GF (
  GF(gfPoly),
  fromCoefs,
  ord,
  primitive,
  ElemReprs(..),
  elemsReprs,
  zechTable,
  ordTable,
) where

import My.Arithmetic

import Control.Monad (join)
import Data.Euclidean (GcdDomain, Euclidean(..))
import Data.Field (Field)
import Data.List (sortBy)
import Data.Poly (VPoly)
import Data.Ratio (numerator, denominator)
import Data.Semiring (Semiring(..), Ring(..))
import GHC.TypeLits (Nat, KnownNat(..))
import My.TypeUtils (natValue)

import qualified Data.Euclidean as Euclidean
import qualified Data.Map.Strict as Map
import qualified Data.Poly as P
import qualified Data.Vector as Vector


class KnownNat q => KnownPoly (q :: Nat) (p :: [Nat]) where
  polySing :: [PrimeFinite q]


instance KnownNat q => KnownPoly q '[] where
  polySing = []


instance forall q h t. (KnownNat q, KnownNat h, KnownPoly q t) => KnownPoly q (h:t) where
  polySing = fromInteger (natValue @h) : polySing @q @t


polyVal :: forall q p. (KnownPoly q p) => VPoly (PrimeFinite q)
polyVal = P.toPoly $ Vector.fromList $ polySing @q @p


polyDeg :: forall q p. (KnownPoly q p) => Integer
polyDeg = fromIntegral (length $ polySing @q @p) - 1


newtype GF (q :: Nat) (ip :: [Nat]) = GF { gfPoly :: VPoly (PrimeFinite q) }
  deriving (Eq, Ord)


fromCoefs :: forall q ip. (KnownNat q, KnownPoly q ip) => [PrimeFinite q] -> GF q ip
fromCoefs = GF . (`Euclidean.rem` polyVal @q @ip) . P.toPoly . Vector.fromList


instance (KnownNat q, KnownPoly q ip) => Num (GF q ip) where
  (GF a) + (GF b) = GF $ (a + b) `Euclidean.rem` polyVal @q @ip
  (GF a) * (GF b) = GF $ (a * b) `Euclidean.rem` polyVal @q @ip
  abs = id
  signum = id
  fromInteger = fromCoefs . (:[]) . fromInteger
  negate = GF . P.toPoly . Vector.map Prelude.negate . P.unPoly . gfPoly


instance Show (GF q ip) where
  show = show . gfPoly


instance forall q ip. (KnownNat q, KnownPoly q ip) => Fractional (GF q ip) where
  recip x = x ^ (ord - 2)
    where
      ord = natValue @q ^ n
      n = polyDeg @q @ip

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


elems :: forall q ip. (KnownNat q, KnownPoly q ip) => [GF q ip]
elems = map fromCoefs (go n [[]])
  where
    n = polyDeg @q @ip
    qs = map fromInteger [0..natValue @q - 1]

    go 0 acc = acc
    go i acc = go (i - 1) (concatMap (\f -> map (:f) qs) acc)


ord :: forall q ip. (KnownNat q, KnownPoly q ip) => GF q ip -> Int
ord x = go 1 x
  where
    go n y | y == 1 = n
    go n y = go (n + 1) (y * x)


primitive :: forall q ip. (KnownNat q, KnownPoly q ip) => GF q ip
primitive = head $ filter ((== n - 1) . ord) $ tail elems
  where
    n = fromInteger $ natValue @q ^ polyDeg @q @ip


rootDegrees :: forall q ip. (KnownNat q, KnownPoly q ip) => Map.Map (GF q ip) Int
rootDegrees = go 0 1 Map.empty
  where
    x = primitive @q @ip
    n = fromInteger $ natValue @q ^ polyDeg @q @ip
    go i p acc
      | i == n - 1 = acc
      | otherwise  = go (i + 1) (p * x) (Map.insert p i acc)


data ElemReprs (q :: Nat) (ip :: [Nat]) = ElemReprs {
  polyRepr :: GF q ip,
  vectorRepr :: Vector.Vector (PrimeFinite q),
  indexRepr :: Maybe Int
} deriving Show


elemsReprs :: forall q ip. (KnownNat q, KnownPoly q ip) => [ElemReprs q ip]
elemsReprs = map (\g -> ElemReprs g (mkVector g) (mkIndex g)) elems
  where
    n = fromInteger $ polyDeg @q @ip

    getDeg p i
      | i < d = pp Vector.! i
      | otherwise = 0
        where
          pp = P.unPoly $ gfPoly p
          d = Vector.length pp

    mkVector p = Vector.generate n (getDeg p)

    rd = rootDegrees @q @ip
    mkIndex g = g `Map.lookup` rd


byIndex :: forall q ip. (KnownNat q, KnownPoly q ip) => Vector.Vector (GF q ip)
byIndex = Vector.fromList $ map polyRepr $ sortBy cmp $ tail $ elemsReprs @q @ip
  where
    cmp x y = compare (indexRepr x) (indexRepr y)



zechTable :: forall q ip. (KnownNat q, KnownPoly q ip) => Vector.Vector (Maybe Int)
zechTable = Vector.fromList [ join ((x + 1) `Map.lookup` idxs) | x <- xs ]
  where
    xs = Vector.toList $ byIndex @q @ip
    idxs = Map.fromList [ (p, i) | ElemReprs p _ i <- elemsReprs @q @ip ]


ordTable :: forall q ip. (KnownNat q, KnownPoly q ip) => Vector.Vector Int
ordTable = Vector.fromList $ 1:[ lcm i n `div` i | i <- [1..n - 1] ]
  where
    n = fromInteger $ natValue @q ^ polyDeg @q @ip - 1
