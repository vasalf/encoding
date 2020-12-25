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
  ElemsReprsTex(..),
  zechTable,
  ordTable,
  GFElemProps(..),
  elemsProps,
  GFElemsPropsTex(..),
  minimalPoly,
  minimalPolyTable,
  rootsTable,
  GFRootsTex(..),
  gfRootsTex,
  CyclotomicClass(..),
  cyclotomicClasses,
  CyclotomicClassesTex(..),
  Subfield(..),
  subfields,
  SubfieldsTex(..),
) where

import My.Arithmetic

import Control.Monad (join, unless)
import Data.Euclidean (GcdDomain, Euclidean(..))
import Data.Field (Field)
import Data.List (sortBy, nub)
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


showPoly :: (Eq p, Num p, Show p) => VPoly p -> ShowS
showPoly p | p == 0 = showString "$0$"
showPoly p = showString "$" . go . showString "$"
  where
    cs = reverse $ Vector.toList $ P.unPoly p
    n  = length cs - 1
    go = fst $ foldl comb (id, n) cs

    comb (f, i) c
      | c == 0    = (f, i - 1)
      | i == 0    = (f . showPlus i . shows c, i - 1)
      | c == 1    = (f . showPlus i . showXDeg i, i - 1)
      | otherwise = (f . showPlus i . shows c . showXDeg i, i - 1)

    showPlus i
      | i == n     = id
      | otherwise  = showChar '+'

    showXDeg i
      | i == 1 = showString "x"
      | otherwise = showString "x^{" . shows i . showChar '}'


newtype ElemsReprsTex q ip = ElemsReprsTex [ElemReprs q ip]


instance (KnownNat q, KnownPoly q ip) => Show (ElemsReprsTex q ip) where
  showsPrec _ (ElemsReprsTex ps) =
    showString "Многочлен & Вектор & Степень $\\alpha$ \\\\ \\hline \\hline \n" .
    foldr ((.) . showR) id ps
      where
        showR r =
          showPoly (gfPoly $ polyRepr r) . showString " & " .
          showVec  (vectorRepr r)        . showString " & " .
          showMaybe al (indexRepr r)     . showString " \\\\ \\hline \n"

        showMaybe _ Nothing = showString "—"
        showMaybe f (Just i) = showChar '$' . f (shows i) . showChar '$'

        showVec v = showString "\\texttt{" . Vector.foldr ((.) . shows) id (Vector.reverse v) . showChar '}'

        al f = showString "\\alpha^{" . f . showChar '}'


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


data GFElemProps q ip = GFElemProps {
  propElem :: GF q ip,
  propIndex :: Maybe Int,
  propOrd :: Maybe Int,
  propZech :: Maybe Int,
  propInvAdd :: Maybe Int,
  propInvMult :: Maybe Int
}


elemsProps :: forall q ip. (KnownNat q, KnownPoly q ip) => [GFElemProps q ip]
elemsProps = zp : op
  where
    zp = GFElemProps 0 Nothing Nothing (Just 0) Nothing Nothing
    op = map ip [0..n - 2]
    n  = fromInteger $ natValue @q ^ polyDeg @q @ip
    bi = byIndex @q @ip
    ot = ordTable @q @ip
    zt = zechTable @q @ip
    rd = rootDegrees @q @ip
    ip i = GFElemProps x (Just i) o z ia im
      where
        x  = bi Vector.! i
        o  = Just $ ot Vector.! i
        z  = zt Vector.! i
        ia = (-x) `Map.lookup` rd
        im = recip x `Map.lookup` rd


newtype GFElemsPropsTex q ip = GFElemsPropsTex [GFElemProps q ip]


instance (KnownNat q, KnownPoly q ip) => Show (GFElemsPropsTex q ip) where
  showsPrec _ (GFElemsPropsTex ps) =
    showString "Элемент $x$ & Индекс $i$ & $\\ord x$ & $Z(i)$ & $-x$ & $x^{-1}$" .
    showString "\\\\ \\hline \\hline\n" .
    foldr ((.) . showP) id ps
      where
        showP p =
          showPoly (gfPoly (propElem p)) . showString " & " .
          showMaybe "—" id (propIndex p)     . showString " & " .
          showMaybe "—" id (propOrd p)       . showString " & " .
          showMaybe "—" id (propZech p)      . showString " & " .
          showMaybe "0" al (propInvAdd p)    . showString " & " .
          showMaybe "—" al (propInvMult p)   . showString " \\\\ \\hline\n"

        showMaybe d _ Nothing = showString d
        showMaybe _ f (Just i) = showChar '$' . f (shows i) . showChar '$'

        al f = showString "\\alpha^{" . f . showChar '}'


irreduciblePolys :: forall q. KnownNat q => [P.VPoly (PrimeFinite q)]
irreduciblePolys = go polys
  where
    qv = natValue @q
    constPolys = [ P.toPoly (Vector.fromList [fromInteger p]) | p <- [0..qv - 1] ]
    polys = constPolys ++ [ P.X * p + r | p <- polys, r <- constPolys ]
    go (p:ps) | Vector.length (P.unPoly p) < 2 = go ps
    go (p:ps) = p : filter ((/= 0) . (`Euclidean.rem` p)) (go ps)


minimalPolyGF :: forall q ip. (KnownNat q, KnownPoly q ip) => GF q ip -> P.VPoly (GF q ip)
minimalPolyGF g = head $ filter ((== 0) . (`P.eval` g)) ips
  where
    ips = map (P.toPoly . Vector.map (fromCoefs @q @ip . (:[])) . P.unPoly) (irreduciblePolys @q)


minimalPoly :: forall q ip. (KnownNat q, KnownPoly q ip) => GF q ip -> P.VPoly (PrimeFinite q)
minimalPoly = P.toPoly . Vector.map (freeCoef . P.unPoly . gfPoly) . P.unPoly . minimalPolyGF
  where
    freeCoef v | Vector.null v = 0
    freeCoef v = v Vector.! 0


minimalPolyTable :: forall q ip. (KnownNat q, KnownPoly q ip) => Vector.Vector (P.VPoly (PrimeFinite q))
minimalPolyTable = Vector.map (minimalPoly @q @ip) byIndex


rootsTable :: forall q ip. (KnownNat q, KnownPoly q ip) => Int -> Vector.Vector [Int]
rootsTable d = Vector.generate n root
  where
    n = fromInteger $ natValue @q ^ polyDeg @q @ip - 1
    root i = filter ((== i) . (`mod` n) . (* d)) [0..n - 1]


gfRootsTex :: forall q ip. (KnownNat q, KnownPoly q ip) => [Int] -> GFRootsTex
gfRootsTex = GFRootsTex <*> foldr addR ini
  where
    n = fromInteger $ natValue @q ^ polyDeg @q @ip - 1
    ini = (Nothing, []) : [ (Just i, []) | i <- [0..n - 1] ]
    addR i es = map (addRoot (rootsTable @q @ip i)) es
    addRoot _ (Nothing, rs) = (Nothing, [Nothing] : rs)
    addRoot v (Just i, rs) = (Just i, map Just (v Vector.! i) : rs)


data GFRootsTex = GFRootsTex [Int] [(Maybe Int, [[Maybe Int]])]


instance Show GFRootsTex where
  showsPrec _ (GFRootsTex ds rs) =
    showString "$x$" . foldr ((.) . showD) id ds . showString " \\\\ \\hline \\hline\n" .
    foldr ((.) . showR) id rs
      where
        showD d = showString " & $\\sqrt[" . shows d . showString "]{x}$"

        showR (x, rs) =
          showMaybe al x . foldr ((.) . showR') id rs . showString " \\\\ \\hline\n"

        showR' []     = showString " & —"
        showR' (r:rs) =
          showString " & " .
          showMaybe al r .
          foldr ((.) . showR'') id rs

        showR'' r = showString ", " . showMaybe al r

        showMaybe _ Nothing = showString "0"
        showMaybe f (Just i) = showChar '$' . f (shows i) . showChar '$'

        al f = showString "\\alpha^{" . f . showChar '}'


cyclotomicIndices :: forall q ip. (KnownNat q, KnownPoly q ip) => [[Int]]
cyclotomicIndices = go [0..n - 1]
  where
    qv = fromInteger $ natValue @q
    n = qv ^ polyDeg @q @ip - 1
    nxt x = (x * qv) `mod` n

    go [] = []
    go (x:xs) = let c = x : mkClass x (nxt x)
                 in c : go [ y | y <- xs, y `notElem` c ]

    mkClass _ 0 = []
    mkClass s x
      | s == x = []
      | otherwise = x : mkClass s (nxt x)


data CyclotomicClass q ip = CyclotomicClass {
  ccIndices :: [Int],
  ccElems :: [GF q ip],
  ccMinPoly :: P.VPoly (PrimeFinite q)
} deriving Show


cyclotomicClasses :: forall q ip. (KnownNat q, KnownPoly q ip) => [CyclotomicClass q ip]
cyclotomicClasses = map mkClass $ cyclotomicIndices @q @ip
  where
    bi = byIndex @q @ip

    mkClass is = CyclotomicClass is es mp
      where
        es = map (bi Vector.!) is
        mp = minimalPoly $ head es


newtype CyclotomicClassesTex q ip = CyclotomicClassesTex [CyclotomicClass q ip]


instance (KnownNat q, KnownPoly q ip) => Show (CyclotomicClassesTex q ip) where
  showsPrec _ (CyclotomicClassesTex cs) =
    showString "Класс & Многочлены & Минимальный многочлен \\\\ \\hline \\hline\n" .
    foldr ((.) . showC) id cs
      where
        showC c =
          showClass c . showString " & " .
          showElems c . showString " & " .
          showPoly (ccMinPoly c) . showString " \\\\ \\hline\n"

        comma g f i = f . showString ", " . g i

        showClass c =
          showString "$\\{" .
          foldl (comma shows) (shows $ head $ ccIndices c) (tail $ ccIndices c) .
          showString "\\}$"

        showElems c = foldl (comma g) (g $ head $ ccElems c) (tail $ ccElems c)
          where
            g = showPoly . gfPoly


isSubfield :: forall q ip. (KnownNat q, KnownPoly q ip) => [GF q ip] -> Bool
isSubfield es = mul && add && min && inv
  where
    mul = all (`elem` es) [ x * y | x <- es, y <- es ]
    add = all (`elem` es) [ x + y | x <- es, y <- es ]
    min = all (`elem` es) [ -x | x <- es ]
    inv = all (`elem` es) [ recip x | x <- es, x /= 0 ]


data Subfield q ip = Subfield {
  sfElems :: [GF q ip],
  sfN :: Int
} deriving Show


subfields :: forall q ip. (KnownNat q, KnownPoly q ip) => [Subfield q ip]
subfields = do
  cc <- map ccElems (cyclotomicClasses @q @ip) ++ [elems]
  let c = nub $ 0 : 1 : cc
  unless (isSubfield c) []
  let d = intLog (fromInteger $ natValue @q) (length c)
  return $ Subfield c d
    where
      intLog b x
        | x == 1 = 0
        | otherwise = 1 + intLog b (x `div` b)


newtype SubfieldsTex q ip = SubfieldsTex [Subfield q ip]


instance (KnownNat q, KnownPoly q ip) => Show (SubfieldsTex q ip) where
  showsPrec _ (SubfieldsTex sfs) =
    showString "Поле & Элементы \\\\ \\hline\\hline \n" .
    foldr ((.) . showS . sortByIndex) id sfs
      where
        showS (Subfield es n) =
          showString "$GF(" . shows (natValue @q) . showString "^" . shows n . showString ")$ & " .
          showE (head es) . foldr ((.) . (\e -> showString ", " . showE e)) id (tail es) .
          showString "\\\\ \\hline\n"

        rd = rootDegrees @q @ip

        showE e =
          maybe (showString "0")
                (\i -> showString "$\\alpha^{" . shows i . showString "}$")
                (Map.lookup e rd)

        sortByIndex (Subfield es n) = Subfield ses n
          where
            ses = sortBy (\x y -> compare (Map.lookup x rd) (Map.lookup y rd)) es
