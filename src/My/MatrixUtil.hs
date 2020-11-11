module My.MatrixUtil (
  diagonalBasis,
  traverseDiagonalBasis,
  basis,
  canonicalBasis,
  rank,
  columnSubmatrix
) where

import Data.Matrix
import My.Arithmetic

import qualified Data.Vector as V


setz :: (Eq a, Fractional a) => Int -> V.Vector a -> V.Vector a -> V.Vector a
setz i v u = u .-. ((u V.! i) / (v V.! i)) .*. v

diagonalBasis :: (Eq a, Fractional a) => Matrix a -> [(Int, V.Vector a)]
diagonalBasis mtx = go 0 vl
  where
    go _ [] = []
    go i vvs@(v:vs)
      | i == m       = go 0 vs
      | v V.! i == 0 = go (i + 1) vvs
      | otherwise    = (i,v) : go 0 (map (setz i v) vs)

    m = ncols mtx
    vl = map V.fromList $ toLists mtx


traverseDiagonalBasis :: [(Int, V.Vector a)] -> ([Int], Matrix a)
traverseDiagonalBasis b = (map fst b, matrix n m f)
  where
    n = length b
    m = V.length $ snd $ head b
    bv = V.fromList $ map snd b
    f (i, j) = bv V.! (i - 1) V.! (j - 1)


basis :: (Eq a, Fractional a) => Matrix a -> Matrix a
basis = snd . traverseDiagonalBasis . diagonalBasis


canonicalBasis :: (Eq a, Fractional a) => Matrix a -> ([Int], Matrix a)
canonicalBasis mtx = (map fst db, matrix n m f)
  where
    go []         = []
    go ((i,v):bs) = v : go (map (fmap $ setz i v) bs)

    db = diagonalBasis mtx
    bv = V.fromList $ reverse $ go $ reverse db
    n = V.length bv
    m = V.length $ V.head bv
    f (i, j) = bv V.! (i - 1) V.! (j - 1)


rank :: (Eq a, Fractional a) => Matrix a -> Int
rank = nrows . basis


columnSubmatrix :: [Int] -> Matrix a -> Matrix a
columnSubmatrix cs mtx = matrix n m f
  where
    n = nrows mtx
    m = length cs
    cv = V.fromList cs
    f (i, j) = mtx ! (i, 1 + cv V.! (j - 1))
