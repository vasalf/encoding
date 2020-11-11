{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module My.Code (
  LinearCode(..),
  CodeVector(..),
  hammingMeasure,
  hammingDistance,
  allCodeVectors,
  codeDistance,
  encode,
  fromGeneratingMatrix,
) where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.List (sort)
import Data.Matrix
import My.Arithmetic
import My.MatrixUtil

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV


data LinearCode = LinearCode {
  codeLength :: Int,
  codeRank :: Int,
  generatingMatrix :: Matrix Binary,
  checkMatrix :: Matrix Binary
}


newtype CodeVector = CodeVector { getCodeVector :: V.Vector Binary }
  deriving (Eq, Additive)


instance Show CodeVector where
  show = show . getCodeVector


newtype Message = Message { getMessage :: V.Vector Binary }
  deriving (Eq, Additive)


instance Show Message where
  show = show . getMessage


hammingMeasure :: CodeVector -> Int
hammingMeasure = fromInteger . V.foldr ((+) . toInteger) 0 . getCodeVector


hammingDistance :: CodeVector -> CodeVector -> Int
x `hammingDistance` y = hammingMeasure (x .-. y)


allCodeVectors :: LinearCode -> [CodeVector]
allCodeVectors g = go (V.replicate (codeLength g) 0) $ toLists $ generatingMatrix g
  where
    go :: V.Vector Binary -> [[Binary]] -> [CodeVector]
    go v []     = [CodeVector v]
    go v (r:rs) = go v rs ++ go (v .+. V.fromList r) rs


codeDistance :: LinearCode -> Int
codeDistance g = foldr (min . hammingMeasure) (codeLength g) $ drop 1 $ allCodeVectors g


encode :: LinearCode -> Message -> CodeVector
encode g m = CodeVector $ getCol 0 $ generatingMatrix g * colVector (getMessage m)


invPerm :: [Int] -> [Int]
invPerm xs = runST $ do
  let xv = V.fromList xs
  let n = V.length xv
  sv <- MV.new n
  forM_ [0..n - 1] (\i -> MV.write sv (xv V.! i) i)
  v <- V.generateM n (MV.read sv)
  return $ V.toList v


fromGeneratingMatrix :: Matrix Binary -> LinearCode
fromGeneratingMatrix g' = LinearCode n k g h
  where
    (i, g) = canonicalBasis g'
    n  = ncols g
    k  = nrows g
    r  = n - k
    is = sort i
    js = compl is [0..n - 1]
    a  = columnSubmatrix js g
    h' = transpose a <|> identity r
    ks = invPerm $ i ++ js
    h  = columnSubmatrix ks h'

    compl []     ys     = ys
    compl _      []     = []
    compl (x:xs) (y:ys) =
      case compare x y of
        LT -> compl xs (y:ys)
        EQ -> compl xs ys
        GT -> y : compl (x:xs) ys
