{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module My.Code (
  LinearCode(..),
  CodeVector(..),
  Message(..),
  hammingMeasure,
  hammingDistance,
  allCodeVectors,
  codeDistance,
  encode,
  fromGeneratingMatrix,
  fromPolynomial,
  dualCode,
) where


import Data.List (sort)
import Data.Matrix
import My.Arithmetic
import My.MatrixUtil

import qualified Data.Vector as V


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
encode g m = CodeVector $ getCol 1 $ transpose (generatingMatrix g) * colVector (getMessage m)


invPerm :: [Int] -> [Int]
invPerm = map snd . sort . flip zip [0..]


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


matrixFromPolynomial :: Int -> V.Vector Binary -> Matrix Binary
matrixFromPolynomial n v = matrix k n gg
  where
    k  = n - V.length v + 1
    v' = V.generate n gf
    gv = V.fromList $ go k v'

    go i v
      | i == 0    = []
      | otherwise = v : go (i - 1) (step v)

    step v = V.generate n (gs v)

    gf i
      | i < V.length v = v V.! i
      | otherwise      = 0

    gs u i
      | i == 0    = 0
      | otherwise = u V.! (i - 1)

    gg (i, j) = gv V.! (i - 1) V.! (j - 1)


dividePolynomial :: V.Vector Binary -> V.Vector Binary -> V.Vector Binary
dividePolynomial q r
    | n == m && q' == r'            = V.fromList [1]
    | n < m && q' == V.fromList [0] = V.fromList [0]
    | n < m                         = error "non-divisible polynomials"
    | otherwise                     = expand (n - m + 1) $ nq `dividePolynomial` r
  where
    q' = cut q
    r' = cut r

    n = V.length q'
    m = V.length r'

    nq = q' .-. move (n - m) r'

    cut v = V.generate (pos (V.length v - 1) + 1) (v V.!)
      where
        pos k
          | k == 0       = 0
          | v V.! k == 0 = pos (k - 1)
          | otherwise    = k

    expand n v = V.generate n gg
      where
        gg i
          | i < V.length v = v V.! i
          | i == n - 1     = 1
          | otherwise      = 0

    move k v = V.generate (k + V.length v) gg
      where
        gg i
          | i < k     = 0
          | otherwise = v V.! (i - k)


fromPolynomial :: Int -> V.Vector Binary -> LinearCode
fromPolynomial n v = LinearCode n k g h
  where
    k = n - V.length v + 1
    g = matrixFromPolynomial n v
    h = matrixFromPolynomial n $ V.reverse iv

    p = V.generate (n + 1) (\i -> bv $ i == 0 || i == n)
    iv = p `dividePolynomial` v

    bv False = 0
    bv True  = 1


dualCode :: LinearCode -> LinearCode
dualCode (LinearCode n k g h) = LinearCode n (n - k) h g
