module My.Information (
  Information(..),
  isInformation,
  tryDecodeByInformation,
  decodeByInformations,
  completeInformations,
  buildInformations
) where


import Data.List (sort)
import Data.Matrix
import My.Code
import My.MatrixUtil

import qualified Data.Vector as V


newtype Information = Information { getInformation :: [Int] }
  deriving (Eq, Ord, Show)


isInformation :: LinearCode -> Information -> Bool
isInformation g (Information i) = rank (columnSubmatrix i $ generatingMatrix g) == codeRank g


tryDecodeByInformation :: Information -> LinearCode -> Message -> CodeVector
tryDecodeByInformation (Information i) g (Message m) = CodeVector $ getRow 1 $ cg * g'
  where
    gg = columnSubmatrix i $ generatingMatrix g
    g' = either error (* generatingMatrix g) (inverse gg)
    cg = columnSubmatrix i $ rowVector m


decodeByInformations :: [Information] -> LinearCode -> Message -> CodeVector
decodeByInformations is g m = foldr1 combine attempts
  where
    attempts = map (\i -> tryDecodeByInformation i g m) is

    combine x y
      | dx < dy   = x
      | otherwise = y
      where
        dx = x `hammingDistance` CodeVector (getMessage m)
        dy = y `hammingDistance` CodeVector (getMessage m)


completeInformations :: Int -> LinearCode -> [Information] -> [Information]
completeInformations k g i = foldr add i errorVectors
  where
    gm = generatingMatrix g
    n = codeLength g

    add ev ans
      | 0 `elem` map (vscalar ev . ivec) ans = ans
      | otherwise = buildNew ev : ans

    buildNew ev = Information $ reverse $ go 0 []
      where
        go k ans
          | k == n = ans
          | ev V.! k == 1 = go (k + 1) ans
          | null ans = go (k + 1) (k:ans)
          | rank (columnSubmatrix (k:ans) gm) == length ans + 1 = go (k + 1) (k:ans)
          | otherwise = go (k + 1) ans

    ivec (Information i) = V.fromList $ go 0 is
      where
        go i []
          | i == n    = []
          | otherwise = 0 : go (i + 1) []
        go i (x:xs)
          | i == x    = 1 : go (i + 1) xs
          | otherwise = 0 : go (i + 1) (x:xs)

        is = sort i

    vscalar u = V.sum . V.zipWith (*) (V.map toInteger u) . V.map toInteger

    errorVectors = map V.fromList $ go k (codeLength g)
      where
        go _ 0 = [[]]
        go 0 n = map (0:) (go 0 (n - 1))
        go k n = map (0:) (go k (n - 1)) ++ map (1:) (go (k - 1) (n - 1))


buildInformations :: Int -> LinearCode -> [Information]
buildInformations k g = completeInformations k g []
