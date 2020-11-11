module My.Information (
  Information(..),
  isInformation,
  tryDecodeByInformation,
  decodeByInformations
) where


import Data.Matrix
import My.Code
import My.MatrixUtil


newtype Information = Information { getInformation :: [Int] }


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
