module Main where

import My.Arithmetic
import My.Code
import My.Information

import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector


code :: LinearCode
code = fromGeneratingMatrix g
  where
    g = Matrix.fromLists
      [[1,0,0,0,0,1,1,1],
       [0,1,0,0,1,0,1,1],
       [0,0,1,0,1,1,0,1],
       [0,0,0,1,1,1,1,0]]


informations :: [Information]
informations = map Information [[0,1,2,3],[4,5,6,7]]


errorVectors :: Int -> Int -> [Vector.Vector Binary]
errorVectors n k = map Vector.fromList $ go n k
  where
    go 0 _ = [[]]
    go n 0 = map (0:) $ go (n - 1) 0
    go n k = map (0:) (go (n - 1) k) ++ map (1:) (go (n - 1) (k - 1))


n :: Int
n = codeLength code


t :: Int
t = (codeDistance code - 1) `div` 2


testMessages :: [(CodeVector, Message)]
testMessages =
  [ (c, Message $ getCodeVector c .+. e) | e <- errorVectors n t
                                         , c <- allCodeVectors code ]


decodedTestMessages :: [(Message, CodeVector, CodeVector)]
decodedTestMessages =
  [(m, decodeByInformations informations code m, c) | (c, m) <- testMessages ]


wrongMessages :: [(Message, CodeVector, CodeVector)]
wrongMessages = filter (\(_, b, c) -> b /= c) decodedTestMessages


main :: IO ()
main = do
  print $ generatingMatrix code
  print $ checkMatrix code
  print $ map (isInformation code) informations
  print $ length wrongMessages
  mapM_ print wrongMessages
