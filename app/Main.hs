module Main where

import My.Code

import qualified Data.Matrix as Matrix

code = fromGeneratingMatrix g
  where
    g = Matrix.fromLists
      [[1,1,1,0,0,0,0],
       [1,0,0,1,1,0,0],
       [0,1,0,1,0,1,0],
       [1,1,0,1,0,0,1]]

main :: IO ()
main = do
  print $ allCodeVectors code
  print $ codeDistance code
