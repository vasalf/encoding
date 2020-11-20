module Main where

import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom(..))

import My.Arithmetic()
import My.Channel
import My.Code
import My.Information

import qualified Data.Vector as Vector


code :: LinearCode
code = fromPolynomial 31 p
  where
    p = Vector.fromList [1,0,1,0,0,0,1,1,1,1,0,1,1,1,1,1,1]


n :: Int
n = codeLength code


t :: Int
t = (codeDistance code - 1) `div` 2


channel :: MonadRandom m => Channel m
channel = manyErrors t


shifts :: [Int] -> [[Int]]
shifts xs = [ [ (x + i) `mod` n | x <- xs ] | i <- [0..n - 1] ]


informations :: [Information]
informations = map Information $ 
  shifts [0..14] ++
  shifts ([0..7] ++ [20..26])


errorVectors :: Int -> Int -> [[Int]]
errorVectors = go 0
  where
    go i n k
      | i == n    = [[]]
      | k == 0    = [[]]
      | otherwise = map (i:) (go (i + 1) n (k - 1)) ++ go (i + 1) n k


data TestData = TestData {
  testInitMessage :: Message,
  testEncodedVector :: CodeVector,
  testTransmittedMessage :: Message,
  testCleanedVector :: CodeVector
} deriving Show


passed :: TestData -> Bool
passed d = testEncodedVector d == testCleanedVector d


doTests :: MonadRandom m => m [TestData]
doTests = replicateM 100 test
  where
    test = do
      i <- Message <$> Vector.replicateM (codeRank code) getRandom
      let e = encode code i
      tr <- applyChannel channel e
      let c = decodeByInformations informations code tr
      return $ TestData i e tr c


reportTests :: [TestData] -> [TestData] -> IO ()
reportTests tests failed
  | null failed = do
      mapM_ print tests
      putStrLn $ "OK: " ++ show (length tests) ++ " tests passed"
  | otherwise =
      putStrLn $ "Fail: " ++ show (length failed) ++ " tests failed"


main :: IO ()
main = do
  putStrLn "G = "
  print (generatingMatrix code)
  putStrLn $ show (length informations) ++ " informations"
  tests <- doTests
  let failed = filter (not . passed) tests
  reportTests tests failed

