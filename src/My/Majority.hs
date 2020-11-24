module My.Majority(
  maxQuasi,
  bestQuasi,
) where

import My.Code

import qualified Data.Vector as Vector


maxQuasi :: LinearCode -> [Int] -> [CodeVector]
maxQuasi g hs = go [] candidateVectors
  where
    h = dualCode g
    candidateVectors = filter (\v -> all ((== 1) . (getCodeVector v Vector.!)) hs) (allCodeVectors h)

    canAdd us v = all (\i -> i `elem` hs || present i <= 1) [0..codeLength g -1]
      where
        present i = length $ filter ((== 1) . (Vector.! i) . getCodeVector) $ v:us

    go ans []     = ans
    go ans (v:vs)
      | not (canAdd ans v) = go ans vs
      | otherwise = let a0 = go ans vs
                        a1 = go (v:ans) vs
                     in argmax length a0 a1
    
    argmax f x y
      | f x > f y = x
      | otherwise = y


bestQuasi :: LinearCode -> ([Int], [CodeVector])
bestQuasi g = (hs, maxQuasi g hs)
  where
    subsets []     = [[]]
    subsets (x:xs) = let s = subsets xs
                      in s ++ map (x:) s

    argmax f xs = let mxx = maximum (map f xs)
                   in filter ((== mxx) . f) xs

    toCV xs = CodeVector $ Vector.generate (codeLength g) (\i -> fromIntegral $ fromEnum $ i `elem` xs)

    ss = subsets [0..codeLength g - 1]
    css = filter (not . (`elem` allCodeVectors (dualCode g)) . toCV) ss

    chs = argmax (length . maxQuasi g) $ filter ((> 1) . length) css
    hs = last $ argmax (negate . length) chs
