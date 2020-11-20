module My.Channel(
  Channel(..),
  oneError, manyErrors
) where


import My.Arithmetic
import My.Code
import Control.Monad ((>=>))
import Control.Monad.Random (MonadRandom(..))

import qualified Data.Vector as V


newtype Channel m = Channel { applyChannel :: CodeVector -> m Message }


oneErrorV :: MonadRandom m => V.Vector Binary -> m (V.Vector Binary)
oneErrorV v = do
  i <- getRandomR (0, V.length v - 1)
  let old = v V.! i
  let new = 1 - old
  return $ v V.// [(i,new)]



manyErrorsV :: MonadRandom m => Int -> V.Vector Binary -> m (V.Vector Binary)
manyErrorsV 0 = pure
manyErrorsV n = manyErrorsV (n - 1) >=> oneErrorV


oneError :: MonadRandom m => Channel m
oneError = Channel $ \(CodeVector cv) -> Message <$> oneErrorV cv


manyErrors :: MonadRandom m => Int -> Channel m
manyErrors n = Channel $ \(CodeVector cv) -> Message <$> manyErrorsV n cv
