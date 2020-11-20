module My.Arithmetic (Binary(..), Additive(..), (.*.)) where


import Control.Monad.Random (Random(..), genRange)

import qualified Data.Vector as V


data Binary = Zero
            | One
            deriving (Eq, Ord, Enum)


instance Num Binary where
  Zero + x    = x
  x    + Zero = x
  One  + One  = Zero

  (-) = (+)

  Zero * _    = Zero
  _    * Zero = Zero
  One  * One  = One

  negate = id
  abs    = id
  signum = id

  fromInteger x
    | even x    = Zero
    | otherwise = One


instance Show Binary where
  show Zero = "0"
  show One  = "1"


instance Read Binary where
  readsPrec _ ('0':xs) = [(Zero, xs)]
  readsPrec _ ('1':xs) = [(One, xs)]
  readsPrec _ _        = []


instance Real Binary where
  toRational Zero = 0
  toRational One  = 1


instance Fractional Binary where
  x / One  = x
  _ / Zero = error "division by zero"

  fromRational 0 = Zero
  fromRational 1 = One
  fromRational _ = error "unable to convert"


instance Integral Binary where
  x `quotRem` One  = (x, Zero)
  _ `quotRem` Zero = error "division by zero"

  toInteger Zero = 0
  toInteger One  = 1


instance Random Binary where
  randomR (l, r) g = (fromBool res, g')
    where
      toBool Zero = False
      toBool One  = True

      (res, g') = randomR (toBool l, toBool r) g

      fromBool False = Zero
      fromBool True  = One

  random = randomR (Zero, One)


class Additive a where
  infixl 6 .+.
  infixl 6 .-.
  (.+.) :: a -> a -> a
  (.-.) :: a -> a -> a


instance Num a => Additive (V.Vector a) where
  (.+.) = V.zipWith (+)
  (.-.) = V.zipWith (-)


infixl 7 .*.
(.*.) :: Num a => a -> V.Vector a -> V.Vector a
(.*.) a = V.map (a *)
