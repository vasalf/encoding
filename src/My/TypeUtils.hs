{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module My.TypeUtils (natValue) where


import GHC.TypeLits (KnownNat, natVal)
import Data.Proxy (Proxy(..))


natValue :: forall n. KnownNat n => Integer
natValue = natVal (Proxy @n)
