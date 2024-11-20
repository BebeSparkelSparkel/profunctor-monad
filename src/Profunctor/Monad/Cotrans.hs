{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
module Profunctor.Monad.Cotrans
  ( Cotrans(..)
  ) where

import Control.Monad ((>=>))
import Data.Coerce (coerce)
import Control.Arrow (Kleisli(Kleisli))
import qualified Control.Category as C

class Cotrans p m | p -> m where
  colift :: (a -> m b) -> p b c -> p a c

instance Monad m => Cotrans (Kleisli m) m where
  colift = (C.>>>) . Kleisli

instance Cotrans

newtype Fwd m u v = Fwd {unFwd :: m v}
instance Cotrans (Fwd m) m where
  colift = const coerce

newtype Bwd m u v = Bwd {unBwd :: u -> m v}
instance Monad m => Cotrans (Bwd m) m where
  colift f = Bwd . (>=>) f . unBwd
