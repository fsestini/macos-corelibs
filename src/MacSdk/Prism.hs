{-# LANGUAGE RankNTypes #-}

module MacSdk.Prism where

import Data.Functor.Identity (Identity(..))
import Data.Profunctor (Profunctor(..))
import Data.Profunctor.Choice (Choice(..))
import Control.Monad.Error.Class (MonadError(..))

newtype Tagged x b = Tagged { unTagged :: b }

instance Profunctor Tagged where
  dimap _ g (Tagged x) = Tagged (g x)
instance Choice Tagged where
  left' (Tagged x) = Tagged (Left x)
  right' (Tagged x) = Tagged (Right x)

type Prism' s a =
  forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

review :: AReview t b -> b -> t
review r = runIdentity . unTagged . r . Tagged . Identity

throwing :: MonadError t m => Prism' t e -> e -> m a
throwing p e = throwError (review p e)
