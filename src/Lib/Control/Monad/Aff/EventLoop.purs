module Control.Monad.Aff.EventLoop
  ( make
  , make'
  , forever
  , forever'
  , break
  , break'
  , Breaker
  ) where

import Prelude
import Control.Monad.Aff.AVar (AffAVar, AVAR, makeVar', takeVar, putVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Data.Maybe (Maybe(..), maybe)

newtype Breaker a = Breaker (∀ eff. a → AffAVar eff Unit)

-- | Creates a new runnable event loop, which can be started using `run`. The
-- | event loop will run forever
make
  ∷ ∀ eff a
  . AffAVar eff (Maybe a)
  → AffAVar eff { breaker ∷ Breaker a, run ∷ AffAVar eff a }
make go = do
  breaker ← makeVar' Nothing
  pure
    { breaker: Breaker \a → putVar breaker (Just a)
    , run: tailRecM (loop breaker) unit
    }

  where
  loop breaker _ = do
    res ← takeVar breaker
    putVar breaker Nothing
    case res of
      Nothing → maybe (Loop unit) Done <$> go
      Just a → pure (Done a)

-- | Like `make` but with an MonadAff constraint.
make'
  ∷ ∀ eff m a
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ MonadRec m
  ⇒ m (Maybe a)
  → m { breaker ∷ Breaker a, run ∷ m a }
make' go = liftAff do
  breaker ← makeVar' Nothing
  pure
    { breaker: Breaker \a → putVar breaker (Just a)
    , run: tailRecM (loop breaker) unit
    }

  where
  loop breaker _ = do
    res ← liftAff (takeVar breaker)
    liftAff (putVar breaker Nothing)
    case res of
      Nothing → maybe (Loop unit) Done <$> go
      Just a → pure (Done a)

forever
  ∷ ∀ eff a
  . AffAVar eff a
  → AffAVar eff { breaker ∷ Breaker Unit, run ∷ AffAVar eff Unit }
forever go =
  make (go $> Nothing)

forever'
  ∷ ∀ eff m a
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ MonadRec m
  ⇒ m a
  → m { breaker ∷ Breaker Unit, run ∷ m Unit }
forever' go =
  make' (go $> Nothing)

break ∷ ∀ eff a. Breaker a → a → AffAVar eff Unit
break (Breaker run) = run

break' ∷ ∀ eff. Breaker Unit → AffAVar eff Unit
break' (Breaker run) = run unit
