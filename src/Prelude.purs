module Foundation.Prelude
  ( (∘), (⊕), (⋙), (⋘), (≡), (≠), (×), (≪), (≫)
  , (∨), (∧), (⨁), (⊹)
  , flipCompose, applyRight, applyLeft
  , hLog
  , type (⨁), type (⊹), type (×)
  , module Prelude
  , module Control.Alt
  , module Control.Apply
  , module Control.Bind
  , module Control.Monad
  , module Control.Monad.Error.Class
  , module Control.Monad.Except
  , module Control.Monad.Maybe.Trans
  , module Control.Monad.Reader
  , module Control.Monad.Trans.Class
  , module Control.MonadPlus
  , module Control.Parallel
  , module Control.Plus
  , module Data.Bifoldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.Const
  , module Data.Either
  , module Data.Foldable
  , module Data.Functor
  , module Data.Functor.Coproduct
  , module Data.Generic
  , module Data.Maybe
  , module Data.Newtype
  , module Data.Monoid
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Void
)
       where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply ((*>), (<*))
import Control.Bind (join, (>=>), (<=<))
import Control.Monad (when, unless)
import Control.Monad.Error.Class (class MonadError, throwError, catchError)
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT, except)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Parallel (class Parallel, parTraverse, parTraverse_)
import Control.Plus (class Plus, empty)

import Data.Bifoldable (class Bifoldable, bitraverse_, bifor_)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bitraverse, bisequence, bifor)
import Data.Const (Const(..))
import Data.Either (Either(..), either, isLeft, isRight, fromRight)
import Data.Foldable (class Foldable, traverse_, for_, foldMap, foldl, foldr, fold)
import Data.Functor (($>), (<$))
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, ala, alaF)
import Data.Traversable (class Traversable, traverse, sequence, for)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Void (Void, absurd)

import Control.Monad.Eff.Console (logShow)
import Halogen as H

flipCompose ∷ ∀ a b c d. Semigroupoid a ⇒ a b c → a c d → a b d
flipCompose = flip compose

applyRight ∷ ∀ f a b. Apply f ⇒ f a → f b → f b
applyRight a b = const id <$> a <*> b

applyLeft ∷ ∀ f a b. Apply f ⇒ f a → f b → f a
applyLeft a b = const <$> a <*> b

infixr 9 compose as ∘
infixr 5 append as ⊕
infixr 9 flipCompose as ⋙
infixr 9 compose as ⋘
infixl 4 apply as ⊛
infix 4 eq as ≡
infix 4 notEq as ≠
infixr 1 Tuple as ×
infixl 4 applyRight as ≫
infixl 4 applyLeft as ≪
infixr 3 conj as ∧
infixr 2 disj as ∨

infixr 4 type Either as ⊹
infixr 4 type Coproduct as ⨁
infixr 5 either as ⊹
infixr 5 coproduct as ⨁

infixr 4 type Tuple as ×

hLog = H.liftEff ∘ logShow
