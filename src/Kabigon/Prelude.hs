module Kabigon.Prelude
  ( module Prelude
  
  , module GHC.Num
  , module GHC.Generics
  , module GHC.TypeLits

  , module System.IO
  , module System.FilePath

  , module Data.Kind
  , module Data.Bool
  , module Data.Char
  , module Data.Int
  , module Data.Word
  , module Data.Text
  , module Data.Text.IO
  , module Data.Text.Encoding
  , module Data.ByteString
  , module Data.Tuple
  , module Data.Maybe
  , module Data.Either
  , module Data.List
  , module Data.Function
  , module Data.Eq
  , module Data.Ord
  , module Data.Semigroup
  , module Data.Monoid
  , module Data.Functor
  , module Data.Bifunctor
  , module Data.Traversable
  , module Data.Foldable

  , module Text.Show
  , module Text.Read

  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Control.Monad.State.Class
  , module Control.Monad.Reader.Class
  , module Control.Exception.Safe

  , module Control.Lens
  , module Control.Lens.TH

  , tshow
  , quote, an, indefinite, definite
  , headMay, atMay
  , throwLeft
  , log, warn
  , Pretty(..)
  , KabigonException(..), throw
  ) where

import Prelude (Double, quot, mod, rem, quotRem, pred, succ, fromIntegral, sqrt, pi, sin, cos, tan, round, (/))

import GHC.Num (Num(..))
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol)

import System.IO (IO, stdin, stdout, stderr, FilePath)
import System.FilePath ((</>))

import Data.Kind (Type)
import Data.Bool (Bool(..), otherwise, not, (&&), (||))
import Data.Char (Char, isUpper)
import Data.Int (Int)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Text (Text, pack, unpack, unwords)
import Data.Text.IO (putStrLn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Tuple (fst, snd, curry, uncurry, swap)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, catMaybes, mapMaybe)
import Data.Either (Either(..))
import Data.List (take, drop, dropWhile, filter, reverse, lookup, zip, zip3, replicate, sortOn, concatMap, elemIndex)
import Data.Function (id, const, flip, ($), (&), (.))
import Data.Eq (Eq(..))
import Data.Ord (Ord(..), Down(..))
import Data.Semigroup(Semigroup(..), (<>))
import Data.Monoid (Monoid(..), mconcat)
import Data.Functor (Functor(..), (<$>), (<$), ($>))
import Data.Bifunctor (Bifunctor(..), first, second)
import Data.Traversable (Traversable(..), forM, sequence)
import Data.Foldable (Foldable(..), any, all, mapM_, forM_)

import Text.Show (Show(..))
import Text.Read (readMaybe)

import Control.Applicative (Applicative(..), (<*), (*>))
import Control.Monad (Monad(..), join, forever, mapM, forM, foldM, void, when, unless, (>>=), (=<<), (>=>), (<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..), get, put, modify)
import Control.Monad.Reader.Class (MonadReader(..), ask)
import Control.Exception.Safe (Exception, SomeException, IOException, MonadThrow, MonadCatch, throwM, try, catch, catchIO)
import Control.Logging (log', warn')

import Control.Lens
  ( view, (^.), views, use, uses, preuse, (^?)
  , set, (.~), (?~), over, (%~), setting, assign, (.=), (%=), (<~)
  , at, ix, to
  , _1, _2, _Just
  )
import Control.Lens.TH (makeLenses, makeClassy)

tshow :: Show a => a -> Text
tshow = pack . show

quote :: Text -> Text
quote s = mconcat ["\"", s, "\""]

an :: Text -> Text
an t = unwords
  [ case headMay $ unpack t of
      Just c | c `elem` ("aeiou" :: [Char]) -> "an"
      _ -> "a"
  , t
  ]

indefinite :: Text -> Text
indefinite t
  | Just c <- headMay $ unpack t
  , isUpper c
  = t
  | otherwise = an t

definite :: Text -> Text
definite t
  | Just c <- headMay $ unpack t
  , isUpper c
  = t
  | otherwise = unwords ["the", t]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs $ n - 1

throwLeft :: (Exception e, MonadThrow m) => (b -> e) -> Either b a -> m a
throwLeft f (Left x) = throwM $ f x
throwLeft _ (Right x) = pure x

log :: MonadIO m => Text -> m ()
log = log'

warn :: MonadIO m => Text -> m ()
warn = warn'

class Pretty a where
  pretty :: a -> Text

newtype KabigonException = KabigonException Text
  deriving (Show, Eq, Ord)
instance Exception KabigonException

throw :: MonadThrow m => Text -> m a
throw = throwM . KabigonException
