{-# LANGUAGE OverloadedStrings #-}
module Pattern where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T

-- | Apply a function to replace the text between a given start and end tag.
-- Example:
--
-- >>> runIdentity $ substituteBetween "%" "%" (return . map toUpper) "abcd%def%gh"
-- "abcdDEFgh"
substituteBetween :: Applicative f => T.Text -> T.Text -> (T.Text -> f T.Text) -> T.Text -> f T.Text
substituteBetween open close f t
  | T.null t = pure t
  | T.null pat = (b <>) <$> substituteBetween open close f e
  | otherwise = (\x y -> b <> x <> y) <$> f pat <*> substituteBetween open close f e
  where (b,r) = T.breakOn open t
        (pat,e') = T.breakOn close $ T.drop (T.length open) r
        e = T.drop (T.length close) e'

-- | Evaluate a pattern using the lookup function @f@. A pattern may contain the following special expressions:
--
--   - $$a$$ will be replaced with @f a@, returning Left a if @f@ returns Nothing
--   - ??a?? will be replaced with @f a@, returning an empty string if @f@ returns Nothing
evalPattern :: (Applicative f, Monad f) => (T.Text -> f (Maybe T.Text)) -> T.Text -> ExceptT T.Text f T.Text
evalPattern f t = replaceRequiredVar . fromMaybe T.empty =<< runMaybeT (replaceOptionalVar t)
  where replaceRequiredVar = substituteBetween "$$" "$$" (\x -> maybe (throwE x) return =<< lift (f x))
        replaceOptionalVar = substituteBetween "??" "??" (MaybeT . lift . f)
