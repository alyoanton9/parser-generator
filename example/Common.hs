{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Common
  ( Parser(..)
  , satisfy
  , single
  , token
  , ok
  , eof
  , (<|>)
  ) where

import           Control.Applicative
import           Control.Arrow
import           Data.Char
import           Data.Functor        ((<&>))
import           Data.Maybe

newtype Parser t a =
  Parser
    { runP :: [t] -> Maybe (a, [t])
    }

instance Functor (Parser t) where
  fmap :: (a -> b) -> Parser t a -> Parser t b
  fmap f (Parser parse) = Parser $ \tokens -> fmap (first f) (parse tokens)

instance Applicative (Parser t) where
  pure :: a -> Parser t a
  pure param = Parser $ \tokens -> Just (param, tokens)
  (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
  Parser funcP <*> Parser aP =
    Parser $ \tokens ->
      case funcP tokens of
        Nothing -> Nothing
        Just (func, tokens') ->
          case aP tokens' of
            Nothing            -> Nothing
            Just (a, tokens'') -> Just (func a, tokens'')

instance Alternative (Parser t) where
  empty :: Parser t a
  empty = Parser $ const Nothing
  (<|>) :: Parser t a -> Parser t a -> Parser t a
  Parser aP1 <|> Parser aP2 = Parser $ \tokens -> aP1 tokens <|> aP2 tokens

instance Monad (Parser t) where
  return :: a -> Parser t a
  return = pure
  (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
  Parser aP >>= func =
    Parser $ \tokens ->
      case aP tokens of
        Nothing            -> Nothing
        Just (res, remain) -> runP (func res) remain

satisfy :: (t -> Bool) -> Parser t t
satisfy predicate = token checkPred
  where
    checkPred t
      | predicate t = Just t
      | otherwise = Nothing

single :: Eq t => t -> Parser t t
single tok = satisfy (== tok)

token :: (t -> Maybe a) -> Parser t a
token ev =
  Parser $ \case
    [] -> Nothing
    (x:xs) -> ev x <&> (, xs)

ok :: Parser t ()
ok = Parser $ \s -> return ((), s)

eof :: Parser t ()
eof = Parser $ \case
        [] -> return ((), [])
        _  -> Nothing
