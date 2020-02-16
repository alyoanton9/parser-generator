module Grammar
  ( TokenName(..)
  , TokenValue(..)
  , Token(..)
  , Element(..)
  , Rule
  , Code
  ) where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

type TokenName = String

type TokenValue = String

type Code = String

data Token =
  Token TokenName TokenValue (Maybe (Code, Code))
  deriving (Eq, Show, Ord)

data Element
  = T String
  | NT String
  deriving (Eq, Show, Ord)

type Rule = (String, ([Element], Code))