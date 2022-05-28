
module Parser (parse) where
import Ast (Expression(..))

import Common
import Text.Regex.TDFA ((=~~))
import Control.Exception (assert)
import Data.Function ((&))

data Token  = OpParT
            | ClParT
            | PlusT
            | MinusT
            | StarT
            | SlashT
            | NumT Int
            deriving Eq

parse str = do
  tokens <- tokenize str
  runP (parse_E <* eof) tokens & maybe
      (Left "failed to parse") (return . fst)

prefixMatch :: String -> String -> Maybe (String, String)
prefixMatch str regex = do
    (left, middle, right) <- str =~~ ('\\' : '`' : regex)
    assert ("" == left) $ return (middle, right)

tokenize [] = return []
tokenize str | Just (match, rest) <- prefixMatch str "\\+" = ( ( const PlusT ) match :) <$> tokenize rest
tokenize str | Just (match, rest) <- prefixMatch str "-" = ( ( const MinusT ) match :) <$> tokenize rest
tokenize str | Just (match, rest) <- prefixMatch str "\\*" = ( ( const StarT ) match :) <$> tokenize rest
tokenize str | Just (match, rest) <- prefixMatch str "\\\\" = ( ( const SlashT ) match :) <$> tokenize rest
tokenize str | Just (match, rest) <- prefixMatch str "\\(" = ( ( const OpParT ) match :) <$> tokenize rest
tokenize str | Just (match, rest) <- prefixMatch str "\\)" = ( ( const ClParT ) match :) <$> tokenize rest
tokenize str | Just (match, rest) <- prefixMatch str "[0-9]+" = ( ( NumT . read ) match :) <$> tokenize rest
tokenize str | Just (match, rest) <- prefixMatch str "[[:space:]]+" = tokenize rest
tokenize str = Left $ "Tokenization failure at '" ++ str ++ "'"

parseTok_1 = single $  PlusT
parseTok_2 = single $  MinusT
parseTok_3 = single $  StarT
parseTok_4 = single $  SlashT
parseTok_5 = single $  OpParT
parseTok_6 = single $  ClParT
parseTok_7 = token getV
  where
    getV ( NumT v ) = Just v
    getV _ = Nothing


parse_E
    = rule1 <$> parse_T <*> parse_E'
  where
    rule1 v1 v2 =  v2 v1

parse_E'
    = rule1 <$> parseTok_1 <*> parse_T <*> parse_E'
  <|> rule2 <$> parseTok_2 <*> parse_T <*> parse_E'
  <|> rule3 <$ ok
  where
    rule1 v1 v2 v3 =  v3 . flip Add v2
    rule2 v1 v2 v3 =  v3 . flip Sub v2
    rule3  =  id

parse_F
    = rule1 <$> parseTok_7
  <|> rule2 <$> parseTok_5 <*> parse_E <*> parseTok_6
  <|> rule3 <$> parseTok_2 <*> parse_F
  where
    rule1 v1 =  Num v1
    rule2 v1 v2 v3 =  v2
    rule3 v1 v2 =  Neg v2

parse_T
    = rule1 <$> parse_F <*> parse_T'
  where
    rule1 v1 v2 =  v2 v1

parse_T'
    = rule1 <$> parseTok_3 <*> parse_F <*> parse_T'
  <|> rule2 <$> parseTok_4 <*> parse_F <*> parse_T'
  <|> rule3 <$ ok
  where
    rule1 v1 v2 v3 =  v3 . flip Mul v2
    rule2 v1 v2 v3 =  v3 . flip Div v2
    rule3  =  id


