module Grammar
  ( TokenName(..)
  , TokenValue(..)
  , Token(..)
  , Terminal(..)
  , NonTerminal(..)
  , Element(..)
  , Rule
  , Code
  ) where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

--newtype TokenName  = TokenName String   deriving (Eq, Show, Ord)
--newtype TokenValue = TokenValue String  deriving (Eq, Show, Ord)
type TokenName = String

type TokenValue = String

type Code = String

data Token =
  Token TokenName TokenValue Code Code
  deriving (Eq, Show, Ord)

newtype Terminal =
  Terminal String
  deriving (Eq, Show, Ord) -- n = [0-9]*

newtype NonTerminal =
  NonTerminal String
  deriving (Eq, Show, Ord) -- E, E', T, T' ...

data Element
  = T String
  | NT String
  deriving (Eq, Show, Ord)

-- make it beautiful now.
--data GrammarWithTokens =
--  GWT [Token] Grammar
--  deriving (Show)
--
---- firstly all Elements are eps and nonterminals
--data Grammar =
--  Grammar (Map.Map NonTerminal [[Element]])
--  deriving (Show)

type Rule = (String, ([Element], Code))


{---to_rule_list' :: [(NonTerminal, [[Element]])] -> [(NonTerminal, [Element])]
                 -> [(NonTerminal, [Element])]
to_rule_list' ((nt, seq_els) : nts) rules = to_rule_list' nts $ rules ++ zip (repeat nt) seq_els
to_rule_list' [] rules = rules
calc_grammar :: Grammar
calc_grammar =
  Grammar
    (Map.fromList . reverse $
     [ (NonTerminal "E", [[NT "T", NT "E'"]])
     , (NonTerminal "E'", [[T "plus", NT "T", NT "E'"], [T "eps"]])
     , (NonTerminal "F", [[T "n"], [T "op", NT "E", T "cl"]])
     , (NonTerminal "T", [[NT "F", NT "T'"]])
     , (NonTerminal "T'", [[T "mul", NT "F", NT "T'"], [T "eps"]])
     ])

rules :: [Rule]
rules =
  [ (NonTerminal "E", [NT "T", NT "E'"])
  , (NonTerminal "E'", [T "plus", NT "T", NT "E'"])
  , (NonTerminal "E'", [T "eps"])
  , (NonTerminal "F", [T "n"])
  , (NonTerminal "F", [T "op", NT "E", T "cl"])
  , (NonTerminal "T", [NT "F", NT "T'"])
  , (NonTerminal "T'", [T "mul", NT "F", NT "T'"])
  , (NonTerminal "T'", [T "eps"])
  ]-}

{-show_grammar :: Grammar -> String
show_grammar (Grammar grammar_map) = join "      " (map show_rule $ Map.toList grammar_map)

show_rule :: (NonTerminal, [[Element]]) -> String
show_rule (NonTerminal nt, elem_lists) = nt ++ " -> " ++ show_elem_lists elem_lists

show_elem_lists :: [[Element]] -> String --[[A, B, Eps], [D, S']]
show_elem_lists elem_lists = do
    elem_seqs <- elem_lists
    mapped_seqs <- pure $ show_elem_seq elem_seqs
    let mapped_seqs_list = pure mapped_seqs -- [String], ["B 8", "+ A"]
    delim_seqs_list <- join " | " mapped_seqs_list
    return delim_seqs_list

show_elem_seq :: [Element] -> String
show_elem_seq elem_seq = join " " (map show_elem elem_seq)

-- don't want to see this always => no instance Show, just function
show_elem :: Element -> String
show_elem (T (Terminal t))      = t
show_elem (NT (NonTerminal nt)) = nt
show_elem Epsilon               = "eps"
show_elem End                   = "$"

define_terminals :: Grammar -> Grammar
define_terminals (Grammar grammar_map) = define_terminals' (Map.toList grammar_map) []

define_terminals' :: [(NonTerminal, Set.Set Element)] -> [(NonTerminal, Set.Set Element)]
                      -> [(NonTerminal, Set.Set Element)]
define_terminals' (r : rs) new_rules = let Set.toList $ snd r
to_terminal :: Element -> Element
to_terminal (NT t) = T t--}
