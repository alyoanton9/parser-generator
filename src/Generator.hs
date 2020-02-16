module Generator
  ( generate
  ) where

import           Data.Char
import           Data.Function   ((&))
import           Data.List       (intercalate, uncons)
import           Grammar
import           Utils           (collect, maybeToEither, replaceNumberStub,
                                  replaceStub)

import qualified Data.Map.Strict as Map

{-

data Token = Token TokenName TokenValue TokenCode deriving (Eq, Show, Ord)
type Rule = (NonTerminal, ([Element], Code))

-}
type TokToI = Map.Map String Int

myImports :: [String]
myImports = ["Common", "Text.Regex.TDFA ((=~~))", "Control.Exception (assert)", "Data.Function ((&))"]

--map ("tokenize str | Just (match, right) <- prefixMatch "
generate :: String -> [Token] -> [Rule] -> String -> Either String String
generate initData tokens rules finalData = do
  nts <- collect rules & mapM (uncurry $ generateNonTerminal tokToI)
  let imports = myImports & map ("import " ++) & intercalate "\n"
  return $ unlines [initData, imports, finalData, mainFunction, generateTokenizer tokens, mapTokens, unlines nts]
  where
    tokToI = Map.fromList (map getTokenName tokens `zip` [1 ..])
    getTokenName (Token name _ _ _) = name
    mainFunction = unlines [ "parse str = do"
                           , "  tokens <- tokenize str"
                           , "  runP (parse_" ++ firstNT ++ " <* eof) tokens & maybe"
                           , "      (Left \"failed to parse\") (return . fst)"]
    firstNT = head rules & fst
    mapTokens = tokens & map (\(Token name _ _ ctorCode) -> generateTokenParser (tokToI Map.! name) ctorCode) & unlines

generateTokenizer :: [Token] -> String
generateTokenizer tokens = helperFunction ++ "\n" ++ tokenizerFunction
  where
    helperFunction =
      unlines
        [ "prefixMatch :: String -> String -> Maybe (String, String)"
        , "prefixMatch str regex = do"
        , "    (left, middle, right) <- str =~~ ('^' : regex)"
        , "    assert (\"\" == left) $ return (middle, right)"
        ]
    tokenizerFunction =
      unlines
        [ "tokenize [] = return []"
        , tokens & map makeTokenizeLine & intercalate "\n"
        , "tokenize str = Left $ \"Tokenization failure at '\" ++ str ++ \"'\""
        ]
    makeTokenizeLine (Token _ value code _) =
      "tokenize str | Just (match, rest) <- prefixMatch str " ++
      show value ++ " = ( (" ++ code ++ ") match :) <$> tokenize rest"

generateNonTerminal :: TokToI -> String -> [([Element], Code)] -> Either String String
generateNonTerminal tokToI nt productionsPairs = do
  let (productions, codes) = unzip productionsPairs
  let lengths = map length productions
  rules' <- zipWith makeRule [1 ..] productions & sequence
  case uncons rules' of
    Nothing -> Left $ "No productions for " ++ nt
    Just (rule1, rules) ->
      return $
      concat
        [ "parse_" ++ nt ++ "\n"
        , "    = " ++ rule1 ++ "\n" ++ unlines (map ("  <|> " ++) rules)
        , "  where\n"
        , unlines $ zipWith3 makeBindLine [1 ..] lengths codes
        ]
  where
    makeRule i = generateRule tokToI ("rule" ++ show i)
    makeBindLine i count code = "    " ++ generateBind ("rule" ++ show i) count code

-- find productions with the 0 length => they will be in the end of parse 
-- if it exist => delete it && go to the end
-- if it doesn't exist => a pofek!

generateRule :: TokToI -> String -> [Element] -> Either String String
generateRule tokToI ruleName prod = do
  funcs <- mapM genFunc prod
  let args =
        if null prod
          then " <$ ok"
          else " <$> " ++ intercalate " <*> " funcs
  return $ ruleName ++ args
  where
    genFunc (T t)
      | Just i <- Map.lookup t tokToI = return $ "parseTok_" ++ show i
      | otherwise = Left "Unkown terminal"
    genFunc (NT nt) = return $ "parse_" ++ nt

-- {Add $2 $3}
generateBind :: String -> Int -> Code -> String
generateBind ruleName count code =
  ruleName ++ " " ++ unwords (map (("v" ++) . show) [1 .. count]) ++ " = " ++ replaceNumberStub "v" code

generateTokenParser :: Int -> String -> String
generateTokenParser tokenI ctorCode
  | Just replaced <- replaceStub "v" ctorCode = common ++ "token getV\n" ++ makeTokenExtractorBinding replaced
  | otherwise = common ++ "single $ " ++ ctorCode
  where
    common = "parseTok_" ++ show tokenI ++ " = "

makeTokenExtractorBinding :: String -> String
makeTokenExtractorBinding ctor = unlines ["  where", "    getV (" ++ ctor ++ ") = Just v", "    getV _ = Nothing"]

-- let it be like this, examples of used modules
{-
tokens = [Token "plus" "+", Token "mul" "*", Token "num" "[0-9]+"]

parseAddSub :: Parser (Expression -> Expression -> Expression)
parseAddSub = (parseBinOper AddT Add) <|> (parseBinOper SubT Sub)
parseT =
data Token  = OpParT --op
            | ClParT --cl
            | AddT   --add
            | SubT
            | MulT
            | DivT
            | ModT
            | NegT
            | NumT Int
            | ErrorT String
            deriving Eq
-}
{-
2: T -> T + F { Add $1 $3 }
      | T - F { Sub $1 $3 }
4: T -> F     { $1 }
5: T -> n     { $1 & \(NumT v) -> Var v }

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b --applic
(<*) :: f a -> f b -> f a

func1 :: Expr -> Token -> Expr -> Expr

parseT
    = func1 <$> parseT <* parseToken '+" <*> parseF
  <|> func2 <$> parseT <*> parseToken '-' <*> parseF
  <|> func3 <$> parseF

     where
       func1 v1 v2 v3 = Add v1 v3
       func2 v1 v2 v3 = Sub v1 v3
       func3 v1 = v1

-- lection notes
-XTupleSection
map (42,) [1..5] => [(42,1)..(42,5)]

list comprehension
-}
-- If token is regular expression => need to have one more field for its constructor
-- parseToken, tokenPred ?
-- read what in {} -> grammar
