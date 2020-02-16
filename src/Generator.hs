module Generator
  ( generate
  ) where

import           Data.Char
import           Data.Function   ((&))
import           Data.List       (delete, find, intercalate, null, uncons)
import           Data.Maybe      (fromMaybe, isJust)
import           Grammar
import           Utils           (collect, maybeToEither, replaceNumberStub,
                                  replaceStub)

import qualified Data.Map.Strict as Map

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
    tokens' = filter (\(Token _ _ maybeCode) -> isJust maybeCode) tokens
    tokToI = Map.fromList (map getTokenName tokens' `zip` [1 ..])
    getTokenName (Token name _ _) = name
    mainFunction =
      unlines
        [ "parse str = do"
        , "  tokens <- tokenize str"
        , "  runP (parse_" ++ firstNT ++ " <* eof) tokens & maybe"
        , "      (Left \"failed to parse\") (return . fst)"
        ]
    firstNT = head rules & fst
    mapTokens =
      tokens' & map (\(Token name _ (Just (_, ctorCode))) -> generateTokenParser (tokToI Map.! name) ctorCode) & unlines

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
    makeTokenizeLine (Token _ regex maybeCode) =
      "tokenize str | Just (match, rest) <- prefixMatch str " ++
      show regex ++
      " = " ++
      (case maybeCode of
         Nothing        -> ""
         Just (code, _) -> "( (" ++ code ++ ") match :) <$> ") ++
      "tokenize rest"

generateNonTerminal :: TokToI -> String -> [([Element], Code)] -> Either String String
generateNonTerminal tokToI nt productionsPairs = do
  let epsProduction = find (null . fst) productionsPairs
  let (productions, codes) =
        case epsProduction of
          Just epsProd -> productionsPairs & delete epsProd & (++ [epsProd]) & unzip
          Nothing -> productionsPairs & unzip
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
