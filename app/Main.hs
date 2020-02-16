module Main where

import           Data.Function ((&))
import           Generator     (generate)
import           Grammar
import           GrammarLexer
import           GrammarParser
import           Utils


main :: IO ()
main = do
  fileName <- getLine
  genParser fileName
{-  input <- getContents
  let (initData, tokens, rules, finalData) = parseGrammar . alexScanTokens $ input
  let file = generate initData tokens rules finalData
  file & either (putStrLn . ("error: " ++)) putStrLn -}


genParser :: FilePath -> IO ()
genParser filePath =
  removeExtension ".alyo" filePath 
    & maybe (putStrLn $ filePath ++ " is not '.alyo' file") writeWithFileBase
  where
    writeWithFileBase fileBase = do
      input <- readFile filePath
      let (initData, tokens, rules, finalData) = parseGrammar . alexScanTokens $ input
      let genCode = generate initData tokens rules finalData
      genCode & either (putStrLn . ("error: " ++)) (writeWithCode fileBase)
    writeWithCode fileBase genCode = do
      common <- readFile "../assets/Common.hs"
      let dir = extractDir filePath
      writeFile (dir ++ "Common.hs") common
      writeFile (fileBase ++ ".hs") genCode

{-
{
data Token =  | bla | la deriving Show
}

{
module XYZ (parse) where

import Ast (Expression(..))
}


plus = "\+"   { const AddT }   { AddT }
mul = "\*"    { const MulT }   { MulT }
op = "\("     { const OpParT } { OpT }
cl = "\)"     { const ClParT } { ClT }
n = "[0-9]+"  { NumT . read }  { NumT $$ }
%%
E -> T E'       { maybe $1 (Add $1) $2 }
E' -> plus T E' { maybe $2 (Add $2) $3 }
E' ->           { Nothing }
T -> F T'       { maybe $1 (Mul $1) $2 }
T' -> mul F T'  { maybe $2 (Mul $2) $3 }
T' ->           { Nothing }
F -> n          { Val $1 }
F -> op E cl    { $1 }

{
data Token = blab | bla | la deriving Show
}

-}

-- expr = term `chainl1` parseAddSub   -- E -> TE'