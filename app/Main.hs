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
      common <- readFile "./assets/Common.hs"
      let dir = extractDir filePath
      writeFile (dir ++ "Common.hs") common
      writeFile (fileBase ++ ".hs") genCode
