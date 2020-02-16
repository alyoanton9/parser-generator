module Utils
  ( collect
  , replaceStub
  , replaceNumberStub
  , maybeToEither
  , removeExtension
  , extractDir
  ) where

import           Control.Arrow   (second)
import           Data.Char       (isDigit)
import           Data.Function   ((&))
import qualified Data.Map.Strict as Map
import           Data.List       (isSuffixOf, dropWhileEnd)

collect :: Ord k => [(k, v)] -> [(k, [v])]
collect pairs = pairs & map (second pure) & Map.fromListWith (++) & Map.toList

replaceStub :: String -> String -> Maybe String
replaceStub _ "" = Nothing
replaceStub replacement ('$':'$':rest) = return $ replacement ++ rest
replaceStub replacement (c:rest) = (c :) <$> replaceStub replacement rest

replaceNumberStub :: String -> String -> String
replaceNumberStub _ "" = ""
replaceNumberStub replacement ('$':d:rest)
  | isDigit d = replacement ++ (d : replaceNumberStub replacement rest)
replaceNumberStub replacement (c:rest) = c : replaceNumberStub replacement rest

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left Nothing   = Left left
maybeToEither _ (Just right) = Right right

removeExtension :: String -> String -> Maybe String
removeExtension ext filename
  | ext `isSuffixOf` filename = return $ take (length filename - length ext) filename
  | otherwise = Nothing
  
extractDir :: FilePath -> FilePath
extractDir = dropWhileEnd (/= '/')