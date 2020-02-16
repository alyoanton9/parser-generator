module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--dorithgasdflk filename = do
--  contents <- getContents filename
--  let gen = generate contents
--  
--  gen & either (putStrLn) (writeFile (filename + ".hs"))