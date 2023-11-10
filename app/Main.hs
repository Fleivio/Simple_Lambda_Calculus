module Main (main) where

import qualified Parser.ParserLambda as P

main :: IO ()
main = do
    a <- readFile "input/input4"  
    print $ P.parseFull a