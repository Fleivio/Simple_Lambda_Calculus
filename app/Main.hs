module Main (main) where

import qualified Parser.ParserLambda as P
import Lam
import Defines

evaluatorRunner :: IO ()
evaluatorRunner = do
    putStr ">"
    a <- getLine
    print . eval' . P.parseFull $ a
    evaluatorRunner

main :: IO ()
main = do
    evaluatorRunner
    -- testExp
    -- a <- readFile "input/input4"  
    -- print $ eval' $ P.parseFull a

testExp :: IO()
testExp = print $ eval' $ app (app (app lIf lFalse) (vr 'h')) (vr 'j')