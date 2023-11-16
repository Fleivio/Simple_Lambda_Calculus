module Main (main) where

import qualified Parser.ParserLambda as P
import Lam
import Defines

evaluatorRunner :: IO ()
evaluatorRunner = do
    putStr ">"
    a <- getLine
    print . eval . P.parseFull $ a
    evaluatorRunner

main :: IO ()
main = do
    evaluatorRunner

testExp :: IO()
testExp = evalPrint $ app (app (app lIf lFalse) (vr 'h')) (vr 'j')