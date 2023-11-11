module Main (main) where

import qualified Parser.ParserLambda as P
import Lam
import Defines

main :: IO ()
main = do
    testExp
    -- a <- readFile "input/input4"  
    -- print $ eval' $ P.parseFull a

testExp :: IO()
testExp = print $ eval' $ LamApp (LamApp (LamApp lIf lFalse) (LamVar 'h')) (LamVar 'j')