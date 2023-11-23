module Main (main) where

-- import qualified Parser.ParserLambda as P
-- import NamedLam

-- evaluatorRunner :: IO ()
-- evaluatorRunner = do
--     putStr ">"
--     a <- getLine
--     evalPrint . P.parseFull $ a
--     evaluatorRunner

import BruijnLam
import NamedLam
import Defines

main :: IO ()
main = do
    evalPrint t1
    putStrLn "----------------"
    evalPrint t2

t1 = App (App (App lIf lTrue) (Var 100)) (Var 10000)
t2 = LamApp (LamApp (LamAbs 'i' (LamAbs 'j' (LamVar 'i')) ) (LamVar 'x')) (LamVar 'y')