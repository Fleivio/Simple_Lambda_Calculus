module Main (main) where

import qualified Parser.ParserLambda as P
import BruijnLam
import NamedLam
import Defines
import Conversor

evaluatorRunner :: IO ()
evaluatorRunner = do
    putStr ">"
    a <- getLine
    evalPrint . P.parseFull $ a
    evaluatorRunner


main :: IO ()
main = do
    -- evaluatorRunner
    evalPrint b1
    evalPrint $ convert b1

    evalPrint b2
    evalPrint $ convert b2

    evalPrint b3
    evalPrint $ convert b3



b1 = App (App (App lIf lTrue) (Var 10)) (Var 1000)
b2 = App (App lSum lOne) lOne
b3 = App (App lPow (lN 4)) lTwo

t1 = LamAbs 'a' (LamAbs 'b' (LamVar 'a'))
t2 = LamApp (LamApp (LamAbs 'i' (LamAbs 'j' (LamVar 'i')) ) (LamVar 'x')) (LamVar 'y')