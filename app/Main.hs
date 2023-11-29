{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import qualified Parser.ParserLambda as P
import BruijnLam
import NamedLam
import Defines
import Conversor

evaluatorRunner :: (LamExp -> LamExp) -> IO ()
evaluatorRunner f = do
    putStr ">"
    a <- getLine
    evalPrint f $ P.parseFull a
    evaluatorRunner f


main :: IO ()
main = do
    -- evaluatorRunner
    putStrLn "Call By Name:"
    evalPrint callByName $ convert a0
    evalPrint callByName a0

    putStrLn "Call By Value:"
    evalPrint callByValue $ convert a0
    evalPrint callByValue a0

    putStrLn "Full Beta:"
    evalPrint fullBeta $ convert a0
    evalPrint fullBeta a0

    -- evalPrint b2
    -- evalPrint $ convert b2

    -- evalPrint b3
    -- evalPrint $ convert b3

a0 = lId .: (lId .: (Abs (lId .: Var 0)))

b1 = App (App (App lIf lTrue) (Var 10)) (Var 1000)
b2 = App (App lSum lOne) lOne
b3 = App (App lPow (lN 4)) lTwo `App` Var 0 `App` Var 1

t1 = LamAbs 'a' (LamAbs 'b' (LamVar 'a'))
t2 = LamApp (LamApp (LamAbs 'i' (LamAbs 'j' (LamVar 'i')) ) (LamVar 'x')) (LamVar 'y')