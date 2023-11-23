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
import Conversor

main :: IO ()
main = do
    print $ convert lIf
    print $ convert lOne
    print $ convert lSum

    print $ convert t1
    -- evalPrint b1
    -- evalPrint b2
    -- evalPrint b3
    -- evalPrint t2


b1 = App (App (App lIf lTrue) (Var 100)) (Var 10000)
b2 = App (App lSum lOne) lOne
b3 = App (App lPow (lN 4)) lTwo

t1 = LamAbs 'a' (LamAbs 'b' (LamVar 'a'))
t2 = LamApp (LamApp (LamAbs 'i' (LamAbs 'j' (LamVar 'i')) ) (LamVar 'x')) (LamVar 'y')