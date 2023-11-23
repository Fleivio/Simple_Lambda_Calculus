module Exp(Exp(..)) where

import Control.Monad.Writer

class (Eq e, Show e) => Exp e where
    evalStep :: e -> e
    
    evalStepShow :: e -> Writer [String] e
    evalStepShow e = do
        tell [show e]
        return (evalStep e)

    evalShow :: e -> Writer [String] e
    evalShow e = do
        e2 <- evalStepShow e
        if e2 == e
            then return e
            else evalShow e2

    evalPrint :: e -> IO ()
    evalPrint e = do
        let (e1, s) = runWriter $ evalShow e
        putStr $ unlines s
        print e1

    eval :: e -> e
    eval e | e == evalStep e = e
           | otherwise       = eval $ evalStep e

