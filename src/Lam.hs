module Lam(LamExp(..), (->>), λ, vr, app, eval, evalPrint, evalW) where

import Control.Monad.Writer

data LamExp = LamVar Char
            | LamAbs Char LamExp
            | LamApp LamExp LamExp 
             deriving(Eq)

(->>) :: Char -> LamExp -> LamExp
a ->> b = LamAbs a b
infixr 1 ->>

λ :: Char -> LamExp -> LamExp
λ = (->>)

vr :: Char -> LamExp
vr = LamVar

app :: LamExp -> LamExp -> LamExp
app = LamApp

instance Show LamExp where
    show (LamVar x) = [x]
    show (LamAbs x t1) = "\\" ++ [x] ++ "." ++ show t1
    show (LamApp t1 t2) = "((" ++ show t1 ++ ") " ++ show t2 ++ ")" 

freevars :: LamExp -> [Char]
freevars ex = case ex of
            (LamVar x)     -> [x]
            (LamAbs x t1)  -> remove (freevars t1) x
            (LamApp t1 t2) -> freevars t1 ++ freevars t2

boundvars :: LamExp -> [Char]
boundvars ex = case ex of
        (LamVar _) -> []
        (LamAbs x t1) -> x : boundvars t1
        (LamApp t1 t2) -> freevars t1 <> freevars t2

remove :: [Char] -> Char -> [Char]
remove [] _ = []
remove (a:x) v | a == v    = x
               | otherwise = a : remove x v

subs :: Char -> LamExp -> LamExp -> LamExp
subs x s (LamVar y) | x == y    = s 
                    | otherwise = LamVar y

subs x s (LamAbs y t2) | x == y              = LamAbs y t2 
                       | y `elem` freevars s = subs x s k 
                       | otherwise           = LamAbs y (subs x s t2)
        where
                k = let j = genNewVarName (LamAbs y t2) s 
                    in LamAbs j (subs y (LamVar j) t2)
                

subs x s (LamApp t1 t2) = LamApp (subs x s t1) (subs x s t2)

genNewVarName :: LamExp -> LamExp -> Char
genNewVarName w k = head [x | x <- ['a'..]
                          , x `notElem` freevars w ++ boundvars w
                          , x `notElem` freevars k]


isVal :: LamExp -> Bool
isVal (LamApp _ _ ) = False
isVal _ = True

type ExpW = Writer [String] LamExp

eval' :: LamExp -> ExpW
eval' ex = case ex of
        (LamApp (LamAbs x t1) v2)
                | isVal v2 -> do 
                        tell [show ex ++ " betaReduction"]
                        return $ subs x v2 t1
        LamApp t1 t2 -> do 
                        tell [show ex]
                        e1 <- eval' t1
                        e2 <- eval' t2
                        return $ LamApp e1 e2
        a -> return a

evalW :: LamExp -> ExpW
evalW e = do 
        ne <- eval' e
        if e /= ne 
        then  evalW ne
        else  eval' e

evalPrint :: LamExp -> IO ()
evalPrint ex = do
        let (v, log') = runWriter $ evalW ex 
        putStrLn $ unlines log'
        print v

eval :: LamExp -> LamExp
eval ex = 
        let (v, _) = runWriter $ evalW ex 
        in v

----------------------------------------------------------------------------

--liberação de variavel
_test1 :: LamExp 
_test1 = subs 'x' (LamVar 'y') (LamAbs 'x' (LamVar 'y'))

--captura de variavel
_test2 :: LamExp
_test2 = subs 'x' (LamVar 'z') (LamAbs 'z' (LamVar 'x')) 