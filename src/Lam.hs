module Lam(LamExp(..), freevars, remove, subs, eval') where

data LamExp = LamVar Char
            | LamAbs Char LamExp
            | LamApp LamExp LamExp deriving(Eq)

instance Show LamExp where
    show (LamVar x) = [x]
    show (LamAbs x t1) = "λ" ++ [x] ++ "." ++ show t1
    show (LamApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

freevars :: LamExp -> [Char]
freevars ex = case ex of
            (LamVar x)     -> [x]
            (LamAbs x t1)  -> remove (freevars t1) x
            (LamApp t1 t2) -> freevars t1 ++ freevars t2


remove :: [Char] -> Char -> [Char]
remove [] _ = []
remove (a:x) v | a == v    = x
               | otherwise = a : remove x v

-- [x -> s] \y. t2 
subs :: Char -> LamExp -> LamExp -> LamExp
subs x s (LamVar y) | x == y    = s 
                    | otherwise = LamVar y

subs x s (LamAbs y t2) | x == y    
                       || y `elem` freevars s = LamAbs y t2 
                       | otherwise = LamAbs y (subs x s t2)

subs x s (LamApp t1 t2) = LamApp (subs x s t1) (subs x s t2)

eval :: LamExp -> LamExp
eval ex = case ex of
        (LamApp (LamAbs x t1) t2) -> subs x (eval t2) (eval t1)
        (LamApp t1 t2)            -> LamApp (eval t1) (eval t2)
        _ -> ex

eval' :: LamExp -> LamExp
eval' e | e /= eval e = eval' . eval $ e
        | otherwise   = eval e

----------------------------------------------------------------------------

--liberação de variavel
_test1 :: LamExp 
_test1 = subs 'x' (LamVar 'y') (LamAbs 'x' (LamVar 'y'))

--captura de variavel
_test2 :: LamExp
_test2 = subs 'x' (LamVar 'z') (LamAbs 'z' (LamVar 'x')) 