module Lam(LamExp(..), (->>), λ, vr, app, freevars, remove, subs, eval') where

data LamExp = LamVar Char
            | LamAbs Char LamExp
            | LamApp LamExp LamExp deriving(Eq)

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
    show (LamAbs x t1) = "λ" ++ [x] ++ "." ++ show t1
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

eval :: LamExp -> LamExp
eval ex = case ex of
        (LamApp (LamAbs x t1) v2)
                | isVal v2 -> subs x v2 t1
        LamApp t1 t2 -> LamApp (eval t1) (eval t2)
        a -> a

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