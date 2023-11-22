{-#LANGUAGE LambdaCase#-}
module NamelessLam(Exp(..), shift) where

data Exp = 
      Var Int
    | App Exp Exp
    | Abs Exp
    deriving (Eq, Show)

shift :: Int -> Int -> Exp -> Exp
shift ctf inc = \case
    (Var i) | i >= ctf  -> Var (i + inc)
            | otherwise -> Var i
    App t1 t2 -> shift ctf inc t1 `App` shift ctf inc t2
    Abs body  -> Abs $ shift (ctf + 1) inc body  

subst :: Int -> Exp -> Exp -> Exp
subst j s = \case 
    (Var i) | i == j    -> Var j
            | otherwise -> Var i
    App t1 t2  -> subst j s t1 `App` subst j s t2
    (Abs body) -> Abs $ subst (j+1) (shift 0 1 s) body

betaReduction :: Exp -> Exp -> Exp
betaReduction = subst 0 

evalStep :: Exp -> Exp
evalStep (App (Abs b) t2) = betaReduction b t2
evalStep (App t1 t2)      = evalStep t1 `App` evalStep t2
evalStep a = a

eval :: Exp -> Exp
eval a | a == evalStep a = a
       | otherwise = eval . evalStep $ a

test = (Abs (Var 0)) `App` (Var 10)