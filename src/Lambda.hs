module Lambda(Term (..), betaReduct, eval) where

data Term =
      Var Int
    | Abs Term
    | App Term Term
    deriving (Eq)

instance Show Term where
  show t = case t of
    Var x       -> show x
    Abs t1      -> "λ" ++ show t1 ++ "."
    App t1 t2   -> "(" ++ show t1 ++ " " ++ show t2 ++ ")"

shift :: Int -> Term -> Term
shift d = walk 0
    where walk c t = case t of
                Var x -> if x >= c
                        then Var ( x + d )
                        else Var x
                Abs t1 -> Abs (walk (c+1) t1)
                App t1 t2 -> App (walk c t1) (walk c t2)

subst :: Int -> Term -> Term -> Term
subst j s = walk 0
    where walk c t = case t of
                Var x -> if x == j + c
                         then shift (c + 1) s
                         else Var x
                Abs t1 -> Abs (walk (c + 1) t1)
                App t1 t2 -> App (walk c t1) (walk c t2)

betaReduct :: Term -> Term -> Term
betaReduct s t = shift (-1) (subst 0 (shift 0 s) t)

evalRun :: Term -> Term
evalRun (App (Abs a) b) = betaReduct b a
evalRun (App a b)       = App (eval a) (eval b)
evalRun (Abs a)         = Abs (eval a)
evalRun t = t 

eval :: Term -> Term 
eval x
    | x == evalRun x = x
    | otherwise = eval $ evalRun x