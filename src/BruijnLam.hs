module BruijnLam(BLamExp(..), (.:), module Exp) where

import Exp

data BLamExp = 
      Var Int
    | App BLamExp BLamExp
    | Abs BLamExp
    deriving (Eq)

(.:) :: BLamExp -> BLamExp -> BLamExp
a .: b = a `App` b

instance Show BLamExp where
    show (Var k) = show k
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Abs t) = "(λ." ++ show t ++ ")"

shift :: Int -> Int -> BLamExp -> BLamExp
shift c d = \case
    (Var k) | k < c     -> Var k
            | otherwise -> Var (k + d)
    Abs t1    -> Abs $ shift (c + 1) d t1  
    App t1 t2 -> shift c d t1 `App` shift c d t2

subst :: Int -> BLamExp -> BLamExp -> BLamExp
subst j s = \case 
    (Var k) | k == j    -> s
            | otherwise -> Var k
    (Abs t1)   -> Abs $ subst (j + 1) (shift 0 1 s) t1
    App t1 t2  -> subst j s t1 `App` subst j s t2

betaReduction :: BLamExp -> BLamExp -> BLamExp
betaReduction s n = shift 0 (-1) $ subst 0 (shift 0 1 s) n

isVal :: BLamExp -> Bool
isVal (App _ _) = False
isVal _ = True

instance Exp BLamExp where 
    evalStep = \case
        (App t@(Abs b) t2)
            | isVal t2   -> betaReduction t2 b
            | otherwise  -> App t (evalStep t2)
        (App t1 t2)      -> evalStep t1 .: t2
        a                -> a