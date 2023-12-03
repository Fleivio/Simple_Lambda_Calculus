module BruijnLam(BLamExp(..), module Exp) where

import Exp

data BLamExp = 
	  Var Int
	| App BLamExp BLamExp
	| Abs BLamExp
	deriving (Eq)

instance Show BLamExp where
	show = \case
		Var k   -> show k
		Abs t   -> "(λ." ++ show t ++ ")"
		App t p -> "(" ++ show t ++ " " ++ show p ++ ")"

shift :: Int -> Int -> BLamExp -> BLamExp
shift c d = \case
	Var k | k < c     -> Var k
				| otherwise -> Var (k + d)
	Abs t1    -> Abs $ shift (c + 1) d t1  
	App t1 t2 -> shift c d t1 `App` shift c d t2

subst :: Int -> BLamExp -> BLamExp -> BLamExp
subst j s = \case 
	Var k | k == j    -> s
				| otherwise -> Var k
	Abs t1    -> Abs $ subst (j + 1) (shift 0 1 s) t1
	App t1 t2 -> subst j s t1 `App` subst j s t2

betaRed :: BLamExp -> BLamExp -> BLamExp
betaRed s n = shift 0 (-1) $ subst 0 (shift 0 1 s) n

isVal :: BLamExp -> Bool
isVal (App _ _) = False
isVal _ = True

instance Exp BLamExp where 
	(·) = App

	callByValue = \case
		App p@(Abs t) v
				| isVal v    -> betaRed v t
				| otherwise  -> App p (callByValue v)
		App t1 t2        -> callByValue t1 · t2
		a                -> a

	fullBeta = \case
		App (Abs b) t -> betaRed t b
		App t       p -> App (fullBeta t) (fullBeta p)
		Abs b         -> Abs (fullBeta b)
		a             -> a

	callByName = \case
		App (Abs b) t -> betaRed t b 
		App t       p -> App (callByName t) p
		a             -> a