module NamedLam (LamExp (..), module Exp) where

import Exp
import Data.List (delete)

data LamExp = 
	  NVar Char
	| NAbs Char LamExp
	| NApp LamExp LamExp
	deriving (Eq)

instance Show LamExp where
	show = \case
		NVar x     -> [x]
		NAbs x t1  -> "\\" ++ [x] ++ "." ++ show t1
		NApp t1 t2 -> "((" ++ show t1 ++ ") " ++ show t2 ++ ")"

-------------------------------------------------------

freevars :: LamExp -> [Char]
freevars = \case
	NVar x   -> [x]
	NAbs x t -> delete x (freevars t)
	NApp t p -> freevars t ++ freevars p

boundvars :: LamExp -> [Char]
boundvars = \case
	NVar _   -> []
	NAbs x t -> x : boundvars t
	NApp t p -> freevars t <> freevars p

betaRed :: Char -> LamExp -> LamExp -> LamExp
betaRed x s = \case 
	NVar y 
		| x == y    -> s
		| otherwise -> NVar y
	NAbs y t2
		| x == y              -> NAbs y t2
		| y `elem` freevars s -> betaRed x s k
		| otherwise           -> NAbs y (betaRed x s t2)
			where
				k = let j = genNewVarName (NAbs y t2) s
						in NAbs j (betaRed y (NVar j) t2)
	NApp t1 t2 -> NApp (betaRed x s t1) (betaRed x s t2)

genNewVarName :: LamExp -> LamExp -> Char
genNewVarName w k = head
		[ x | x <- ['a' ..], x `notElem` freevars w ++ boundvars w, x `notElem` freevars k]

isVal :: LamExp -> Bool
isVal (NApp _ _) = False
isVal _ = True

instance Exp LamExp where
	(·) = NApp

	callByValue = \case
		NApp p@(NAbs x t) v
			| isVal v   -> betaRed x v t
			| otherwise -> NApp p (callByValue v) 
		NApp t1 t2    -> callByValue t1 · t2
		a             -> a

	fullBeta = \case
		NApp (NAbs x t) v -> betaRed x v t
		NApp t          p -> fullBeta t · fullBeta p
		NAbs x          t -> NAbs x (fullBeta t)
		x                 -> x

	callByName = \case
		NApp (NAbs x t) v -> betaRed x v t
		NApp t          p -> NApp (callByName t) p
		a                 -> a