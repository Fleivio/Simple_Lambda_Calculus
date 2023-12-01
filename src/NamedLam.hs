module NamedLam (LamExp (..), module Exp) where

import Exp
import Data.List (delete)

data LamExp = 
		LamVar Char
  | LamAbs Char LamExp
  | LamApp LamExp LamExp
  deriving (Eq)

instance Show LamExp where
  show = \case
    LamVar x     -> [x]
    LamAbs x t1  -> "\\" ++ [x] ++ "." ++ show t1
    LamApp t1 t2 -> "((" ++ show t1 ++ ") " ++ show t2 ++ ")"

-------------------------------------------------------

freevars :: LamExp -> [Char]
freevars = \case
  LamVar x     -> [x]
  LamAbs x t  -> delete x (freevars t)
  LamApp t p -> freevars t ++ freevars p

boundvars :: LamExp -> [Char]
boundvars = \case
  LamVar _    -> []
  LamAbs x t  -> x : boundvars t
  LamApp t p -> freevars t <> freevars p

betaRed :: Char -> LamExp -> LamExp -> LamExp
betaRed x s = \case 
  LamVar y 
    | x == y    -> s
    | otherwise -> LamVar y
  LamAbs y t2
    | x == y              -> LamAbs y t2
    | y `elem` freevars s -> betaRed x s k
    | otherwise           -> LamAbs y (betaRed x s t2)
      where
        k = let j = genNewVarName (LamAbs y t2) s
            in LamAbs j (betaRed y (LamVar j) t2)
  LamApp t1 t2 -> LamApp (betaRed x s t1) (betaRed x s t2)

genNewVarName :: LamExp -> LamExp -> Char
genNewVarName w k = head
    [ x | x <- ['a' ..], x `notElem` freevars w ++ boundvars w, x `notElem` freevars k]

isVal :: LamExp -> Bool
isVal (LamApp _ _) = False
isVal _ = True

instance Exp LamExp where
  callByValue = \case
    LamApp p@(LamAbs x t) v
      | isVal v   -> betaRed x v t
      | otherwise -> LamApp p (callByValue v) 
    LamApp t1 t2  -> LamApp (callByValue t1) t2
    a             -> a

  fullBeta = \case
    LamApp (LamAbs x t) v -> betaRed x v t
    LamApp t            p -> LamApp (fullBeta t) (fullBeta p)
    LamAbs x            t -> LamAbs x (fullBeta t)
    x                     -> x

  callByName = \case
    LamApp (LamAbs x t) v -> betaRed x v t
    LamApp t            p -> LamApp (callByName t) p
    a                     -> a