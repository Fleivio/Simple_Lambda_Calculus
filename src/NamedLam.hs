module NamedLam (LamExp (..), module Exp) where

import Exp

data LamExp = 
		LamVar Char
  | LamAbs Char LamExp
  | LamApp LamExp LamExp
  deriving (Eq)

instance Show LamExp where
  show (LamVar x) = [x]
  show (LamAbs x t1) = "\\" ++ [x] ++ "." ++ show t1
  show (LamApp t1 t2) = "((" ++ show t1 ++ ") " ++ show t2 ++ ")"

freevars :: LamExp -> [Char]
freevars = \case
  (LamVar x)     -> [x]
  (LamAbs x t1)  -> remove (freevars t1) x
  (LamApp t1 t2) -> freevars t1 ++ freevars t2

boundvars :: LamExp -> [Char]
boundvars = \case
  (LamVar _)     -> []
  (LamAbs x t1)  -> x : boundvars t1
  (LamApp t1 t2) -> freevars t1 <> freevars t2

remove :: [Char] -> Char -> [Char]
remove [] _ = []
remove (a : x) v
  | a == v    = x
  | otherwise = a : remove x v

subs :: Char -> LamExp -> LamExp -> LamExp
subs x s (LamVar y)
  | x == y    = s
  | otherwise = LamVar y
subs x s (LamAbs y t2)
  | x == y              = LamAbs y t2
  | y `elem` freevars s = subs x s k
  | otherwise           = LamAbs y (subs x s t2)
  where
    k = let j = genNewVarName (LamAbs y t2) s
        in LamAbs j (subs y (LamVar j) t2)
subs x s (LamApp t1 t2) = LamApp (subs x s t1) (subs x s t2)

genNewVarName :: LamExp -> LamExp -> Char
genNewVarName w k = head
    [ x | x <- ['a' ..], x `notElem` freevars w ++ boundvars w, x `notElem` freevars k]

isVal :: LamExp -> Bool
isVal (LamApp _ _) = False
isVal _ = True

instance Exp LamExp where
  callByValue = \case
    LamApp p@(LamAbs x t) v
      | isVal v   -> subs x v t
      | otherwise -> LamApp p (callByValue v) 
    LamApp t1 t2  -> LamApp (callByValue t1) t2
    a             -> a

  fullBeta = \case
    LamApp (LamAbs x t) v -> subs x v t
    LamApp t            p -> LamApp (fullBeta t) (fullBeta p)
    LamAbs x            t -> LamAbs x (fullBeta t)
    x                     -> x

  callByName = \case
    LamApp (LamAbs x t) v -> subs x v t
    LamApp t            p -> LamApp (callByName t) p
    a                     -> a