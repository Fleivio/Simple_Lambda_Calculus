{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Conversor(Conversor(..)) where

import NamedLam 
import BruijnLam

class Conversor a b | a -> b where
	convert :: a -> b

instance Conversor BLamExp LamExp where
	convert bjin = runConv bjin 0
		where 
		runConv e i = case e of
			Var k     -> NVar (find (i-k-1))
			App t1 t2 -> NApp (runConv t1 i) (runConv t2 i)
			Abs t1    -> NAbs (find i) (runConv t1 (i+1)) 
		find j
			| j < 0 = dict !! (abs j + 1000)
			| otherwise = dict !! j
		dict = ['a'..]

instance Conversor LamExp BLamExp where
	convert lexp = runConv lexp []
		where
		runConv e dict = case e of
			NVar k     -> Var (find k dict)
			NApp t1 t2 -> App (runConv t1 dict) (runConv t2 dict)
			NAbs k t1  -> Abs (runConv t1 (k:dict))
		find k (x:xs) = if k == x then 0 else 1 + find k xs
		find k []     = error $ "Variable not found " ++ [k]