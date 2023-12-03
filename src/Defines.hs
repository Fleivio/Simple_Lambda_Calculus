module Defines
	( lId,
		lFalse,
		lTrue,
		lIf,
		lNot,
		lAnd,
		lOr,
		lXor,
		lZero,
		lOne,
		lTwo,
		lThree,
		lN,
		lSum,
		lSucc,
		lMult,
		lPow,
		lPred,
		lSub,
		lIsZro,
		lEq,
		lPair,
		lFst,
		lLst,
	)
where

import BruijnLam


---[Logic]-----------------------------------------------------------------------

lId :: BLamExp
lId = Abs (Var 0)

lFalse :: BLamExp
lFalse =
	Abs $ -- 1
			Abs $ -- 0 
				Var 0

lTrue :: BLamExp
lTrue =
	Abs $
		 Abs $
			 Var 1


lIf :: BLamExp
lIf =
	Abs $ -- 2 
			Abs $ -- 1 
					Abs $ -- 0
						 (Var 2 · Var 1) · Var 0

lNot :: BLamExp
lNot =
	Abs $ -- 0
		((lIf · Var 0) · lFalse) · lTrue

lAnd :: BLamExp
lAnd =
	Abs $ -- 1 
			Abs $ -- 
					((lIf · Var 0) · Var 1) · lFalse

lOr :: BLamExp
lOr =
	Abs $ -- 1 
			Abs $ -- 0
					((lIf · Var 0) · lTrue) · Var 1

lXor :: BLamExp
lXor =
	Abs $ -- 1 
			Abs $ -- 0
					(Var 1 · (lNot · Var 0)) · Var 0

---[Numeric]-----------------------------------------------------------------------------

lZero :: BLamExp
lZero = lFalse

lOne :: BLamExp
lOne =
	Abs $ -- f 
			Abs $ -- z
					Var 1 · Var 0

lTwo :: BLamExp
lTwo =
		Abs $ -- f 
				Abs $ -- z
						Var 1 · (Var 1 · Var 0)

lThree :: BLamExp
lThree =
		Abs $ -- f 
				Abs $ -- z
						Var 1 · (Var 1 · (Var 1 · Var 0))

lN :: Int -> BLamExp
lN n = 
		Abs $ -- f 
				Abs $ -- z
						foldr1 (·) ((replicate n (Var 1)) ++ [Var 0])

lSum :: BLamExp
lSum =
	Abs $ -- 3 
			Abs $ -- 2
					Abs $ -- 1 
							Abs $ -- 0 
										(Var 3 · Var 1)
										`App`
										((Var 2 · Var 1) · Var 0)

lSucc :: BLamExp
lSucc =
	Abs $ -- 2 n
			Abs $ -- 1 s 
					Abs $ -- 0 z
							Var 1 · ((Var 2 · Var 1) · Var 0)

lMult :: BLamExp
lMult =
	Abs $
		Abs $
			Abs $
					Var 2 · (Var 1 · Var 0)


lPow :: BLamExp
lPow =
	Abs $ 
			Abs $ 
					Var 1 · Var 0

_zz :: BLamExp
_zz = (lPair · lZero) · lZero

_ss :: BLamExp
_ss =
	Abs $
		App
			(lPair · (lLst · Var 0))
			(lSucc · (lLst · Var 0))

lPred :: BLamExp
lPred =
	Abs $
		lFst · ((Var 0 · _ss) · _zz)

lSub :: BLamExp
lSub =
	Abs $
		Abs $
			 (Var 0 · lPred) · Var 1

lIsZro :: BLamExp
lIsZro =
	Abs $
		(Var 0 · Abs lFalse) · lTrue

lEq :: BLamExp
lEq =
	Abs $
		Abs $
					( lAnd ·
							(lIsZro · ((lSub · Var 0) · Var 1))
					)
					`App`
					(lIsZro · ((lSub · Var 1) · Var 0))

---[Pair]-----------------------------------------------------------------

lPair :: BLamExp
lPair =
	Abs $
		Abs $
			Abs $
				(Var 0 · Var 2) · Var 1
						
lLst :: BLamExp
lLst =
	Abs $
		Var 0 · lFalse
		
lFst :: BLamExp
lFst =
	Abs $
		Var 0 · lTrue
		