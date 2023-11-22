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

import NamelessLam

---[Logic]-----------------------------------------------------------------------

lId :: Exp
lId = Abs (Var 0)

lFalse :: Exp
lFalse =
  Abs
    ( -- 1
      Abs -- 0
        (Var 0)
    )

lTrue :: Exp
lTrue =
  Abs
    ( Abs
        (Var 1)
    )

lIf :: Exp
lIf =
  Abs
    ( -- 2
      Abs
        ( -- 1
          Abs -- 0
            (App (App (Var 2) (Var 1)) (Var 0))
        )
    )

lNot :: Exp
lNot =
  Abs -- 0
    (App (App (App lIf (Var 0)) lFalse) lTrue)

lAnd :: Exp
lAnd =
  Abs
    ( -- 1
      Abs
        ( -- 0
          App (App (App lIf (Var 0)) (Var 1)) lFalse
        )
    )

lOr :: Exp
lOr =
  Abs
    ( -- 1
      Abs
        ( -- 0
          App (App (App lIf (Var 0)) lTrue) (Var 1)
        )
    )

lXor :: Exp
lXor =
  Abs
    ( -- 1
      Abs
        ( -- 0
          App (App (Var 1) (lNot `App` (Var 0))) (Var 0)
        )
    )

---[Numeric]-----------------------------------------------------------------------------

lZero :: Exp
lZero = lFalse

lOne :: Exp
lOne =
  Abs
    ( -- f
      Abs
        ( -- z
          App (Var 1) (Var 0)
        )
    )

lSum :: Exp
lSum =
  Abs
    ( -- 3 n2
      Abs
        ( -- 2 n1
          Abs
            ( -- 1 succ
              Abs
                ( -- 0 zero
                  App
                    (App (Var 3) (Var 1))
                    (App (App (Var 2) (Var 1)) (Var 0))
                )
            )
        )
    )

lSucc :: Exp
lSucc =
  Abs
    ( -- 2 n
      Abs
        ( -- 1 s
          Abs
            ( -- 0 z
              App (Var 1) (App (App (Var 2) (Var 1)) (Var 0))
            )
        )
    )

lMult :: Exp
lMult =
  Abs
    ( Abs
        ( Abs
            ( App (Var 2) (App (Var 1) (Var 0))
            )
        )
    )

lPow :: Exp
lPow =
  Abs
    ( -- f
      Abs
        ( -- z
          App (Var 1) (Var 0)
        )
    )

_zz :: Exp
_zz = App (App lPair lZero) lZero

_ss :: Exp
_ss =
  Abs
    ( App
        (App lPair (App lLst (Var 0)))
        (App lSucc (App lLst (Var 0)))
    )

lPred :: Exp
lPred =
  Abs
    ( App lFst (App (App (Var 0) _ss) _zz)
    )

lSub :: Exp
lSub =
  Abs
    ( Abs
        ( App (App (Var 0) lPred) (Var 1)
        )
    )

lIsZro :: Exp
lIsZro =
  Abs
    ( App (App (Var 0) (Abs lFalse)) lTrue
    )

lEq :: Exp
lEq =
  Abs
    ( Abs
        ( ( App
              ( App
                  lAnd
                  (App lIsZro (App (App lSub (Var 0)) (Var 1)))
              )
              (App lIsZro (App (App lSub (Var 1)) (Var 0)))
          )
        )
    )

---[Pair]-----------------------------------------------------------------

lPair :: Exp
lPair =
  Abs
    ( Abs
        ( Abs
            ( App (App (Var 0) (Var 2)) (Var 1)
            )
        )
    )

lLst :: Exp
lLst =
  Abs
    ( App (Var 0) lFalse
    )

lFst :: Exp
lFst =
  Abs
    ( App (Var 0) lTrue
    )