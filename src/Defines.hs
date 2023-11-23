module Defines
  ( lId,
    lFalse,
    lTrue,
    lIf,
    lNot,
    lAnd,
    lOr,
    -- lXor,
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

import BruijnLam


lId :: BLamExp
lId = Abs (Var 0)

lFalse :: BLamExp
lFalse = Abs ( -- 1
            Abs -- 0 
            (Var 0))

lTrue :: BLamExp
lTrue = Abs (
            Abs
            (Var 1)
        )

lLst :: BLamExp
lLst = Abs (
        App (Var 0) lFalse
    )

lFst :: BLamExp
lFst = Abs (
        App (Var 0) lTrue
    )

lIf :: BLamExp
lIf = Abs ( -- 2
        Abs ( -- 1
            Abs -- 0
                (App (App (Var 2) (Var 1)) (Var 0))
        )
    )

lNot :: BLamExp
lNot = Abs -- 0
            ( App (App (App lIf (Var 0) ) lFalse) lTrue )


lAnd :: BLamExp
lAnd = Abs ( -- 1
            Abs ( -- 0
                App ( App ( App lIf (Var 0) ) (Var 1) ) lFalse
            )
        )

lOr :: BLamExp
lOr = Abs (  -- 1
        Abs ( -- 0
            App (App ( App lIf (Var 0) ) lTrue) (Var 1)
        )
    )

lZero :: BLamExp
lZero = lFalse

lOne :: BLamExp
lOne = Abs ( -- f
        Abs ( -- z
            App (Var 1) (Var 0)
        )
    )

lSum :: BLamExp
lSum = Abs ( --3 n2
        Abs ( --2 n1
            Abs ( --1 succ
                Abs ( --0 zero
                    App
                    (App (Var 3) (Var 1))
                    (App (App (Var 2) (Var 1)) (Var 0))
                )
            )
        )
    )

lSucc :: BLamExp
lSucc = Abs ( -- 2 n
            Abs ( -- 1 s
                Abs ( -- 0 z
                    App (Var 1) (App (App (Var 2) (Var 1)) (Var 0))
                )
            ) 
    )

lPair :: BLamExp
lPair = Abs (
        Abs (
            Abs (
                App (App (Var 0) (Var 2)) (Var 1) 
            )
        )
    )

lMult :: BLamExp
lMult = Abs (
        Abs (
            Abs (
                App (Var 2) (App (Var 1) (Var 0))
            )
        )
    )

lPow :: BLamExp
lPow = Abs ( -- f
        Abs ( -- z
            App (Var 1) (Var 0)
        )
    )

_zz :: BLamExp 
_zz = App (App lPair lZero) lZero

_ss :: BLamExp
_ss = Abs ( App 
                (App lPair (App lLst (Var 0)))
                (App lSucc (App lLst (Var 0) ))
        )

lPred :: BLamExp
lPred = Abs (
            App lFst (App (App (Var 0) _ss) _zz) 
    )

lSub :: BLamExp
lSub = Abs (
        Abs (
            App (App (Var 0) lPred) (Var 1) 
        )
    )

lIsZro :: BLamExp
lIsZro = Abs (
        App (App (Var 0) (Abs lFalse)) lTrue
    )


lEq :: BLamExp
lEq = 
    Abs (
        Abs(
            (App (App lAnd
                (App lIsZro (App (App lSub (Var 0)) (Var 1))))
                (App lIsZro (App (App lSub (Var 1)) (Var 0))))
        )
    )

-- ---[Logic]-----------------------------------------------------------------------

-- lId :: BLamExp
-- lId = Abs (Var 0)

-- lFalse :: BLamExp
-- lFalse =
--   Abs $ -- 1
--       Abs $ -- 0 
--         Var 0

-- lTrue :: BLamExp
-- lTrue =
--   Abs $
--      Abs $
--        Var 1


-- lIf :: BLamExp
-- lIf =
--   Abs $ -- 2 
--       Abs $ -- 1 
--           Abs $ -- 0
--              (Var 2 .: Var 1) .: Var 0

-- lNot :: BLamExp
-- lNot =
--   Abs $ -- 0
--     ((lIf .: Var 0) .: lFalse) .: lTrue

-- lAnd :: BLamExp
-- lAnd =
--   Abs $ -- 1 
--       Abs $ -- 
--           ((lIf .: Var 0) .: Var 1) .: lFalse

-- lOr :: BLamExp
-- lOr =
--   Abs $ -- 1 
--       Abs $ -- 0
--           ((lIf .: Var 0) .: lTrue) .: Var 1

-- lXor :: BLamExp
-- lXor =
--   Abs $ -- 1 
--       Abs $ -- 0
--           (Var 1 .: (lNot .: Var 0)) .: Var 0

-- ---[Numeric]-----------------------------------------------------------------------------

-- lZero :: BLamExp
-- lZero = lFalse

-- lOne :: BLamExp
-- lOne =
--   Abs $ -- f 
--       Abs $ -- z
--           Var 1 .: Var 0

-- lSum :: BLamExp
-- lSum =
--   Abs $ -- 3 
--       Abs $ -- 2
--           Abs $ -- 1 
--               Abs $ -- 0 
--                     (Var 3 .: Var 1)
--                     `App`
--                     ((Var 2 .: Var 1) .: Var 0)

-- lSucc :: BLamExp
-- lSucc =
--   Abs $ -- 2 n
--       Abs $ -- 1 s 
--           Abs $ -- 0 z
--               Var 1 .: ((Var 2 .: Var 1) .: Var 0)

-- lMult :: BLamExp
-- lMult =
--   Abs $
--     Abs $
--       Abs $
--           Var 2 .: (Var 1 .: Var 0)


-- lPow :: BLamExp
-- lPow =
--   Abs $ 
--       Abs $ 
--           Var 1 .: Var 0

-- _zz :: BLamExp
-- _zz = (lPair .: lZero) .: lZero

-- _ss :: BLamExp
-- _ss =
--   Abs $
--     App
--       (lPair .: (lLst .: Var 0))
--       (lSucc .: (lLst .: Var 0))

-- lPred :: BLamExp
-- lPred =
--   Abs $
--     lFst .: ((Var 0 .: _ss) .: _zz)

-- lSub :: BLamExp
-- lSub =
--   Abs $
--     Abs $
--        (Var 0 .: lPred) .: Var 1

-- lIsZro :: BLamExp
-- lIsZro =
--   Abs $
--     (Var 0 .: Abs lFalse) .: lTrue

-- lEq :: BLamExp
-- lEq =
--   Abs $
--     Abs $
--           ( lAnd .:
--               (lIsZro .: ((lSub .: Var 0) .: Var 1))
--           )
--           `App`
--           (lIsZro .: ((lSub .: Var 1) .: Var 0))

-- ---[Pair]-----------------------------------------------------------------

-- lPair :: BLamExp
-- lPair =
--   Abs $
--     Abs $
--       Abs $
--         (Var 0 .: Var 2) .: Var 1
            
-- lLst :: BLamExp
-- lLst =
--   Abs $
--     Var 0 .: lFalse
    
-- lFst :: BLamExp
-- lFst =
--   Abs $
--     Var 0 .: lTrue
    