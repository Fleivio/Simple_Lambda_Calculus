module Defines(
    lId, lFalse, lTrue,
    lIf, lNot, lAnd, lOr, lXor,
    
    lZero, lOne, lSum, lSucc, lMult, lPow,
    lPred, lSub, lIsZro, lEq,

    lPair, lFst, lLst
    ) where

import Lam

(->>) :: Char -> LamExp -> LamExp
a ->> b = LamAbs a b
infixr 1 ->>

vr :: Char -> LamExp
vr = LamVar

app :: LamExp -> LamExp -> LamExp
app = LamApp

---[Logic]-----------------------------------------------------------------------

lId :: LamExp
lId = 'a' ->>
             vr 'a'

lFalse :: LamExp
lFalse = 'a' ->>
         'b' ->>
            vr 'b'

lTrue :: LamExp
lTrue =  'a' ->>
         'b' ->>
            vr 'a'

lIf :: LamExp
lIf = 'a' ->>
      'b' ->>
      'c' ->>
        app (app (vr 'a') (vr 'b') ) (vr 'c')

lNot :: LamExp
lNot = 'a' ->>
        app (app (app lIf (vr 'a')) lFalse ) lTrue

lAnd :: LamExp
lAnd =   'a' ->>
         'b' ->>
            app (app (app lIf (vr 'b')) (vr 'a')) lFalse

lOr :: LamExp
lOr =  'a' ->>
       'b' ->>
            app (app (app lIf (vr 'b')) lTrue) (vr 'a')

lXor :: LamExp
lXor = 'a' ->>
       'b' ->>
            app (app (vr 'a') (app lNot (vr 'b')) ) (vr 'b')

---[Numeric]-----------------------------------------------------------------------------

lZero :: LamExp
lZero = lFalse

lOne :: LamExp
lOne = 'f' ->>
       'z' ->> 
            app (vr 'f') (vr 'z')

lSum :: LamExp
lSum = 'n' ->>
       'm' ->>
       'f' ->>
       'z' ->>
        app
            (app (vr 'n') (vr 'f') )
            (app (app (vr 'm') (vr 'f')) (vr 'z'))

lSucc :: LamExp
lSucc = 'n' ->>
        'f' ->>
        'z' ->>
            app (vr 'f') (app (app (vr 'n') (vr 'f')) (vr 'z') )

lMult :: LamExp
lMult = 'n' ->>
        'f' ->>
        'z' ->>
            app (vr 'n') (app (vr 'f') (vr 'z'))

lPow :: LamExp
lPow = 'f' ->>
       'z' ->>
            app (vr 'f') (vr 'z')

lIsZro :: LamExp
lIsZro = 'n' ->>
            app (app (vr 'n') (LamAbs '_' lFalse)) lTrue


_zz :: LamExp
_zz = app (app lPair lZero) lZero

_ss :: LamExp
_ss = 'a' ->>
        app
            (app lPair (app lLst (vr 'a')))
            (app lSucc (app lLst (vr 'a')))

lPred :: LamExp
lPred = 'n' ->>
            app lFst (app (app (vr 'n') _ss) _zz)

lSub :: LamExp
lSub = 'n' ->>
       'm' ->>
            app (app (vr 'm') lPred) (vr 'n')

lEq :: LamExp
lEq = 'n' ->>
      'm' ->>
        app (app lAnd
                (app lIsZro (app (app lSub (vr 'm')) (vr 'n'))))
                (app lIsZro (app (app lSub (vr 'n')) (vr 'm')))

---[Pair]-----------------------------------------------------------------

lPair :: LamExp
lPair = 'a' ->>
        'b' ->>
        'c' ->>
            app (app (vr 'c') (vr 'a')) (vr 'b')

lLst :: LamExp
lLst = 'a' ->>
            app (vr 'a') lFalse

lFst :: LamExp
lFst = 'a' ->>
            app (vr 'a') lTrue
