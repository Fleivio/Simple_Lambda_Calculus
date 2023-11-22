module NamedLam (LamExp (..), eval) where

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
freevars ex = case ex of
  (LamVar x)     -> [x]
  (LamAbs x t1)  -> remove (freevars t1) x
  (LamApp t1 t2) -> freevars t1 ++ freevars t2

boundvars :: LamExp -> [Char]
boundvars ex = case ex of
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

evalStep :: LamExp -> LamExp
evalStep ex = case ex of
  (LamApp (LamAbs x t1) v2)
    | isVal v2 -> subs x v2 t1
  LamApp t1 t2 -> LamApp (evalStep t1) (evalStep t2)
  a            -> a

eval :: LamExp -> LamExp
eval e | e == evalStep e = e 
			 | otherwise       = eval . evalStep $ e 
----------------------------------------------------------------------------

tru = LamAbs 't' (LamAbs 'f' (LamVar 't'))
fls = LamAbs 't' (LamAbs 'f' (LamVar 'f'))
test = LamAbs 'l' (LamAbs 'm' (LamAbs 'n' (LamApp (LamApp (LamVar 'l') (LamVar 'm')) (LamVar 'n'))))

---------------------------------------------------------
-- liberação de variavel
_test1 :: LamExp
_test1 = subs 'x' (LamVar 'y') (LamAbs 'x' (LamVar 'y'))

-- captura de variavel
_test2 :: LamExp
_test2 = subs 'x' (LamVar 'z') (LamAbs 'z' (LamVar 'x'))