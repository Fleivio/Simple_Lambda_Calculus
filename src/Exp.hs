module Exp(Exp(..)) where

import Control.Monad.Writer

class (Eq e, Show e) => Exp e where

	(·) :: e -> e -> e

	evalStepShow :: (e -> e) -> e -> Writer [String] e
	evalStepShow f e = do
		tell [show e]
		return (f e)

	evalShow :: (e -> e) -> e -> Writer [String] e
	evalShow f e = do
		e2 <- evalStepShow f e
		if e2 == e
		then return e
		else evalShow f e2

	----------------------------------------------

	callByName :: e -> e

	fullBeta :: e -> e
	
	callByValue :: e -> e

	----------------------------------------------

	evalPrint :: (e -> e) -> e -> IO e
	evalPrint f e = do
		let (e', s) = runWriter $ evalShow f e
		putStrLn $ unlines s
		return e'

	eval :: (e -> e) -> e -> e
	eval f e | e == f e  = e
			 | otherwise = eval f $ f e
