{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import qualified Parser.ParserLambda as P
import BruijnLam
import NamedLam
import Defines
import Conversor

import System.IO (hFlush, stdout)


evaluatorRunner :: (BLamExp -> BLamExp) -> IO ()
evaluatorRunner f = do
	putStr "> "
	hFlush stdout
	a <- getLine
	e' <- evalPrint f $ convert $ P.parseFull a
	print $ convert e'
	evaluatorRunner f

main :: IO ()
main = do
	-- let test = b3
	evaluatorRunner fullBeta
	-- putStrLn "Call By Name:"
	-- evalPrint callByName $ convert test
	-- evalPrint callByName test

	-- putStrLn "Call By Value:"
	-- evalPrint callByValue $ convert test
	-- evalPrint callByValue test

	-- putStrLn "Full Beta:"
	-- evalPrint fullBeta $ convert test
	-- evalPrint fullBeta test

a0 = lId · (lId · (Abs (lId · Var 0)))

b1 = lIf · lTrue · Var 10 · Var 1000
b2 = lSum · lOne · lOne
b3 = lPow · lN 4 · lTwo · Var 0 · Var 1

t1 = NAbs 'a' (NAbs 'b' (NVar 'a'))
t2 = NAbs 'i' (NAbs 'j' (NVar 'i')) · (NVar 'x') · (NVar 'y')

captura = NAbs 'y' $ NAbs 'x' (NAbs 'y' (NVar 'x')) · NVar 'y'
captura2 = NAbs 'y' $ NAbs 'x' (NAbs 'y' (NVar 'x')) · NAbs 'k' (NVar 'y')

liberacao = NAbs 'x' $ NAbs 'y' (NAbs 'x' (NVar 'y')) · NVar 'x'