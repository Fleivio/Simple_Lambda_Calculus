module Parser.Lexer(Token(..), lexer) where

import Data.Char

data Token 
		= TokenVar Char
		| TokenPoint
		| TokenOB
		| TokenCB
		| TokenLam 
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | c == '.'  = TokenPoint : lexer cs
    | c == '('  = TokenOB : lexer cs
    | c == ')'  = TokenCB : lexer cs
    | isAlpha c = let (a,rest) = span isAlpha (c:cs)
                  in case a of
                      "lam" -> TokenLam : lexer rest
                      _     -> TokenVar c : lexer rest 
    | otherwise = lexer cs