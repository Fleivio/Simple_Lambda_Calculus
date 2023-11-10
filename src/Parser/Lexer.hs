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
    | isSpace c = lexer cs
    | c == '.'  = TokenPoint : lexer cs
    | c == '('  = TokenOB : lexer cs
    | c == ')'  = TokenCB : lexer cs
    | isAlpha c = let (a,rest) = span isAlpha (c:cs)
                  in if (a == "lam") then TokenLam : lexer rest
                     else (TokenVar c) : lexer rest 