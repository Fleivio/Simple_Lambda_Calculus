{
module Parser.ParserLambda(parserlamb, parseFull) where
import Lam
import Parser.Lexer
}

%name parserlamb
%tokentype { Token }
%error { parseError }

%token
	lam { TokenLam } 
	var { TokenVar $$ }
	'.' { TokenPoint }
	'(' { TokenOB }
	')' { TokenCB }

%right '.'
%left APP	

%%

Term : var                   { LamVar $1 }
	 | lam var  '.' Term     { LamAbs $2 $4}
	 | Term Term  %prec APP  { LamApp $1 $2 }
	 | '(' Term ')'          { $2 }

{

parseError :: [Token] -> a
parseError b = error "Parse Error"

parseFull :: String -> LamExp
parseFull = parserlamb . lexer

main = getContents >>= print . parserlamb . lexer
}