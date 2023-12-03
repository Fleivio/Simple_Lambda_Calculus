{
module Parser.ParserLambda(parserlamb, parseFull) where
import NamedLam
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
%nonassoc lam var '(' ')'
%left APP	

%%

Term : var                   { LamVar $1 }
	 | lam var  '.' Term     { LamAbs $2 $4 }
	 | Term Term %prec APP   { LamApp $1 $2 }
	 | '(' Term ')'          { $2 }

{

parseError :: [Token] -> a
parseError b = error "Parse Error"

parseFull :: String -> LamExp
parseFull = parserlamb . lexer

}
