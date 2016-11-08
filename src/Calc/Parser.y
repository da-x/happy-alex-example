{
module Calc.Parser where

import Calc.Base
import Calc.Data
}

%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }
%errorhandlertype explist
%error { handleErrorExpList }

%token
        let             { Token _ TokenLet      }
        in              { Token _ TokenIn       }
        int             { Token _ (TokenInt $$) }
        var             { Token _ (TokenVar $$) }
        '+'             { Token _ TokenPlus     }
        '-'             { Token _ TokenMinus    }
        '*'             { Token _ TokenTimes    }
        '/'             { Token _ TokenDiv      }
        '='             { Token _ TokenEq       }
        '('             { Token _ TokenOB       }
        ')'             { Token _ TokenCB       }

%%

Exp :: {Exp}
     : let var '=' Exp in Exp	{ Let (tokenToPosN $1) $2 $4 $6 }
     | Exp1			{ Exp1 $1 }

Exp1 :: {Exp1}
      : Exp1 '+' Term		{ Plus $1 $3 }
      | Exp1 '-' Term		{ Minus $1 $3 }
      | Term			{ Term $1 }

Term :: {Term}
      : Term '*' Factor 	{ Times $1 $3 }
      | Term '/' Factor	        { Div $1 $3 }
      | Factor			{ Factor $1 }


Factor :: {Factor}
        : int			{ Int $1 }
        | var			{ Var $1 }
        | '(' Exp ')'		{ Brack $2 }

{}
