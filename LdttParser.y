{
module LdttParser 
       ( Prog (..)
       , termParsing 
       , contextParsing
       ) where 

import LdttLexer
import LdTyping

import System.IO
import Text.Printf

failE :: String -> TE a
failE s = Terr s 

catchE :: TE a -> (String -> TE a) -> TE a
catchE m f = 
  case m of Tok a -> Tok a
            Terr s -> f s

data Prog = Pvar String
  | Pint
  | Pnum Int
  | Pop Prog Prog
  | Ppi String Prog Prog
  | Pabs String Prog Prog 
  | Plpi String Prog Prog
  | Plabs String Prog Prog 
  | Papp Prog Prog
  | Peq Prog Prog Prog
  | Prefl Prog
  | Psubst String Prog Prog Prog
  deriving (Eq, Show)

makeApps :: [Prog] -> Prog
makeApps [p] = p
makeApps (p1 : p2 : ps) = makeApps ((Papp p1 p2) : ps)

progToLdTerm :: Prog -> LdTerm 
progToLdTerm (Pvar s) = M_var s
progToLdTerm Pint = M_Int
progToLdTerm (Pnum n) = M_num n
progToLdTerm (Pop e1 e2) = M_op (progToLdTerm e1) (progToLdTerm e2)
progToLdTerm (Ppi str e1 e2) = M_pi str (progToLdTerm e1) (progToLdTerm e2)
progToLdTerm (Pabs str e1 e2) = M_abs str (progToLdTerm e1) (progToLdTerm e2)
progToLdTerm (Plpi str e1 e2) = M_lpi str (progToLdTerm e1) (progToLdTerm e2)
progToLdTerm (Plabs str e1 e2) = M_labs str (progToLdTerm e1) (progToLdTerm e2)
progToLdTerm (Papp e1 e2) = M_app (progToLdTerm e1) (progToLdTerm e2)
progToLdTerm (Peq e1 e2 e3) = M_eq (progToLdTerm e1) (progToLdTerm e2) (progToLdTerm e3)
progToLdTerm (Prefl e1) = M_refl (progToLdTerm e1)
progToLdTerm (Psubst str e1 e2 e3) = M_subst str (progToLdTerm e1) (progToLdTerm e2) (progToLdTerm e3)
}

%name termParsing ldterm
%name contextParsing ldcontext
%tokentype { Token }
%monad { TE }
%error { parseError }

%token
  ':'            { COLON $$ }
  "::"           { BICOLON $$ }
  ';'            { SEMICOLON $$ }
  ','            { COMMA $$ }
  "*"            { STAR $$ }
  "+"            { PLUS $$ }
  "=="           { EQUAL $$ }
  "->"           { ARROW $$ }
  '('            { LPAREN $$ }
  ')'            { RPAREN $$ }
  '<'            { LPAIR $$ }
  '>'            { RPAIR $$ }
  '['            { LBOX $$ }
  ']'            { RBOX $$ }
  "id"           { IDENT p s } 
  "idcap"        { IDENTCAP p s } 
  "num"          { NUMBER p n } 
  "Int"          { INT $$ }
  "Bool"         { BOOL $$ }
  "True"         { TRUE $$ }
  "False"        { FALSE $$ }
  "if"           { IF $$ }
  "then"         { THEN $$ }
  "else"         { ELSE $$ }
  "Pi"           { PI $$ }
  "lambda"       { LAMBDA $$ }
  "Eq"           { EQT $$ }
  "refl"         { REFL $$ }
  "subst"        { SUBST $$ }
  "fst"          { PROJFST $$ }
  "snd"          { PROJSND $$ }
  "injfst"       { INJFST $$ }
  "injsnd"       { INJSND $$ }
  "case"         { CASE $$ }
  "of"           { OF $$ }
  "in"           { IN $$ }
  "fix"          { FIX $$ }
  "fcend"        { FCEND }
  "fcstart"      { FCSTART }

%right "->"
%left '='
%left "+"
%left "*"

%%

ldcontext :: { LdContext }
          : {- empty -} { [] } 
          | "id" ':' ldterm { let IDENT _ s = $1 in [(Nvar s $3)] }
          | "id" "::" ldterm { let IDENT _ s = $1 in [(Lvar s $3)] }
          | ldcontext ';' "id" ':' ldterm { let IDENT _ s = $3 in $1 ++ [(Nvar s $5)] }
          | ldcontext ';' "id" "::" ldterm { let IDENT _ s = $3 in $1 ++ [(Lvar s $5)] }

ldterm :: { LdTerm }
       : progseq                         { progToLdTerm (makeApps $1) }

progseq :: { [Prog] }
        : progton                      { [$1] }
        | progseq progton              { $1 ++ [$2] }   

progton :: { Prog }
: "id"                              { let IDENT p s = $1 in Pvar s }
| "Int"                             { Pint }
| "num"                             { let NUMBER p n = $1 in Pnum n }
| progseq "*" progseq               { Pop (makeApps $1) (makeApps $3) }
| '(' progseq ')'                   { makeApps $2 }
| "Pi" '(' "id" ':' progseq ')' progseq           { let IDENT _ s = $3 in Ppi s (makeApps $5) (makeApps $7)}
| "Pi" '(' "id" "::" progseq ')' progseq           { let IDENT _ s = $3 in Plpi s (makeApps $5) (makeApps $7)}
| "lambda" '(' "id" ':' progseq ')' progseq       { let IDENT _ s = $3 in Pabs s (makeApps $5) (makeApps $7)}
| "lambda" '(' "id" "::" progseq ')' progseq       { let IDENT _ s = $3 in Plabs s (makeApps $5) (makeApps $7)}
| "Eq" '(' progseq ',' progseq ',' progseq ')' { Peq (makeApps $3) (makeApps $5) (makeApps $7)}
| "refl" '(' progseq ')'                       { Prefl (makeApps $3)}
| "subst" '(' "id" ',' progseq ',' progseq ',' progseq ')' 
{ let IDENT _ s = $3 in Psubst s (makeApps $5) (makeApps $7) (makeApps $9)}


{
parseError [] = failE "Parse error!"
parseError (tok : ts) = failE $ printf "Error: %s (%s)" (show tok) (show ts)

}
