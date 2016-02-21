{-
LdTerm ::= <var> | <num> | Int | <term> * <term>
         | Pi (<var> : <term>) <term> 
         | lambda (<var> : <term>)  <term>
         | Pi (<var> :: <term>) <term>
         | lambda (<var> :: <term>) <term>
         | <term> <term>
         | Eq(<term>, <term>, <term>)
         | refl(<term>)
         | subst(var, <term>, <term>, <term>)
Preserved words:
    Int, Bool, True, False, if, then, else, Pi, lambda, Eq, refl, subst
    fst, snd, injfst, injsnd, case, of, in, fix
!!!Notice: The algorithm requires that all bound variables have different names
and do not confilict with free variables.
-}
module LDTT
       -- ( LdTerm (..)
       -- , LdContext (..)
       -- , VarDef(..)
       -- )
       where 

import LdTyping as LT
import LdttLexer
import LdttParser

import Data.List as List hiding (union, intersection, delete, singleton, member, insert)
import Data.Map as Map hiding (union, intersection, delete, singleton, member)
import Data.Set as Set
import Text.Printf

parseContext :: String -> TE LdContext
parseContext = contextParsing . lexing

parseTerm :: String -> TE LdTerm 
parseTerm = termParsing . lexing

infer :: String -> String -> SE LdTerm
infer s_ctx s_tm = 
  evalTSE $ do ctx <- liftTE $ parseContext s_ctx
               tm <- liftTE $ parseTerm s_tm
               t <- typeInfer ctx tm
               return t

splitContext :: String -> String -> TE (LdContext, LdContext)
splitContext s_ctx s_tm =
  do ctx <- parseContext s_ctx
     tm <- parseTerm s_tm
     return $ splitLdContext ctx (freeVar tm)

checkContext :: String -> TE Bool
checkContext s_ctx =
  do ctx <- parseContext s_ctx 
     return $ isValidContext ctx

checkType :: String -> String -> TE Bool
checkType s_ctx s_tm =
  do ctx <- parseContext s_ctx
     tm <- parseTerm s_tm
     return $ isValidType ctx tm
     
ctx1 = "x1: Int; x2: Pi(z:Int)Int; x3: Eq(Int, x1, 0)"
ctx2 = "x1: Int; x2: Eq(Int,x1,x1); x3: Eq(Int,x1,x2); x4: Int; x5: Eq(Int,x4,0); x6: Eq(Int,x2,x4)"
ctx3 = "x1:: Int; x2: Pi(z::Eq(Int,x1,x1))Int; x3: Eq(Int,x1,x1)"
ctx4 = "x1: Int; x2: Pi(z:Int)Int; x1: Eq(Int, x1, 0)"
ctx5 = "x1: Pi(y::Int)Pi(z:Eq(Int,y,y))Int; x2:: Int; x3: Eq(Int,x2,x2)"

tm1 = "x1 * 2" 
tm2 = "x2 (lambda (y:Int) 0)" 
tm3 = "lambda (xx: Pi(y:Int)Int) x2"
tm4 = "x2 x3"

testTerms = [tm1, tm2, tm3]
            
-- test = 
--   let printTyping ctx exp = case infer ctx exp 
--                             of Terr s -> putStrLn $ exp ++ " : " ++ s
--                                Tok t -> putStrLn $ exp ++ " : " ++ show t
--   in do putStrLn $ "{ " ++ ctx1 ++  " }" 
--         putStrLn "--------------------------------------------------------------------------------"
--         mapM (printTyping ctx1) testTerms
