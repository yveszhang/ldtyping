module LdTyping  -- Define type substitutions as functions 
       -- ( LdTerm (..)
       -- , VarDef (..)
       -- , LdContext
       -- , typeOfVar
       -- , codomain
       -- , isDefined
       -- , freeVar
       -- , subst
       -- , reduce
       -- , alphaEq
       -- , splitContext
       -- , isValidContext
       -- , isValidType
       -- , typeInfer
       -- )
       where 

import Data.List as List hiding (union, intersection, delete, singleton, member, insert)
import Data.Map as Map hiding (union, intersection, delete, singleton, member)
import Data.Set as Set
import Text.Printf
import Control.Monad
import Control.Applicative

data TE a = Tok a | Terr String 
     deriving (Show)

instance Monad TE where 
  return x = Tok x
  m >>= k = case m of Tok x -> k x 
                      Terr s -> Terr s 

instance Functor TE where 
  fmap = liftM

instance Applicative TE where   
  pure = return 
  (<*>) = ap

-- failTE :: String -> TE a
-- failTE s = Terr s 

-- LDTT terms
data LdTerm = M_var String              -- program variable, with a string denoting the variable name
            | M_Int                   -- Int type
            | M_num Int               -- Int constants 
            | M_op LdTerm LdTerm      -- Int operations
            | M_pi String LdTerm LdTerm            -- Intuitionistic Pi-type: Pi_x:M.M
            | M_abs String LdTerm LdTerm           -- Intuitionistic lambda abstraction: \x:M.M
            | M_lpi String LdTerm LdTerm         -- Linear Pi-type: Pi_x::M.M
            | M_labs String LdTerm LdTerm        -- Linear lambda abstractions \x::M.M
            | M_app LdTerm LdTerm                  -- lambda applications M M
            | M_eq LdTerm LdTerm LdTerm            -- equality type Eq_M(M, M)
            | M_refl LdTerm                -- reflection: refl(M)
            | M_subst String LdTerm LdTerm LdTerm  -- subst(x.M, M, M)
            deriving (Eq)

data VarDef = Nvar String LdTerm | Lvar String LdTerm
            deriving (Eq) 

type LdContext = [VarDef]

instance Show (LdTerm) where
  show (M_var xx) = xx
  show M_Int = "Int"
  show (M_num n) = show n
  show (M_op e1 e2) = "(" ++ (show e1) ++ ") * (" ++ show e2 ++ ")"
  show (M_pi xx e1 e2) = "Pi(" ++ xx ++ ":" ++ show e1 ++ ")." ++ show e2
  show (M_abs xx e1 e2) = "lambda(" ++ xx ++ ":" ++ show e1 ++ ") -> " ++ show e2
  show (M_lpi xx e1 e2) = "Pi(" ++ xx ++ "::" ++ show e1 ++ ")." ++ show e2
  show (M_labs xx e1 e2) = "lambda(" ++ xx ++ "::" ++ show e1 ++ ") -> " ++ show e2
  show (M_app e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (M_eq e1 e2 e3) = "Eq(" ++ show e1 ++ ", " ++ show e2 ++ ", " ++ show e3 ++ ")"
  show (M_refl e) = "refl(" ++ show e ++ ")"
  show (M_subst xx e1 e2 e3) = "subst(" ++ xx ++ ":" ++ show e1 ++ ", " ++ show e2 ++ ", " ++ show e3 ++ ")"

instance Show (VarDef) where
  show (Nvar xx e) = xx ++ ": " ++ show e
  show (Lvar xx e) = xx ++ ":: " ++ show e

showLdContext :: LdContext -> String
showLdContext [] = ""
showLdContext [df] = show df
showLdContext (df:ctx) = (show df) ++ "; " ++ (showLdContext ctx)

-- _getVarName :: VarDef -> String
-- _getVarType :: VarDef -> LdTerm
-- _isLinear :: VarDef -> Bool

-- typeOfVar :: LdContext -> String -> TE LdTerm
-- codomain :: LdContext -> [String]
-- isDefined :: String -> LdContext -> Bool
-- freeVar :: LdTerm -> [String]
-- subst :: LdTerm -> String -> LdTerm -> LdTerm 
-- reduce :: LdTerm -> LdTerm
-- alphaEq :: LdTerm -> LdTerm -> Bool

-- _isValidContextR :: LdContext -> Bool
-- _isTypeR :: LdContext -> LdTerm -> Bool
-- _typeInferR :: LdContext -> LdTerm -> TE LdTerm

_getVarName :: VarDef -> String
_getVarName (Nvar xx _) = xx
_getVarName (Lvar xx _) = xx

_getVarType :: VarDef -> LdTerm
_getVarType (Nvar _ t) = t
_getVarType (Lvar _ t) = t

_isLinear (Nvar _ _) = False
_isLinear (Lvar _ _) = True

typeOfVar :: LdContext -> String -> TE LdTerm
typeOfVar [] xx = Terr $ printf "Variable %s is not defined" xx
typeOfVar (vdef : ctx) xx = if (xx == _getVarName vdef) then Tok (_getVarType vdef) else typeOfVar ctx xx

codomain :: LdContext -> [String]
codomain [] = []
codomain (vdef : ctx) = (_getVarName vdef) : (codomain ctx)

isDefined :: String -> LdContext -> Bool
isDefined xx ctx = xx `elem` (codomain ctx)


_checkDefinedR :: String -> LdContext -> TE ()
_checkDefinedR xx ctx =
  if xx `elem` (codomain $ reverse ctx)
  then Tok ()
  else Terr $ printf "Variable %s is not defined in context %s" xx (showLdContext $ reverse ctx)

freeVar :: LdTerm -> Set String
freeVar (M_var xx) = singleton xx
freeVar M_Int = Set.empty
freeVar (M_num _) = Set.empty
freeVar (M_op e1 e2) = freeVar e1 `union` freeVar e2
freeVar (M_pi xx e1 e2) = delete xx (freeVar e2)
freeVar (M_abs xx e1 e2) = delete xx (freeVar e2)  
freeVar (M_lpi xx e1 e2) = delete xx (freeVar e2)
freeVar (M_labs xx e1 e2) = delete xx (freeVar e2)
freeVar (M_app e1 e2) = freeVar e1 `union` freeVar e2
freeVar (M_eq e1 e2 e3) = freeVar e2 `union` freeVar e3
freeVar (M_refl e) = freeVar e
freeVar (M_subst xx e1 e2 e3) = freeVar e2 `union` freeVar e3

subst :: LdTerm -> String -> LdTerm -> LdTerm 
subst e@(M_var xx) yy e' = if xx == yy then e' else e
subst e@(M_op e1 e2) yy e' = M_op (subst e1 yy e') (subst e2 yy e')
subst e@(M_pi xx e1 e2) yy e' = if xx == yy then e else M_pi xx (subst e1 yy e') (subst e2 yy e')
subst e@(M_abs xx e1 e2) yy e' = if xx == yy then e else M_abs xx (subst e1 yy e') (subst e2 yy e')
subst e@(M_lpi xx e1 e2) yy e' = if xx == yy then e else M_lpi xx (subst e1 yy e') (subst e2 yy e')
subst e@(M_labs xx e1 e2) yy e' = if xx == yy then e else M_labs xx (subst e1 yy e') (subst e2 yy e')
subst e@(M_app e1 e2) yy e' = M_app (subst e1 yy e') (subst e2 yy e')
subst e@(M_eq e1 e2 e3) yy e' = M_eq (subst e1 yy e') (subst e2 yy e') (subst e3 yy e')
subst e@(M_refl e1) yy e' = M_refl (subst e1 yy e')
subst e@(M_subst xx e1 e2 e3) yy e' = if xx == yy then e else M_subst xx (subst e1 yy e') (subst e2 yy e') (subst e3 yy e')
subst e _ _ = e

reduce :: LdTerm -> LdTerm
reduce (M_op e1 e2) = 
  case (reduce e1, reduce e2) of (M_num n1, M_num n2) -> M_num (n1 * n2)
                                 (n1, n2) -> M_op n1 n2
reduce (M_app e1 e2) =
  case reduce e1 of M_pi xx _ ee -> reduce (subst ee xx e2)
                    M_abs xx _ ee -> reduce (subst ee xx e2)
                    M_lpi xx _ ee -> reduce (subst ee xx e2)
                    M_labs xx _ ee -> reduce (subst ee xx e2)
                    e1' -> M_app e1' e2
reduce (M_eq e1 e2 e3) = M_eq (reduce e1) (reduce e2) (reduce e3) 
reduce (M_refl e) = M_refl (reduce e) 
reduce (M_subst xx e1 e2 e3) = M_subst xx (reduce e1) (reduce e2) (reduce e3)
reduce e = e 

alphaEq :: LdTerm -> LdTerm -> Bool
alphaEq (M_var xx) (M_var yy) = (xx == yy)
alphaEq M_Int M_Int = True
alphaEq (M_num n1) (M_num n2) = (n1 == n2)
alphaEq (M_op e1 e2) (M_op f1 f2) = alphaEq e1 f1 && alphaEq e2 f2
alphaEq (M_pi xx e1 e2) (M_pi yy f1 f2) =
  if xx == yy
  then alphaEq e1 f1 && alphaEq e2 f2
  else alphaEq e1 f1 && alphaEq e2 (subst f2 yy (M_var xx))
alphaEq (M_abs xx e1 e2) (M_abs yy f1 f2) =
  if xx == yy
  then alphaEq e1 f1 && alphaEq e2 f2
  else alphaEq e1 f1 && alphaEq e2 (subst f2 yy (M_var xx))
alphaEq (M_lpi xx e1 e2) (M_lpi yy f1 f2) =
  if xx == yy
  then alphaEq e1 f1 && alphaEq e2 f2
  else alphaEq e1 f1 && alphaEq e2 (subst f2 yy (M_var xx))
alphaEq (M_labs xx e1 e2) (M_labs yy f1 f2) =
  if xx == yy
  then alphaEq e1 f1 && alphaEq e2 f2
  else alphaEq e1 f1 && alphaEq e2 (subst f2 yy (M_var xx))
alphaEq (M_app e1 e2) (M_app f1 f2) = alphaEq e1 f1 && alphaEq e2 f2
alphaEq (M_eq e1 e2 e3) (M_eq f1 f2 f3) = alphaEq e1 f1 && alphaEq e2 f2 && alphaEq e3 f3
alphaEq (M_refl e) (M_refl f) = alphaEq e f 
alphaEq (M_subst xx e1 e2 e3) (M_subst yy f1 f2 f3) =
  if xx == yy
  then alphaEq e1 f1 && alphaEq e2 f2 && alphaEq e3 f3
  else alphaEq e1 (subst f1 yy (M_var xx)) && alphaEq e2 f2 && alphaEq e3 f3
alphaEq _ _ = False

_linearVars :: LdContext -> (Set String)
_linearVars ctx = Set.fromList $ List.map _getVarName (List.filter _isLinear ctx)

_linearVarsInTerm :: LdTerm -> LdContext -> (Set String)
_linearVarsInTerm e ctx = (freeVar e) `intersection` (_linearVars ctx)

_linearDefined :: String -> LdContext -> Bool
_linearDefined xx ctx = xx `member` (_linearVars ctx)

_ldVarsR :: LdContext -> (Set String)
_ldVarsR [] = Set.empty
_ldVarsR ((Lvar xx e):ctx) = Set.insert xx (_ldVarsR ctx)
_ldVarsR ((Nvar xx e):ctx) =
  let vs = _ldVarsR ctx in
  if (freeVar e) `intersection` vs == Set.empty then vs else Set.insert xx vs

_ldvCompute :: (Map String (Set String)) -> VarDef -> (Map String (Set String))
_ldvCompute f (Nvar xx e) =
  let getLdVars = (\x->findWithDefault Set.empty x f) in  -- String -> Set String
  let ldv_xx = Set.foldl Set.union Set.empty (Set.map getLdVars (freeVar e)) in
  if ldv_xx == Set.empty then Map.insert xx ldv_xx f else Map.insert xx (Set.insert xx ldv_xx) f
_ldvCompute f (Lvar xx e) =
  let getLdVars = (\x->findWithDefault Set.empty x f) in  -- String -> Set String
  let ldv_xx = Set.foldl Set.union Set.empty (Set.map getLdVars (freeVar e)) in
  Map.insert xx (Set.insert xx ldv_xx) f

_allLdVarsR :: LdContext -> (Map String (Set String))
_allLdVarsR ctx = List.foldl _ldvCompute Map.empty (reverse ctx)

_splitContextR :: LdContext -> Set String -> (LdContext, LdContext)
_splitContextR ctx vs =
  let ldMap = \x -> findWithDefault Set.empty x (_allLdVarsR ctx) in -- String -> Set String
  let ldvs = Set.foldl Set.union Set.empty (Set.map ldMap vs) in -- Set String
  let f1 = (\x -> let (xx, t) = (_getVarName x, _getVarType x)
                  in ldMap xx == Set.empty || xx `member` ldvs || (freeVar t) `intersection` ldvs /= Set.empty ) in
  let f2 = (\x -> let (xx, t) = (_getVarName x, _getVarType x)
                  in ldMap xx == Set.empty || (not (xx `member` ldvs) && (freeVar t) `intersection` ldvs == Set.empty) ) in
  (List.filter f1 ctx, List.filter f2 ctx)

splitLdContext :: LdContext -> Set String -> (LdContext, LdContext)
splitLdContext ctx vs = let (c1, c2) = _splitContextR (reverse ctx) vs in (reverse c1, reverse c2)

_showTypingR :: LdContext -> LdTerm -> String
_showTypingR ctx e = (showLdContext $ reverse ctx) ++ " |- " ++ (show e) 

_checkContextR :: LdContext -> TE ()
_checkContextR [] = Tok ()
_checkContextR (df:ctx) = do _ <- let xx = _getVarName df
                                  in if isDefined xx ctx
                                     then Terr $ printf "%s is already defined in %s" xx (showLdContext $ reverse ctx)
                                     else _checkTypeR ctx (_getVarType df)
                             return ()

_checkTypeR :: LdContext -> LdTerm -> TE ()
_checkTypeR ctx M_Int = _checkContextR ctx
_checkTypeR ctx e@(M_pi xx e1 e2) =
  do _ <- _checkContextR ctx
     _ <- _checkTypeR ctx e1
     _ <- if (isDefined xx ctx)
          then Terr $ printf "T_pi {{ %s }} %s is already defined" (_showTypingR ctx e) xx
          else _checkTypeR ((Nvar xx e1):ctx) e2
     return ()
_checkTypeR ctx e@(M_lpi xx e1 e2) = 
  do _ <- _checkContextR ctx
     _ <- _checkTypeR ctx e1
     _ <- if (isDefined xx ctx)
          then Terr $ printf "T_lpi {{ %s }} %s is already defined" (_showTypingR ctx e) xx
          else _checkTypeR ((Lvar xx e1):ctx) e2
     return ()
_checkTypeR ctx e@(M_eq t e1 e2) =
  do _ <- _checkContextR ctx
     _ <- _checkTypeR ctx t
     t1 <- let (ctx1, _) = _splitContextR ctx (freeVar e1) in _typeInferR ctx1 e1
     t2 <- let (ctx2, _) = _splitContextR ctx (freeVar e2) in _typeInferR ctx2 e2
     _ <- if not $ alphaEq (reduce t) (reduce t1)
          then Terr ( printf "T_eq {{ %s }} %s has incosistent type %s" 
                      (_showTypingR ctx e) (show e1) (show t1) 
                    )
          else if not $ alphaEq (reduce t) (reduce t2)
               then Terr ( printf "T_eq {{ %s }} %s has inconsistent type %s" 
                           (_showTypingR ctx e) (show e2) (show t2) 
                         )
               else Tok ()
     return ()
_checkTypeR ctx e = Terr $ printf "{{ %s }}" (_showTypingR ctx e)

_typeInferR :: LdContext -> LdTerm -> TE LdTerm
_typeInferR ctx e@(M_Int) = do _ <- _checkTypeR ctx e
                               return e
_typeInferR ctx e@(M_pi _ _ _) = do _ <- _checkTypeR ctx e
                                    return e
_typeInferR ctx e@(M_lpi _ _ _) = do _ <- _checkTypeR ctx e
                                     return e
_typeInferR ctx e@(M_eq _ _ _) = do _ <- _checkTypeR ctx e
                                    return e
------------
_typeInferR ctx e@(M_var xx) =
  do _ <- _checkContextR ctx
     t <- if (_linearDefined xx ctx && _linearVars ctx == singleton xx)
             || (not (_linearDefined xx ctx) && _linearVars ctx == Set.empty) 
          then typeOfVar ctx xx
          else Terr $ printf "T_var {{ %s }} unused linear resource" (_showTypingR ctx e)
     return t
_typeInferR ctx e@(M_num _) = do _ <- _checkContextR ctx
                                 return M_Int
_typeInferR ctx e@(M_op e1 e2) =
  do t1 <- _typeInferR ctx e1
     t2 <- _typeInferR ctx e2
     t <- if t1 /= M_Int
          then Terr ( printf "T_op {{ %s }} %s has type %s (expecting Int)" 
                      (_showTypingR ctx e) (show e1) (show t1) 
                    )
          else if t2 /= M_Int
               then Terr ( printf "T_op {{ %s }} %s has type %s (expecting Int)" 
                            (_showTypingR ctx e) (show e2) (show t2)
                         )
               else Tok M_Int
     return t
_typeInferR ctx e@(M_abs xx t e0) =
  if isDefined xx ctx
  then Terr $ printf "T_abs {{ %s }} %s is already defined" (_showTypingR ctx e) xx
  else do _ <- _checkTypeR ctx t
          t' <- _typeInferR ((Nvar xx t):ctx) e0
          return (M_pi xx t t')
_typeInferR ctx e@(M_labs xx t e0) =
  if (isDefined xx ctx)
  then Terr $ printf "T_abs {{ %s }} %s is already defined" (_showTypingR ctx e) xx
  else do _ <- _checkTypeR ctx t
          t' <- _typeInferR ((Lvar xx t):ctx) e0
          return (M_lpi xx t t')
_typeInferR ctx e@(M_app e1 e2) =
  let (ctx1, ctx2) =  if freeVar e2 == Set.empty then (ctx, ctx) else _splitContextR ctx (freeVar e1) in
  do t1 <- _typeInferR ctx1 e1
     t2 <- _typeInferR ctx2 e2
     t <- let t1' = reduce t1
          in ( case t1'
               of M_pi xx t11 t12 ->
                    if alphaEq (reduce t2) (reduce t11) 
                    then if (_linearVarsInTerm e2 ctx)== Set.empty
                         then Tok (subst t12 xx e2)
                         else Terr $ printf "T_app {{ %s }} %s has linear variables" (_showTypingR ctx e) (show e2)
                    else Terr $ printf "T_app {{ %s }} %s %s" (_showTypingR ctx e) (show t1) (show t2)
                  M_lpi xx t11 t12 -> 
                    if alphaEq (reduce t2) (reduce t11)
                    then Tok (subst t12 xx e2)
                    else Terr $ printf "T_app {{ %s }} %s %s" (_showTypingR ctx e) (show t1) (show t2)
                  _ ->
                    Terr $ printf "T_app {{ %s }} %s %s" (_showTypingR ctx e) (show t1) (show t2)
             )
     return t
_typeInferR ctx e@(M_refl e0) =
  do t <- _typeInferR ctx e0
     return (M_eq t e0 e0)
_typeInferR ctx e@(M_subst xx t e1 e2) =
  if (isDefined xx ctx) 
  then Terr $ printf "T_subst {{ %s }} %s is already defined" (_showTypingR ctx e) xx
  else do t1 <- _typeInferR ctx e1
          t2 <- _typeInferR ctx e2
          t <- ( case reduce t1
                 of M_eq t' a b -> ( case _checkTypeR ((Nvar xx t'):ctx) t
                                     of Tok _ -> if alphaEq (reduce t2) (reduce (subst t xx a))
                                                 then Tok (subst t xx b)
                                                 else Terr $ printf "T_subst {{ %s }}" (_showTypingR ctx e)
                                        _ -> Terr $ printf "T_subst {{ %s }}" (_showTypingR ctx e)
                                   )
                    _ -> Terr $ printf "T_subst {{ %s }}" (_showTypingR ctx e)
               )
          return t

isValidContext :: LdContext -> TE Bool
isValidContext ctx = do _ <- _checkContextR (reverse ctx)
                        return True

isValidType :: LdContext -> LdTerm -> TE Bool
isValidType ctx t = do _ <-  _checkTypeR (reverse ctx) t 
                       return True

typeInfer :: LdContext -> LdTerm -> TE LdTerm
typeInfer ctx e = _typeInferR (reverse ctx) e

     
