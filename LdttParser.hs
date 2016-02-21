{-# OPTIONS_GHC -w #-}
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

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 (LdContext)
	| HappyAbsSyn6 (LdTerm)
	| HappyAbsSyn7 ([Prog])
	| HappyAbsSyn8 (Prog)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71 :: () => Int -> ({-HappyReduction (TE) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (TE) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (TE) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (TE) HappyAbsSyn)

happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21 :: () => ({-HappyReduction (TE) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (TE) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (TE) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (TE) HappyAbsSyn)

action_0 (17) = happyShift action_8
action_0 (23) = happyShift action_9
action_0 (25) = happyShift action_10
action_0 (26) = happyShift action_11
action_0 (33) = happyShift action_12
action_0 (34) = happyShift action_13
action_0 (35) = happyShift action_14
action_0 (36) = happyShift action_15
action_0 (37) = happyShift action_16
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 _ = happyFail

action_1 (23) = happyShift action_4
action_1 (5) = happyGoto action_3
action_1 _ = happyReduce_2

action_2 _ = happyFail

action_3 (11) = happyShift action_28
action_3 (48) = happyAccept
action_3 _ = happyFail

action_4 (9) = happyShift action_26
action_4 (10) = happyShift action_27
action_4 _ = happyFail

action_5 (48) = happyAccept
action_5 _ = happyFail

action_6 (13) = happyShift action_25
action_6 (17) = happyShift action_8
action_6 (23) = happyShift action_9
action_6 (25) = happyShift action_10
action_6 (26) = happyShift action_11
action_6 (33) = happyShift action_12
action_6 (34) = happyShift action_13
action_6 (35) = happyShift action_14
action_6 (36) = happyShift action_15
action_6 (37) = happyShift action_16
action_6 (7) = happyGoto action_23
action_6 (8) = happyGoto action_24
action_6 _ = happyReduce_7

action_7 _ = happyReduce_8

action_8 (17) = happyShift action_8
action_8 (23) = happyShift action_9
action_8 (25) = happyShift action_10
action_8 (26) = happyShift action_11
action_8 (33) = happyShift action_12
action_8 (34) = happyShift action_13
action_8 (35) = happyShift action_14
action_8 (36) = happyShift action_15
action_8 (37) = happyShift action_16
action_8 (7) = happyGoto action_22
action_8 (8) = happyGoto action_7
action_8 _ = happyFail

action_9 _ = happyReduce_10

action_10 _ = happyReduce_12

action_11 _ = happyReduce_11

action_12 (17) = happyShift action_21
action_12 _ = happyFail

action_13 (17) = happyShift action_20
action_13 _ = happyFail

action_14 (17) = happyShift action_19
action_14 _ = happyFail

action_15 (17) = happyShift action_18
action_15 _ = happyFail

action_16 (17) = happyShift action_17
action_16 _ = happyFail

action_17 (23) = happyShift action_38
action_17 _ = happyFail

action_18 (17) = happyShift action_8
action_18 (23) = happyShift action_9
action_18 (25) = happyShift action_10
action_18 (26) = happyShift action_11
action_18 (33) = happyShift action_12
action_18 (34) = happyShift action_13
action_18 (35) = happyShift action_14
action_18 (36) = happyShift action_15
action_18 (37) = happyShift action_16
action_18 (7) = happyGoto action_37
action_18 (8) = happyGoto action_7
action_18 _ = happyFail

action_19 (17) = happyShift action_8
action_19 (23) = happyShift action_9
action_19 (25) = happyShift action_10
action_19 (26) = happyShift action_11
action_19 (33) = happyShift action_12
action_19 (34) = happyShift action_13
action_19 (35) = happyShift action_14
action_19 (36) = happyShift action_15
action_19 (37) = happyShift action_16
action_19 (7) = happyGoto action_36
action_19 (8) = happyGoto action_7
action_19 _ = happyFail

action_20 (23) = happyShift action_35
action_20 _ = happyFail

action_21 (23) = happyShift action_34
action_21 _ = happyFail

action_22 (13) = happyShift action_25
action_22 (17) = happyShift action_8
action_22 (18) = happyShift action_33
action_22 (23) = happyShift action_9
action_22 (25) = happyShift action_10
action_22 (26) = happyShift action_11
action_22 (33) = happyShift action_12
action_22 (34) = happyShift action_13
action_22 (35) = happyShift action_14
action_22 (36) = happyShift action_15
action_22 (37) = happyShift action_16
action_22 (7) = happyGoto action_23
action_22 (8) = happyGoto action_24
action_22 _ = happyFail

action_23 (13) = happyShift action_25
action_23 (17) = happyShift action_8
action_23 (23) = happyShift action_9
action_23 (25) = happyShift action_10
action_23 (26) = happyShift action_11
action_23 (33) = happyShift action_12
action_23 (34) = happyShift action_13
action_23 (35) = happyShift action_14
action_23 (36) = happyShift action_15
action_23 (37) = happyShift action_16
action_23 (7) = happyGoto action_23
action_23 (8) = happyGoto action_24
action_23 _ = happyFail

action_24 (13) = happyReduce_9
action_24 (17) = happyReduce_9
action_24 (23) = happyReduce_9
action_24 (25) = happyReduce_9
action_24 (26) = happyReduce_9
action_24 (33) = happyReduce_9
action_24 (34) = happyReduce_9
action_24 (35) = happyReduce_9
action_24 (36) = happyReduce_9
action_24 (37) = happyReduce_9
action_24 _ = happyReduce_9

action_25 (17) = happyShift action_8
action_25 (23) = happyShift action_9
action_25 (25) = happyShift action_10
action_25 (26) = happyShift action_11
action_25 (33) = happyShift action_12
action_25 (34) = happyShift action_13
action_25 (35) = happyShift action_14
action_25 (36) = happyShift action_15
action_25 (37) = happyShift action_16
action_25 (7) = happyGoto action_32
action_25 (8) = happyGoto action_7
action_25 _ = happyFail

action_26 (17) = happyShift action_8
action_26 (23) = happyShift action_9
action_26 (25) = happyShift action_10
action_26 (26) = happyShift action_11
action_26 (33) = happyShift action_12
action_26 (34) = happyShift action_13
action_26 (35) = happyShift action_14
action_26 (36) = happyShift action_15
action_26 (37) = happyShift action_16
action_26 (6) = happyGoto action_31
action_26 (7) = happyGoto action_6
action_26 (8) = happyGoto action_7
action_26 _ = happyFail

action_27 (17) = happyShift action_8
action_27 (23) = happyShift action_9
action_27 (25) = happyShift action_10
action_27 (26) = happyShift action_11
action_27 (33) = happyShift action_12
action_27 (34) = happyShift action_13
action_27 (35) = happyShift action_14
action_27 (36) = happyShift action_15
action_27 (37) = happyShift action_16
action_27 (6) = happyGoto action_30
action_27 (7) = happyGoto action_6
action_27 (8) = happyGoto action_7
action_27 _ = happyFail

action_28 (23) = happyShift action_29
action_28 _ = happyFail

action_29 (9) = happyShift action_46
action_29 (10) = happyShift action_47
action_29 _ = happyFail

action_30 _ = happyReduce_4

action_31 _ = happyReduce_3

action_32 (17) = happyShift action_8
action_32 (23) = happyShift action_9
action_32 (25) = happyShift action_10
action_32 (26) = happyShift action_11
action_32 (33) = happyShift action_12
action_32 (34) = happyShift action_13
action_32 (35) = happyShift action_14
action_32 (36) = happyShift action_15
action_32 (37) = happyShift action_16
action_32 (7) = happyGoto action_23
action_32 (8) = happyGoto action_24
action_32 _ = happyReduce_13

action_33 _ = happyReduce_14

action_34 (9) = happyShift action_44
action_34 (10) = happyShift action_45
action_34 _ = happyFail

action_35 (9) = happyShift action_42
action_35 (10) = happyShift action_43
action_35 _ = happyFail

action_36 (12) = happyShift action_41
action_36 (13) = happyShift action_25
action_36 (17) = happyShift action_8
action_36 (23) = happyShift action_9
action_36 (25) = happyShift action_10
action_36 (26) = happyShift action_11
action_36 (33) = happyShift action_12
action_36 (34) = happyShift action_13
action_36 (35) = happyShift action_14
action_36 (36) = happyShift action_15
action_36 (37) = happyShift action_16
action_36 (7) = happyGoto action_23
action_36 (8) = happyGoto action_24
action_36 _ = happyFail

action_37 (13) = happyShift action_25
action_37 (17) = happyShift action_8
action_37 (18) = happyShift action_40
action_37 (23) = happyShift action_9
action_37 (25) = happyShift action_10
action_37 (26) = happyShift action_11
action_37 (33) = happyShift action_12
action_37 (34) = happyShift action_13
action_37 (35) = happyShift action_14
action_37 (36) = happyShift action_15
action_37 (37) = happyShift action_16
action_37 (7) = happyGoto action_23
action_37 (8) = happyGoto action_24
action_37 _ = happyFail

action_38 (12) = happyShift action_39
action_38 _ = happyFail

action_39 (17) = happyShift action_8
action_39 (23) = happyShift action_9
action_39 (25) = happyShift action_10
action_39 (26) = happyShift action_11
action_39 (33) = happyShift action_12
action_39 (34) = happyShift action_13
action_39 (35) = happyShift action_14
action_39 (36) = happyShift action_15
action_39 (37) = happyShift action_16
action_39 (7) = happyGoto action_55
action_39 (8) = happyGoto action_7
action_39 _ = happyFail

action_40 _ = happyReduce_20

action_41 (17) = happyShift action_8
action_41 (23) = happyShift action_9
action_41 (25) = happyShift action_10
action_41 (26) = happyShift action_11
action_41 (33) = happyShift action_12
action_41 (34) = happyShift action_13
action_41 (35) = happyShift action_14
action_41 (36) = happyShift action_15
action_41 (37) = happyShift action_16
action_41 (7) = happyGoto action_54
action_41 (8) = happyGoto action_7
action_41 _ = happyFail

action_42 (17) = happyShift action_8
action_42 (23) = happyShift action_9
action_42 (25) = happyShift action_10
action_42 (26) = happyShift action_11
action_42 (33) = happyShift action_12
action_42 (34) = happyShift action_13
action_42 (35) = happyShift action_14
action_42 (36) = happyShift action_15
action_42 (37) = happyShift action_16
action_42 (7) = happyGoto action_53
action_42 (8) = happyGoto action_7
action_42 _ = happyFail

action_43 (17) = happyShift action_8
action_43 (23) = happyShift action_9
action_43 (25) = happyShift action_10
action_43 (26) = happyShift action_11
action_43 (33) = happyShift action_12
action_43 (34) = happyShift action_13
action_43 (35) = happyShift action_14
action_43 (36) = happyShift action_15
action_43 (37) = happyShift action_16
action_43 (7) = happyGoto action_52
action_43 (8) = happyGoto action_7
action_43 _ = happyFail

action_44 (17) = happyShift action_8
action_44 (23) = happyShift action_9
action_44 (25) = happyShift action_10
action_44 (26) = happyShift action_11
action_44 (33) = happyShift action_12
action_44 (34) = happyShift action_13
action_44 (35) = happyShift action_14
action_44 (36) = happyShift action_15
action_44 (37) = happyShift action_16
action_44 (7) = happyGoto action_51
action_44 (8) = happyGoto action_7
action_44 _ = happyFail

action_45 (17) = happyShift action_8
action_45 (23) = happyShift action_9
action_45 (25) = happyShift action_10
action_45 (26) = happyShift action_11
action_45 (33) = happyShift action_12
action_45 (34) = happyShift action_13
action_45 (35) = happyShift action_14
action_45 (36) = happyShift action_15
action_45 (37) = happyShift action_16
action_45 (7) = happyGoto action_50
action_45 (8) = happyGoto action_7
action_45 _ = happyFail

action_46 (17) = happyShift action_8
action_46 (23) = happyShift action_9
action_46 (25) = happyShift action_10
action_46 (26) = happyShift action_11
action_46 (33) = happyShift action_12
action_46 (34) = happyShift action_13
action_46 (35) = happyShift action_14
action_46 (36) = happyShift action_15
action_46 (37) = happyShift action_16
action_46 (6) = happyGoto action_49
action_46 (7) = happyGoto action_6
action_46 (8) = happyGoto action_7
action_46 _ = happyFail

action_47 (17) = happyShift action_8
action_47 (23) = happyShift action_9
action_47 (25) = happyShift action_10
action_47 (26) = happyShift action_11
action_47 (33) = happyShift action_12
action_47 (34) = happyShift action_13
action_47 (35) = happyShift action_14
action_47 (36) = happyShift action_15
action_47 (37) = happyShift action_16
action_47 (6) = happyGoto action_48
action_47 (7) = happyGoto action_6
action_47 (8) = happyGoto action_7
action_47 _ = happyFail

action_48 _ = happyReduce_6

action_49 _ = happyReduce_5

action_50 (13) = happyShift action_25
action_50 (17) = happyShift action_8
action_50 (18) = happyShift action_61
action_50 (23) = happyShift action_9
action_50 (25) = happyShift action_10
action_50 (26) = happyShift action_11
action_50 (33) = happyShift action_12
action_50 (34) = happyShift action_13
action_50 (35) = happyShift action_14
action_50 (36) = happyShift action_15
action_50 (37) = happyShift action_16
action_50 (7) = happyGoto action_23
action_50 (8) = happyGoto action_24
action_50 _ = happyFail

action_51 (13) = happyShift action_25
action_51 (17) = happyShift action_8
action_51 (18) = happyShift action_60
action_51 (23) = happyShift action_9
action_51 (25) = happyShift action_10
action_51 (26) = happyShift action_11
action_51 (33) = happyShift action_12
action_51 (34) = happyShift action_13
action_51 (35) = happyShift action_14
action_51 (36) = happyShift action_15
action_51 (37) = happyShift action_16
action_51 (7) = happyGoto action_23
action_51 (8) = happyGoto action_24
action_51 _ = happyFail

action_52 (13) = happyShift action_25
action_52 (17) = happyShift action_8
action_52 (18) = happyShift action_59
action_52 (23) = happyShift action_9
action_52 (25) = happyShift action_10
action_52 (26) = happyShift action_11
action_52 (33) = happyShift action_12
action_52 (34) = happyShift action_13
action_52 (35) = happyShift action_14
action_52 (36) = happyShift action_15
action_52 (37) = happyShift action_16
action_52 (7) = happyGoto action_23
action_52 (8) = happyGoto action_24
action_52 _ = happyFail

action_53 (13) = happyShift action_25
action_53 (17) = happyShift action_8
action_53 (18) = happyShift action_58
action_53 (23) = happyShift action_9
action_53 (25) = happyShift action_10
action_53 (26) = happyShift action_11
action_53 (33) = happyShift action_12
action_53 (34) = happyShift action_13
action_53 (35) = happyShift action_14
action_53 (36) = happyShift action_15
action_53 (37) = happyShift action_16
action_53 (7) = happyGoto action_23
action_53 (8) = happyGoto action_24
action_53 _ = happyFail

action_54 (12) = happyShift action_57
action_54 (13) = happyShift action_25
action_54 (17) = happyShift action_8
action_54 (23) = happyShift action_9
action_54 (25) = happyShift action_10
action_54 (26) = happyShift action_11
action_54 (33) = happyShift action_12
action_54 (34) = happyShift action_13
action_54 (35) = happyShift action_14
action_54 (36) = happyShift action_15
action_54 (37) = happyShift action_16
action_54 (7) = happyGoto action_23
action_54 (8) = happyGoto action_24
action_54 _ = happyFail

action_55 (12) = happyShift action_56
action_55 (13) = happyShift action_25
action_55 (17) = happyShift action_8
action_55 (23) = happyShift action_9
action_55 (25) = happyShift action_10
action_55 (26) = happyShift action_11
action_55 (33) = happyShift action_12
action_55 (34) = happyShift action_13
action_55 (35) = happyShift action_14
action_55 (36) = happyShift action_15
action_55 (37) = happyShift action_16
action_55 (7) = happyGoto action_23
action_55 (8) = happyGoto action_24
action_55 _ = happyFail

action_56 (17) = happyShift action_8
action_56 (23) = happyShift action_9
action_56 (25) = happyShift action_10
action_56 (26) = happyShift action_11
action_56 (33) = happyShift action_12
action_56 (34) = happyShift action_13
action_56 (35) = happyShift action_14
action_56 (36) = happyShift action_15
action_56 (37) = happyShift action_16
action_56 (7) = happyGoto action_67
action_56 (8) = happyGoto action_7
action_56 _ = happyFail

action_57 (17) = happyShift action_8
action_57 (23) = happyShift action_9
action_57 (25) = happyShift action_10
action_57 (26) = happyShift action_11
action_57 (33) = happyShift action_12
action_57 (34) = happyShift action_13
action_57 (35) = happyShift action_14
action_57 (36) = happyShift action_15
action_57 (37) = happyShift action_16
action_57 (7) = happyGoto action_66
action_57 (8) = happyGoto action_7
action_57 _ = happyFail

action_58 (17) = happyShift action_8
action_58 (23) = happyShift action_9
action_58 (25) = happyShift action_10
action_58 (26) = happyShift action_11
action_58 (33) = happyShift action_12
action_58 (34) = happyShift action_13
action_58 (35) = happyShift action_14
action_58 (36) = happyShift action_15
action_58 (37) = happyShift action_16
action_58 (7) = happyGoto action_65
action_58 (8) = happyGoto action_7
action_58 _ = happyFail

action_59 (17) = happyShift action_8
action_59 (23) = happyShift action_9
action_59 (25) = happyShift action_10
action_59 (26) = happyShift action_11
action_59 (33) = happyShift action_12
action_59 (34) = happyShift action_13
action_59 (35) = happyShift action_14
action_59 (36) = happyShift action_15
action_59 (37) = happyShift action_16
action_59 (7) = happyGoto action_64
action_59 (8) = happyGoto action_7
action_59 _ = happyFail

action_60 (17) = happyShift action_8
action_60 (23) = happyShift action_9
action_60 (25) = happyShift action_10
action_60 (26) = happyShift action_11
action_60 (33) = happyShift action_12
action_60 (34) = happyShift action_13
action_60 (35) = happyShift action_14
action_60 (36) = happyShift action_15
action_60 (37) = happyShift action_16
action_60 (7) = happyGoto action_63
action_60 (8) = happyGoto action_7
action_60 _ = happyFail

action_61 (17) = happyShift action_8
action_61 (23) = happyShift action_9
action_61 (25) = happyShift action_10
action_61 (26) = happyShift action_11
action_61 (33) = happyShift action_12
action_61 (34) = happyShift action_13
action_61 (35) = happyShift action_14
action_61 (36) = happyShift action_15
action_61 (37) = happyShift action_16
action_61 (7) = happyGoto action_62
action_61 (8) = happyGoto action_7
action_61 _ = happyFail

action_62 (13) = happyShift action_25
action_62 (17) = happyShift action_8
action_62 (23) = happyShift action_9
action_62 (25) = happyShift action_10
action_62 (26) = happyShift action_11
action_62 (33) = happyShift action_12
action_62 (34) = happyShift action_13
action_62 (35) = happyShift action_14
action_62 (36) = happyShift action_15
action_62 (37) = happyShift action_16
action_62 (7) = happyGoto action_23
action_62 (8) = happyGoto action_24
action_62 _ = happyReduce_16

action_63 (13) = happyShift action_25
action_63 (17) = happyShift action_8
action_63 (23) = happyShift action_9
action_63 (25) = happyShift action_10
action_63 (26) = happyShift action_11
action_63 (33) = happyShift action_12
action_63 (34) = happyShift action_13
action_63 (35) = happyShift action_14
action_63 (36) = happyShift action_15
action_63 (37) = happyShift action_16
action_63 (7) = happyGoto action_23
action_63 (8) = happyGoto action_24
action_63 _ = happyReduce_15

action_64 (13) = happyShift action_25
action_64 (17) = happyShift action_8
action_64 (23) = happyShift action_9
action_64 (25) = happyShift action_10
action_64 (26) = happyShift action_11
action_64 (33) = happyShift action_12
action_64 (34) = happyShift action_13
action_64 (35) = happyShift action_14
action_64 (36) = happyShift action_15
action_64 (37) = happyShift action_16
action_64 (7) = happyGoto action_23
action_64 (8) = happyGoto action_24
action_64 _ = happyReduce_18

action_65 (13) = happyShift action_25
action_65 (17) = happyShift action_8
action_65 (23) = happyShift action_9
action_65 (25) = happyShift action_10
action_65 (26) = happyShift action_11
action_65 (33) = happyShift action_12
action_65 (34) = happyShift action_13
action_65 (35) = happyShift action_14
action_65 (36) = happyShift action_15
action_65 (37) = happyShift action_16
action_65 (7) = happyGoto action_23
action_65 (8) = happyGoto action_24
action_65 _ = happyReduce_17

action_66 (13) = happyShift action_25
action_66 (17) = happyShift action_8
action_66 (18) = happyShift action_69
action_66 (23) = happyShift action_9
action_66 (25) = happyShift action_10
action_66 (26) = happyShift action_11
action_66 (33) = happyShift action_12
action_66 (34) = happyShift action_13
action_66 (35) = happyShift action_14
action_66 (36) = happyShift action_15
action_66 (37) = happyShift action_16
action_66 (7) = happyGoto action_23
action_66 (8) = happyGoto action_24
action_66 _ = happyFail

action_67 (12) = happyShift action_68
action_67 (13) = happyShift action_25
action_67 (17) = happyShift action_8
action_67 (23) = happyShift action_9
action_67 (25) = happyShift action_10
action_67 (26) = happyShift action_11
action_67 (33) = happyShift action_12
action_67 (34) = happyShift action_13
action_67 (35) = happyShift action_14
action_67 (36) = happyShift action_15
action_67 (37) = happyShift action_16
action_67 (7) = happyGoto action_23
action_67 (8) = happyGoto action_24
action_67 _ = happyFail

action_68 (17) = happyShift action_8
action_68 (23) = happyShift action_9
action_68 (25) = happyShift action_10
action_68 (26) = happyShift action_11
action_68 (33) = happyShift action_12
action_68 (34) = happyShift action_13
action_68 (35) = happyShift action_14
action_68 (36) = happyShift action_15
action_68 (37) = happyShift action_16
action_68 (7) = happyGoto action_70
action_68 (8) = happyGoto action_7
action_68 _ = happyFail

action_69 _ = happyReduce_19

action_70 (13) = happyShift action_25
action_70 (17) = happyShift action_8
action_70 (18) = happyShift action_71
action_70 (23) = happyShift action_9
action_70 (25) = happyShift action_10
action_70 (26) = happyShift action_11
action_70 (33) = happyShift action_12
action_70 (34) = happyShift action_13
action_70 (35) = happyShift action_14
action_70 (36) = happyShift action_15
action_70 (37) = happyShift action_16
action_70 (7) = happyGoto action_23
action_70 (8) = happyGoto action_24
action_70 _ = happyFail

action_71 _ = happyReduce_21

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (let IDENT _ s = happy_var_1 in [(Nvar s happy_var_3)]
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (let IDENT _ s = happy_var_1 in [(Lvar s happy_var_3)]
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 5 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (let IDENT _ s = happy_var_3 in happy_var_1 ++ [(Nvar s happy_var_5)]
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 5 happyReduction_6
happyReduction_6 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (let IDENT _ s = happy_var_3 in happy_var_1 ++ [(Lvar s happy_var_5)]
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (progToLdTerm (makeApps happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (let IDENT p s = happy_var_1 in Pvar s
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn8
		 (Pint
	)

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (let NUMBER p n = happy_var_1 in Pnum n
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (Pop (makeApps happy_var_1) (makeApps happy_var_3)
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (makeApps happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 7 8 happyReduction_15
happyReduction_15 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (let IDENT _ s = happy_var_3 in Ppi s (makeApps happy_var_5) (makeApps happy_var_7)
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 7 8 happyReduction_16
happyReduction_16 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (let IDENT _ s = happy_var_3 in Plpi s (makeApps happy_var_5) (makeApps happy_var_7)
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 8 happyReduction_17
happyReduction_17 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (let IDENT _ s = happy_var_3 in Pabs s (makeApps happy_var_5) (makeApps happy_var_7)
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 7 8 happyReduction_18
happyReduction_18 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (let IDENT _ s = happy_var_3 in Plabs s (makeApps happy_var_5) (makeApps happy_var_7)
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 8 8 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Peq (makeApps happy_var_3) (makeApps happy_var_5) (makeApps happy_var_7)
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 8 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Prefl (makeApps happy_var_3)
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 10 8 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (let IDENT _ s = happy_var_3 in Psubst s (makeApps happy_var_5) (makeApps happy_var_7) (makeApps happy_var_9)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 48 48 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	COLON happy_dollar_dollar -> cont 9;
	BICOLON happy_dollar_dollar -> cont 10;
	SEMICOLON happy_dollar_dollar -> cont 11;
	COMMA happy_dollar_dollar -> cont 12;
	STAR happy_dollar_dollar -> cont 13;
	PLUS happy_dollar_dollar -> cont 14;
	EQUAL happy_dollar_dollar -> cont 15;
	ARROW happy_dollar_dollar -> cont 16;
	LPAREN happy_dollar_dollar -> cont 17;
	RPAREN happy_dollar_dollar -> cont 18;
	LPAIR happy_dollar_dollar -> cont 19;
	RPAIR happy_dollar_dollar -> cont 20;
	LBOX happy_dollar_dollar -> cont 21;
	RBOX happy_dollar_dollar -> cont 22;
	IDENT p s -> cont 23;
	IDENTCAP p s -> cont 24;
	NUMBER p n -> cont 25;
	INT happy_dollar_dollar -> cont 26;
	BOOL happy_dollar_dollar -> cont 27;
	TRUE happy_dollar_dollar -> cont 28;
	FALSE happy_dollar_dollar -> cont 29;
	IF happy_dollar_dollar -> cont 30;
	THEN happy_dollar_dollar -> cont 31;
	ELSE happy_dollar_dollar -> cont 32;
	PI happy_dollar_dollar -> cont 33;
	LAMBDA happy_dollar_dollar -> cont 34;
	EQT happy_dollar_dollar -> cont 35;
	REFL happy_dollar_dollar -> cont 36;
	SUBST happy_dollar_dollar -> cont 37;
	PROJFST happy_dollar_dollar -> cont 38;
	PROJSND happy_dollar_dollar -> cont 39;
	INJFST happy_dollar_dollar -> cont 40;
	INJSND happy_dollar_dollar -> cont 41;
	CASE happy_dollar_dollar -> cont 42;
	OF happy_dollar_dollar -> cont 43;
	IN happy_dollar_dollar -> cont 44;
	FIX happy_dollar_dollar -> cont 45;
	FCEND -> cont 46;
	FCSTART -> cont 47;
	_ -> happyError' (tk:tks)
	}

happyError_ 48 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => TE a -> (a -> TE b) -> TE b
happyThen = (>>=)
happyReturn :: () => a -> TE a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> TE a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> TE a
happyError' = parseError

termParsing tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

contextParsing tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError [] = failE "Parse error!"
parseError (tok : ts) = failE $ printf "Error: %s (%s)" (show tok) (show ts)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
