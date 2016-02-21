module PCF
       ( PcfType 
       , typeInferTest
       ) where 

import PcfTyping 

typeInferTest :: TypeContext -> PcfTerm -> String 
typeInferTest ctx exp = case typeInfer ctx exp 
                        of Nothing -> "Typing error!"
                           Just t -> show t
     
testContext = 
  [ ("x", T_nat)
  , ("y", T_bool)
  , ("z", T_sum T_nat T_bool)
  , ("h", T_arrow T_nat T_nat)
  , ("g", T_arrow T_nat T_bool)
  , ("f", T_arrow (T_arrow T_nat T_bool) (T_arrow T_nat T_bool))
  ]

testTerms = 
  [ (TM_op (TM_num 1) (TM_num 2))
  , (TM_op (TM_var "x") (TM_var "w"))
  , (TM_if TM_true (TM_var "y") TM_false)
  , (TM_pair (TM_var "x") (TM_var "y"))
  , (TM_projfst (TM_var "z"))
  , (TM_injsnd (TM_app (TM_var "f") (TM_var "g")))
  , (TM_case (TM_var "z") ("x1",TM_eqtest (TM_var "x") (TM_var "x1")) ("x2",TM_var "y"))
  , (TM_abs "x" (TM_injfst (TM_var "x")))
  , (TM_app (TM_app (TM_var "f") (TM_abs "x" (TM_var "y"))) (TM_var "x"))
  , (TM_fix (TM_var "f"))
  ]

pcftest = 
  let printTyping ctx exp = putStrLn $ show exp ++ " : " ++ typeInferTest ctx exp 
  in do mapM (printTyping testContext) testTerms