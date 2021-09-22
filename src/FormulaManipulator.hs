{-|
Module      : FormulaManipulator
Description : Manipulate formulas and expressions represented by `Expr` values
Copyright   : STUDENT NAME 1 (ID)
              STUDENT NAME 2 (ID)

`FormulaManipulator` offers functions to manipulate, evaluate, and print
formulas and expressions represented by `Expr` values.
-}
module FormulaManipulator
  ( foldE
  , printE
  , evalE
  , simplifyE
  , diffE
  )
where

import           ExprLanguage                   ( Expr(Var, Const, Plus, Mult) )

{-Has extra inputs: 
  -baseConstant for the function to apply when reaching a constant, 
  -baseVar for the function to apply when reaching a constant,
  -stepPlus for the function to apply when reaching a plus
  -stepMul for the function to apply when reaching a multiply
  -}

foldE :: (Const -> a) -> (Var -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr b c -> a
foldE baseConst baseVar stepPlus stepMult = rec
            where
             rec  (Const i) = baseConst i
             rec  (Var i) = baseVar i
             rec  (Plus eq1 eq2) = stepPlus (rec eq1) (rec eq2) 
             rec  (Mult eq1 eq2) = stepMult (rec eq1) (rec eq2)

printE    = error "Implement, document, and test this function"
evalE     = error "Implement, document, and test this function"
simplifyE = error "Implement, document, and test this function"
diffE     = error "Implement, document, and test this function"
