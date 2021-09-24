{-|
Module      : FormulaManipulator
Description : Manipulate formulas and expressions represented by `Expr` values
Copyright   : Mennolt van Alten (1289667)
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

import ExprLanguage (Expr(Var, Const, Plus, Mult), parseExpr)



-- | The foldE function is a catamorphism for the type Expr.
-- 
-- It has 4 inputs: 
-- 
{-| 

  * baseConstant is the function to apply when reaching a constant. 
    It must convert a numeric type to the chosen output type.

  * baseVar is the function to apply when reaching a variable. 
    It must convert a String to the chosen output type.

  * stepPlus is the function to apply when reaching a plus. 
    It will be applied on the outputs of a recursion on the two equations and must convert two values of the chosen output type into one value of that type.

  * stepMul is the function to apply when reaching a multiply
    It will be applied on the outputs of a recursion on the two equations and must convert two values of the chosen output type into one value of that type.

  -}
foldE :: (Num c) => (c -> a) -> (b -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr b c -> a
foldE baseConst baseVar stepPlus stepMult = rec
            where
             rec  (Var i) = baseVar i
             rec  (Const i) = baseConst i
             rec  (Plus eq1 eq2) = stepPlus (rec eq1) (rec eq2)
             rec  (Mult eq1 eq2) = stepMult (rec eq1) (rec eq2)

-- | The `printE` function takes an expression as input and returns the pretty-printed expression.
--
-- It takes one argument of type Expr. 
--
-- It returns a String.

printE :: Expr String Int -- ^ The Expr argument
       -> String -- ^ The return String
printE = foldE printConst id printPlus printMult
    where
      printConst :: Int -> String
      printConst n  | head (show n) == '-' = "(" ++ show n ++ ")" 
                    | otherwise = show n
      printPlus :: String -> String -> String
      printPlus a b = "(" ++ a ++ " + " ++ b ++ ")"
      printMult :: String -> String -> String
      printMult a b = a ++ " * " ++ b

--evalE :: Expr b a -> a
-- |evalE is a function that evaluates a function given a list of variables to look up and an Expr
-- It is implemented using foldE and the 4 functions below.
-- 
-- Note: Current implementation does not support variable lookup.
evalE     = foldE evalConst evalVar evalPlus evalMult --error "Implement, document, and test this function"

evalConst :: a -> a
evalConst = id

evalVar lookup = lookup--to be improved

evalPlus :: (Num a) => a -> a -> a
evalPlus = (+) 

evalMult :: (Num a) => a -> a -> a
evalMult = (*)

simplifyE = error "Implement, document, and test this function"
diffE     = error "Implement, document, and test this function"
