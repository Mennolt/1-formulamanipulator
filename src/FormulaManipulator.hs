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

import ExprLanguage ( Expr(..) )

{-Has extra inputs: 
  -baseConst for the function to apply when reaching a constant, 
  -baseVar for the function to apply when reaching a constant,
  -stepPlus for the function to apply when reaching a plus
  -stepMul for the function to apply when reaching a multiply
  -}

foldE :: (c -> a) -> (b -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr b c -> a
foldE baseConst baseVar stepPlus stepMult = rec
            where
             rec  (Var i) = baseVar i
             rec  (Const i) = baseConst i
             rec  (Plus eq1 eq2) = stepPlus (rec eq1) (rec eq2)
             rec  (Mult eq1 eq2) = stepMult (rec eq1) (rec eq2)

{-|
  The `printE` function takes an expression as input and returns the pretty-printed expression.
  It takes one argument of type Expr. 
  It returns a String.
-}
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
      printMult a b = "(" ++ a ++ " * " ++ b ++ ")"

--evalE :: Expr b a -> a
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
