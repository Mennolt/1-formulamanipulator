{-|
Module      : FormulaManipulator
Description : Manipulate formulas and expressions represented by `Expr` values
Copyright   : Mennolt van Alten (1289667)
              Ignacio Jordano de Castro (1769626)

`FormulaManipulator` offers functions to manipulate, evaluate, and print
formulas and expressions represented by `Expr` values.
-}

module FormulaManipulator
  ( foldE
  , printE
  , evalE
  , simplifyE
  , diffE
  , evalConst, evalPlus, evalMult
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
             rec  (Const i) = baseConst i
             rec  (Var i) = baseVar i
             rec  (Plus eq1 eq2) = stepPlus (rec eq1) (rec eq2)
             rec  (Mult eq1 eq2) = stepMult (rec eq1) (rec eq2)

-- | The `printE` function takes an expression as input and returns the pretty-printed expression.
--
-- It takes one argument of type Expr. 
--
-- It returns a String.

printE :: Expr String Int -- ^ The input Expression
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
evalE :: (a -> Int) -> Expr a Int -> Int
evalE lookup = foldE evalConst lookup evalPlus evalMult

evalConst :: Int -> Int
evalConst = id

evalPlus :: Int -> Int -> Int
evalPlus = (+) 

evalMult :: Int -> Int -> Int
evalMult = (*)


{-| The `simplifyE` function takes an expression as input and returns the simplified expression.

  It takes one argument of type Expr. 

  It returns an Expression.

  Expressions are simplified according to the following scheme:
    
    * (∀ x: Num x: 0 + x = x)
    * (∀ x: Num x: 0 * x = 0)
    * (∀ x: Num x: 1 * x = x)
  
  Constant expressions are evaluated:
  
    * 3 * 15 is simplified to 45
    * 7 + 12 is simplified to 19
-}
simplifyE :: Expr String Int -> Expr String Int
simplifyE = foldE Const Var simplPlus simplMult

simplPlus :: Expr String Int ->  Expr String Int ->  Expr String Int
simplPlus (Var a) (Const 0) = Var a
simplPlus (Const 0) (Var b) = Var b
simplPlus (Const a) (Const b) = Const (a+b)
simplPlus a b = Plus a b

simplMult :: Expr String Int ->  Expr String Int ->  Expr String Int
simplMult (Var a) (Const 1) = Var a
simplMult (Const 1) (Var b) = Var b
simplMult _ (Const 0) = Const 0
simplMult (Const 0) _ = Const 0
simplMult (Const a) (Const b) = Const (a*b)
simplMult a b = Mult a b
diffE     = error "Implement, document, and test this function"
