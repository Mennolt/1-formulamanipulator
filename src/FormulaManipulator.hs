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

printE :: Expr String Integer -- ^ The input Expression
       -> String -- ^ The return String
printE = foldE printConst id printPlus printMult
    where
      printConst :: Integer -> String
      printConst n  | head (show n) == '-' = "(" ++ show n ++ ")" 
                    | otherwise = show n
      printPlus :: String -> String -> String
      printPlus a b = "(" ++ a ++ " + " ++ b ++ ")"
      printMult :: String -> String -> String
      printMult a b = "(" ++ a ++ " * " ++ b ++ ")"


<<<<<<< HEAD
evalE :: (a -> Integer) -> (Expr a Integer) -> Integer
=======
{- |evalE is a function that evaluates a function given a list of variables to look up and an Expr

  It takes two arguments: one converting the type of a variable into an integer, and one of type Expr that will be evaluated.

  It returns an integer.
-}
evalE :: (a -> Int) -> (Expr a Int) -> Int
>>>>>>> 133959fecc50207b40ec208001e0269885eeec95
evalE lookup = foldE evalConst lookup evalPlus evalMult

evalConst :: Integer -> Integer
evalConst = id

evalPlus :: Integer -> Integer -> Integer
evalPlus = (+) 

evalMult :: Integer -> Integer -> Integer
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
simplifyE :: Expr String Integer -> Expr String Integer
simplifyE = foldE Const Var simplPlus simplMult

simplPlus :: Expr String Integer ->  Expr String Integer ->  Expr String Integer
simplPlus (Var a) (Const 0) = Var a
simplPlus (Const 0) (Var b) = Var b
simplPlus (Const a) (Const b) = Const (a+b)
simplPlus a b = Plus a b

simplMult :: Expr String Integer ->  Expr String Integer ->  Expr String Integer
simplMult (Var a) (Const 1) = Var a
simplMult (Const 1) (Var b) = Var b
simplMult _ (Const 0) = Const 0
simplMult (Const 0) _ = Const 0
simplMult (Const a) (Const b) = Const (a*b)
simplMult a b = Mult a b


{-|
The diffE function is used to differentiate an expression. 

It takes a string and an expression as input and outputs a tuple of expressions.

The string determines what variable to differentiate the expression over.

The output contains first the original expression, then second its derivative.

It is suggested to simplify after using this function, because especially products can become a long equation.
-}
diffE :: String -> Expr String Integer -> (Expr String Integer, Expr String Integer)
-- dv :: String
diffE dv = foldE (diffConst dv) (diffVar dv ) (diffPlus dv) (diffMult dv)
-- dv: variable to derive upon

diffConst :: String -> Integer -> (Expr String Integer, Expr String Integer)
diffConst dv i = (Const i, (Const 0))

diffVar :: String -> String -> (Expr String Integer, Expr String Integer) 
diffVar dv var = if dv == var then ((Var var), (Const 1)) else ((Var var), (Var var))

diffPlus :: String -> (Expr String Integer, Expr String Integer) -> 
  (Expr String Integer, Expr String Integer) ->
  (Expr String Integer, Expr String Integer)
diffPlus dv eq1 eq2 =((Plus (fst eq1) (fst eq2)), (Plus (snd eq1) (snd eq2)))

diffMult :: String -> (Expr String Integer, Expr String Integer) -> 
  (Expr String Integer, Expr String Integer) ->
  (Expr String Integer, Expr String Integer)
diffMult dv eq1 eq2 = ((Mult (fst eq1) (fst eq2)), (Plus (Mult (fst eq1) (snd eq2)) (Mult (snd eq1) (fst eq2))))
