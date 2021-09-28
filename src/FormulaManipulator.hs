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
-- It is implemented using foldE and the 3 functions below.

evalE :: (a -> Int) -> (Expr a Int) -> Int
evalE lookup = foldE evalConst lookup evalPlus evalMult

evalConst :: Int -> Int
evalConst = id

evalPlus :: Int -> Int -> Int
evalPlus = (+) 

evalMult :: Int -> Int -> Int
evalMult = (*)

simplifyE = error "Implement, document, and test this function"

diffE :: String -> Expr String Integer -> (Expr String Integer, Expr String Integer)
-- dv :: String
diffE dv   = foldE diffConst diffVar diffPlus diffMult dv
-- dv: variable to derive upon

diffConst :: String -> Expr String Integer -> (Expr String Integer, Expr String Integer)
diffConst dv i = (i, (Const 0))

diffVar :: String -> String -> (Expr String Integer, Expr String Integer) 
diffVar dv var = if dv == var then ((Var var), (Const 1)) else ((Var var), (Var var))

diffPlus :: String -> (Expr String Integer, Expr String Integer) -> 
  (Expr String Integer, Expr String Integer) ->
  (Expr String Integer, Expr String Integer)
diffPlus dv eq1 eq2 =((Plus (fst eq1) (fst eq2)), (Plus (snd eq1) (snd eq2)))

diffMult :: String -> (Expr String Integer, Expr String Integer) -> 
  (Expr String Integer, Expr String Integer) ->
  (Expr String Integer, Expr String Integer)
diffMult dv eq1 eq2 = ((Mult (fst eq1) (fst eq2)), ((Mult (fst eq1) (snd eq2)) + (Mult (snd eq1) (fst eq2)))) 