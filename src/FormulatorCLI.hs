{-|
Module      : FormulatorCLI
Description : The command-line interface for the formulator program
Copyright   : STUDENT NAME 1 (ID)
              STUDENT NAME 2 (ID)

The command-line interface for the formulator program. Parses and processes
command-line arguments. Manipulates expressions according to the specification
in terms of command-line arguments.
-}
module FormulatorCLI
  ( processCLIArgs
  )
where
import           Data.Either
import           Data.List.Split
import           ExprLanguage                   ( Expr
                                                , parseExpr
                                                , ParseError
                                                )
import           FormulaManipulator             ( printE
                                                , evalE
                                                , simplifyE
                                                , diffE
                                                )


processCLIArgs :: [String] -> String
processCLIArgs as | as!!0 == "-p" || as!!0 == "--print" = display (as!!1)
                  | as!!0 == "-s" || as!!0 == "--simplify" = simplify (as!!1)
                  | as!!0 == "-d" || as!!0 == "--differentiate" = diff (as!!1) (as!!2)
                  | as!!0 == "-e" || as!!0 == "--evaluate" = eval (as!!1) (as!!2)
                  | as!!0 == "-h" || as!!0 == "--help" = help
                  | otherwise = error "Unexpected option, use -h to display help"
                    where
                      help = "Usage: \
                              \formulator -- OPTION EXPR\n\
                              \-p, --print: pretty-print the expression\n\
                              \-s, --simplify: simplify and pretty-print the expression\n\
                              \-d,--differentiate <VAR>: differentiate expression for <VAR> and simplify and pretty-print the result \n\
                              \-e, --evaluate <LOOKUP>: evaluate the expression given the <LOOKUP> table.\n\
                              \   The lookup table is a String containing a list of <VAR>=<VALUE> pairs separated by semicolons.\n\
                              \   For example, \"x=4;y=5\" should give x the value 4 and y the value 5.\n\
                              \h, --help: Show this help message."

display :: String -> String
display s = case (parseExpr s) of
                  Left  err   -> show err
                  Right expr  -> printE expr

simplify :: String -> String
simplify s = case (parseExpr s) of
                  Left  err   -> show err
                  Right expr  -> printE (simplifyE expr)

diff :: String -> String -> String
diff var s = case (parseExpr s) of
                  Left  err   -> show err
                  Right expr  -> printE (simplifyE (diffE var expr))

-- splits a string of statements delimited into
splitArgs :: String -> [String]
splitArgs = splitOn ";"

-- creates a lookup function from a list of statements in shape "x=5"
getLookup :: [String] -> String -> Integer
getLookup [] a = error "unknown variable"
getLookup (var : vars) a = if a == v then c else getLookup vars a
                        where
                          v = head (splitOn "=" var)
                          c = read ((splitOn "=" var)!!1) :: Integer

eval :: String -> String -> String
eval args s = case (parseExpr s) of
                  Left  err   -> show err
                  Right expr  -> show (evalE (getLookup (splitArgs args)) (expr))