import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           ExprLanguage                   ( Expr(Var, Const, Plus, Mult), parseExpr, ParseError )
import           FormulaManipulator             ( foldE
                                                , printE
                                                , evalE
                                                , simplifyE
                                                , diffE
                                                , evalConst, evalPlus, evalMult
                                                )
import           FormulatorCLI                  ( processCLIArgs )

main :: IO ()
main = hspec $ do
  describe "FormulaManipulator" $ do

    describe "foldE" $ do
      it "should evaluate the parsed expression \"1+1\" when provided with all the functions to make evalE, returning 2" $ do
        foldE evalConst id evalPlus evalMult (Plus (Const 1)  (Const 1)) `shouldBe` 2

    describe "printE" $ do  
      it "should convert (Var \"x\") to \"x\"" $ do
        printE (Var "x") `shouldBe` ("x" :: String)

      it "should convert (Const 1) to \"1\"" $ do
        printE (Const 1) `shouldBe` ("1" :: String)

      it "should convert (Const (-1)) to \"(-1)\"" $ do
        printE (Const (-1)) `shouldBe` ("(-1)" :: String)

      it "should convert (Plus (Const (-21)) (Plus (Const 7) (Mult (Const 279) (Var \"y\")))) to \"((-21) + (7 + 279 * y))\"" $ do
        printE (Plus (Const (-21)) (Plus (Const 7) (Mult (Const 279) (Var "y")))) 
        `shouldBe` ("((-21) + (7 + (279 * y)))" :: String)

      it "should convert Mult (Plus (Var \"x\") (Const (-1))) (Plus (Var \"x\") (Const 1)) to \"((x + (-1)) * (x + 1))\"" $ do
        printE (Mult (Plus (Var "x") (Const (-1))) (Plus (Var "x") (Const 1))) 
        `shouldBe` ("((x + (-1)) * (x + 1))" :: String)
      

    describe "parseExpr . printE" $ do
      it "should convert (Var \"x\") to (Var \"x\")" $ do
        parseExpr (printE (Var "x")) 
        `shouldBe` (Right (Var "x") :: Either ParseError (Expr String Integer))
     
      it "should convert (Const 1) to (Const 1)" $ do
        parseExpr (printE (Const 1)) 
        `shouldBe` (Right (Const 1) :: Either ParseError (Expr String Integer))

      it "should convert (Const (-1)) to (Const (-1))" $ do
        parseExpr (printE (Const (-1))) 
        `shouldBe` (Right (Const (-1)) :: Either ParseError (Expr String Integer))

      it "should convert (Plus (Const (-21)) (Plus (Const 7) (Mult (Const 279) (Var \"y\")))) to (Plus (Const (-21)) (Plus (Const 7) (Mult (Const 279) (Var \"y\")))))" $ do
        parseExpr (printE (Plus (Const (-21)) (Plus (Const 7) (Mult (Const 279) (Var "y"))))) 
        `shouldBe` (Right (Plus (Const (-21)) (Plus (Const 7) (Mult (Const 279) (Var "y")))) :: Either ParseError (Expr String Integer))
      
      it "should convert (Mult (Plus (Var \"x\") (Const (-1))) (Plus (Var \"x\") (Const 1))) to (Mult (Plus (Var \"x\") (Const (-1))) (Plus (Var \"x\") (Const 1)))" $ do
        parseExpr (printE (Mult (Plus (Var "x") (Const (-1))) (Plus (Var "x") (Const 1)))) 
        `shouldBe` (Right(Mult (Plus (Var "x") (Const (-1))) (Plus (Var "x") (Const 1))) :: Either ParseError (Expr String Integer))

    describe "evalE" $ do
      it "should convert (Plus (Const 1) (Const 1)) to 2" $ do
        evalE id (Plus (Const 1) (Const 1)) `shouldBe` (2 :: Integer)

      it "should convert (Mult (Const 3) (Const 4)) to 12" $ do
        evalE id (Mult (Const 3) (Const 4)) `shouldBe` (12 :: Integer)
      
      it "should convert (Mult (Var \"x\") (Const 3) where x = 4 to 12" $ do
        evalE (\v -> if v == "x" then 4 else error "unknown variable") (Mult (Var "x") (Const 3)) `shouldBe` (12 :: Integer)

    describe "simplifyE" $ do
      it "should simplify Plus (Const 1) (Const 1) to Const 2" $ do
        simplifyE (Plus (Const 1) (Const 1)) `shouldBe` (Const 2:: Expr String Integer)
      it "should simplify Plus (Var \"x\") (Const 0) to Var \"x\"" $ do
        simplifyE (Plus (Var "x") (Const 0)) `shouldBe` (Var "x":: Expr String Integer)
      it "should simplify Mult (Const 2) (Const 2) to Const 4" $ do
        simplifyE (Plus (Const 2) (Const 2)) `shouldBe` (Const 4:: Expr String Integer)
      it "should simplify Mult (Var \"x\") (Const 0) to Const 0" $ do
        simplifyE (Mult (Var "x") (Const 0)) `shouldBe` (Const 0:: Expr String Integer)
      it "should simplify Mult (Var \"x\") (Const 1) to Var \"x\"" $ do
        simplifyE (Mult (Var "x") (Const 1)) `shouldBe` (Var "x":: Expr String Integer)


    describe "diffE" $ do
      it "Constant derivation: should differentiate Const 1 to Const 0" $ do
        diffE "x" (Const 1) `shouldBe` (Const 1, Const 0)
      
      it "Variable derivation: should differentiate Var \"x\" to Const 1" $ do
        diffE "x" (Var "x") `shouldBe` (Var "x", Const 1)

      it "Variable derivation: differentiation over \"x\" should differentiate Var \"y\" to Var \"y\"" $ do
        diffE "x" (Var "y") `shouldBe` (Var "y", Var "y")

      it "Sum derivation test: should differentiate (Plus (Const 1) (Var \"x\")) to  Const 1" $ do
        diffE "x" (Plus (Const 1) (Var "x")) `shouldBe` (Plus (Const 1) (Var "x"),Plus (Const 0) (Const 1))

      it "Product derivation test: should differentiate (Mult (Const 1) (Var \"x\")) to  Const 1" $ do
        diffE "x" (Mult (Const 1) (Var "x")) `shouldBe` (Mult (Const 1) (Var "x"),Plus (Mult (Const 1) (Const 1)) (Mult (Const 0) (Var "x")))

  describe "FormulatorCLI" $ do
    describe "processCLIArgs" $ do
      it "should print [\"-p\",\"1+1\"] as 1+1" $ do
        processCLIArgs ["-p","1+1"] `shouldBe` "(1 + 1)"
      it "should print [\"--print\",\"1+1\"] as 1+1" $ do
        processCLIArgs ["--print","1+1"] `shouldBe` "(1 + 1)"
      it "should print [\"--print\",\"hello\"] as hello" $ do
        processCLIArgs ["--print","hello"] `shouldBe` "hello"
      it "should print [\"-p\",\"hello\"] as hello" $ do
        processCLIArgs ["-p","hello"] `shouldBe` "hello"
      it "should print [\"-p\",\"-1\"] as (-1)" $ do
        processCLIArgs ["-p","-1"] `shouldBe` "(-1)"
      it "should print [\"--print\",\"-1\"] as (-1)" $ do
        processCLIArgs ["--print","-1"] `shouldBe` "(-1)"

      it "should print [\"--print\",\"((x + (-1)) * (x + 1))\"] as ((x + (-1)) * (x + 1))" $ do
        processCLIArgs ["--print","((x + (-1)) * (x + 1))"] `shouldBe` "((x + (-1)) * (x + 1))"
      it "should print [\"-p\",\"((x + (-1)) * (x + 1))\"] as ((x + (-1)) * (x + 1))" $ do
        processCLIArgs ["-p","((x + (-1)) * (x + 1))"] `shouldBe` "((x + (-1)) * (x + 1))"



      it "should simplify [\"-s\", \"1+0\"] to 1" $ do
        processCLIArgs ["-s", "1+0"] `shouldBe` "1"
      it "should simplify [\"--simplify\", \"1+0\"] to 1" $ do
        processCLIArgs ["--simplify", "1+0"] `shouldBe` "1"
      it "should simplify [\"-s\", \"5+6\"] to 11" $ do
        processCLIArgs ["-s", "5+6"] `shouldBe` "11"
      it "should simplify [\"--simplify\", \"5+6\"] to 11" $ do
        processCLIArgs ["--simplify", "5+6"] `shouldBe` "11"
      it "should simplify [\"-s\", \"5*6\"] to 30" $ do
        processCLIArgs ["-s", "5*6"] `shouldBe` "30"
      it "should simplify [\"--simplify\", \"5*6\"] to 30" $ do
        processCLIArgs ["--simplify", "5*6"] `shouldBe` "30"
        
      it "should simplify [\"-s\", \"0*x\"] to 0" $ do
        processCLIArgs ["-s", "0*x"] `shouldBe` "0"
      it "should simplify [\"--simplify\", \"0*x\"] to 0" $ do
        processCLIArgs ["--simplify", "0*x"] `shouldBe` "0"

      it "should simplify [\"-s\", \"1*x\"] to x" $ do
        processCLIArgs ["-s", "1*x"] `shouldBe` "x"
      it "should simplify [\"--simplify\", \"1*x\"] to x" $ do
        processCLIArgs ["--simplify", "1*x"] `shouldBe` "x"

      it "should print a help message when -h is called" $ do
        processCLIArgs ["-h"] `shouldBe` "- Usage: \
                              \formulator -- OPTION EXPR \

                              \-p, --print: pretty-print the expression \
                              \-s, --simplify: simplify and pretty-print the expression \
                              \-d,--differentiate <VAR>: differentiate expression for <VAR> and simplify and pretty-print the result \
                              \-e, --evaluate <LOOKUP>: evaluate the expression given the <LOOKUP> table. \
                              \   The lookup table is a String containing a list of <VAR>=<VALUE> pairs separated by semicolons. \
                              \   For example, \"x=4;y=5\" should give x the value 4 and y the value 5.\
                              \h, --help: Show this help message."
      it "should print a help message when --help is called" $ do
        processCLIArgs ["--help"] `shouldBe` "- Usage: \
                              \formulator -- OPTION EXPR \

                              \-p, --print: pretty-print the expression \
                              \-s, --simplify: simplify and pretty-print the expression \
                              \-d,--differentiate <VAR>: differentiate expression for <VAR> and simplify and pretty-print the result \
                              \-e, --evaluate <LOOKUP>: evaluate the expression given the <LOOKUP> table. \
                              \   The lookup table is a String containing a list of <VAR>=<VALUE> pairs separated by semicolons. \
                              \   For example, \"x=4;y=5\" should give x the value 4 and y the value 5.\
                              \h, --help: Show this help message."

      



      

      

