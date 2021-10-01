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
      
      it "should have convert (\v -> if v == \"x\" then 4 else error \"unknown variable\") (Mult (Var \"x\") (Const 4)) to 12" $ do
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
        diffE "x" (Const 1) `shouldBe` (Const 0)
      
      it "Variable derivation: should differentiate Var \"x\" to Const 1" $ do
        diffE "x" (Var "x") `shouldBe` (Const 1)

      it "Variable derivation: differentiation over \"x\" should differentiate Var \"y\" to Var \"y\"" $ do
        diffE "x" (Var "y") `shouldBe` (Var "y")

      it "Sum derivation test: should differentiate (Plus (Const 1) (Var \"x\")) to  Const 1" $ do
        diffE "x" (Plus (Const 1) (Var "x")) `shouldBe` (Plus (Const 0) (Const 1))

      it "Product derivation test: should differentiate (Mult (Const 1) (Var \"x\")) to  Const 1" $ do
        diffE "x" (Mult (Const 1) (Var "x")) `shouldBe` (Plus (Mult (Const 1) (Const 1)) (Mult (Const 0) (Var "x")))

  describe "FormulatorCLI" $ do
    describe "processCLIArgs" $ do
      --Differentiate
      it "Constant derivation: should differentiate 1 to  0" $ do
        processCLIArgs ["-d", "x", "1"] `shouldBe` ("0")
            
      it "Variable derivation: should differentiate x to 1" $ do
        processCLIArgs ["--differentiate", "x", "x"] `shouldBe` ("1")

      it "Variable derivation: differentiation over x should differentiate y to y" $ do
        processCLIArgs ["-d", "x", "y"] `shouldBe` ("y")

      it "Sum derivation test: should differentiate 1 + x to  1" $ do
        processCLIArgs ["--differentiate", "x", "1 + x"] `shouldBe` ("1")

      it "Product derivation test: should differentiate 1 * x to 1" $ do
        processCLIArgs ["-d", "x", "1 * x"] `shouldBe` ("1")

      --Evaluate
      it "should convert 1 + 1 to 2" $ do
        processCLIArgs ["-e","", "1 + 1"] `shouldBe` ("2")
            
      it "should convert x=1, 3 * 4 to 12" $ do
        processCLIArgs ["--evaluate", "x=1", "3 * 4"] `shouldBe` ("12")

      it "should convert x=4, x * 3 to 12" $ do
        processCLIArgs ["-e", "x=4", "x * 3"] `shouldBe` ("12")

      it "should convert x=4;y=2, y + x * 3 to 14" $ do
        processCLIArgs ["--evaluate", "x=4;y=2", "y + x * 3"] `shouldBe` ("14")

      --Error
      it "should throw Unexpected option error" $ do
         evaluate(processCLIArgs ["-f", "", "1 + 1"]) `shouldThrow` errorCall "Unexpected option, use -h to display help"

