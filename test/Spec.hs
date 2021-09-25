import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           ExprLanguage                   ( Expr(Var, Const, Plus, Mult), parseExpr, ParseError )
import           FormulaManipulator             ( foldE
                                                , printE
                                                , evalE
                                                , simplifyE
                                                , diffE
                                                , evalConst, evalVar, evalPlus, evalMult
                                                )
import           FormulatorCLI                  ( processCLIArgs )

main :: IO ()
main = hspec $ do
  describe "FormulaManipulator" $ do

    describe "foldE" $ do
      it "should evaluate the parsed expression \"1+1\" when provided with all the functions to make evalE, returning 2" $ do
        foldE evalConst evalVar evalPlus evalMult (Plus (Const 1)  (Const 1)) `shouldBe` 2 --parseExpr "1+1"

    describe "printE" $ do
      it "should convert (Var \"x\") to \"x\"" $ do
        printE (Var "x") `shouldBe` ("x" :: String)
      it "should convert (Const 1) to \"1\"" $ do
        printE (Const 1) `shouldBe` ("1" :: String)
      it "should convert (Const (-1)) to \"(-1)\"" $ do
        printE (Const (-1)) `shouldBe` ("(-1)" :: String)
      it "should convert Mult (Plus (Var \"x\") (Const (-1))) (Plus (Var \"x\") (Const 1)) to \"((x + (-1)) * (x + 1))\"" $ do
        printE (Mult (Plus (Var "x") (Const (-1))) (Plus (Var "x") (Const 1))) `shouldBe` ("((x + (-1)) * (x + 1))" :: String)
      
    describe "parseExpr . printE" $ do
      it "should convert (Var \"x\") to (Var \"x\")" $ do
        parseExpr (printE (Var "x")) `shouldBe` (Right (Var "x") :: Either ParseError (Expr String Integer))
      it "should convert (Const 1) to (Const 1)" $ do
        parseExpr (printE (Const 1)) `shouldBe` (Right (Const 1) :: Either ParseError (Expr String Integer))
      it "should convert (Const (-1)) to (Const (-1))" $ do
        parseExpr (printE (Const (-1))) `shouldBe` (Right (Const (-1)) :: Either ParseError (Expr String Integer))
      it "should convert (Mult (Plus (Var \"x\") (Const (-1))) (Plus (Var \"x\") (Const 1))) to (Mult (Plus (Var \"x\") (Const (-1))) (Plus (Var \"x\") (Const 1)))" $ do
        parseExpr (printE (Mult (Plus (Var "x") (Const (-1))) (Plus (Var "x") (Const 1)))) `shouldBe` (Right(Mult (Plus (Var "x") (Const (-1))) (Plus (Var "x") (Const 1))) :: Either ParseError (Expr String Integer))

    describe "evalE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "simplifyE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "diffE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "FormulatorCLI" $ do
    describe "processCLIArgs" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

