module C
  ( module C
  , module Expr
  ) where

import Prelude hiding (init)

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Expr hiding (Expression)
import qualified Expr
import qualified IR

type Expression = Expr.Expression Identifier

data TopLevel
  = FunctionDefinition Type Identifier [Parameter] Statement
  | FunctionDeclaration Type Identifier [Parameter]
  deriving (Show)

type File = [TopLevel]

data Statement
  = Block [Statement]
  | Break
  | Continue
  | Declaration Identifier (Maybe Expression)
  | DoWhile Statement Expression
  | Expression Expression
  | For Statement Expression Statement Statement
  | If Expression Statement (Maybe Statement)
  | Inert
  | Return Expression
  | While Expression Statement
  deriving (Show)

irStatement :: Statement -> IR.Builder [IR.Statement]
irStatement (Expression e) = pure . IR.Expression <$> traverse IR.lookup e
irStatement (Block stmts) = IR.block $ concat <$> traverse irStatement stmts

irStatement Break = pure [IR.Break]
irStatement Continue = pure [IR.Continue]
irStatement Inert = pure []
irStatement (Return e) = pure . IR.Return <$> traverse IR.lookup e

irStatement (Declaration name Nothing) = IR.declareLocal name >> pure []
irStatement (Declaration name (Just value)) = do
  IR.declareLocal name
  pure . IR.Expression <$> traverse IR.lookup (Expr.Assignment name value)

irStatement (If condition true false) = fmap pure $ IR.If
  <$> traverse IR.lookup condition
  <*> irStatement true
  <*> maybe (pure []) irStatement false

irStatement (DoWhile body condition) = do
  body' <- irStatement body
  condition' <- traverse IR.lookup condition
  pure [IR.Loop IR.OneOrMore condition' body' []]

irStatement (For init cond step body) = IR.block $ do
  init' <- irStatement init
  cond' <- traverse IR.lookup cond
  step' <- irStatement step
  body' <- irStatement body
  pure $ init' <> [IR.Loop IR.ZeroOrMore cond' body' step']

irStatement (While condition body) = do
  condition' <- traverse IR.lookup condition
  body' <- irStatement body
  pure [IR.Loop IR.ZeroOrMore condition' body' []]

irFile :: C.File -> Either Text [IR.TopLevel]
irFile toplevels = IR.evalBuilder $ catMaybes <$> traverse irFdef toplevels
  where
    irFdef (FunctionDeclaration returnType name params)
      = IR.declareFunction returnType name params >> pure Nothing
    irFdef (FunctionDefinition returnType name params body)
      = Just <$> IR.defineFunction returnType name params (irStatement body)
