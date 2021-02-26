module C
  ( module C
  , module Expr
  ) where

import Data.Text (Text)
import Expr hiding (Expression, Term)
import qualified Expr
import qualified IR

type Expression = Expr.Expression Identifier
type Term = Expr.Term Identifier

data TopLevel
  = Fdef Type Identifier Parameters Statement
  deriving (Show)

type File = [TopLevel]

data Statement
  = Block [Statement]
  | Declaration Identifier (Maybe Expression)
  | Expression Expression
  | If Expression Statement (Maybe Statement)
  | Inert
  | Return Expression
  deriving (Show)

irStatement :: Statement -> IR.Builder [IR.Statement]
irStatement (Block stmts) = IR.block $ concat <$> traverse irStatement stmts
irStatement (Declaration name Nothing) = IR.declare name >> pure []
irStatement (Declaration name (Just value)) = do
  IR.declare name
  pure . IR.Expression <$> traverse IR.lookup (Expr.Assignment name value)

irStatement (Expression e) = pure . IR.Expression <$> traverse IR.lookup e

irStatement (If condition true false) = fmap pure $ IR.If
  <$> traverse IR.lookup condition
  <*> irStatement true
  <*> maybe (pure []) irStatement false

irStatement Inert = pure []
irStatement (Return e) = pure . IR.Return <$> traverse IR.lookup e

irFile :: C.File -> Either Text IR.TopLevel
irFile [Fdef returnType name params body] = IR.buildFdef returnType name params $ irStatement body
irFile _ = error "multiple toplevels not supported"
