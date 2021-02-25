module C
  ( module C
  , module Expr
  ) where

import Data.Text (Text)
import Expr hiding (Expression, Term)
import qualified Expr
import qualified IR

type Body = [Statement]

type Expression = Expr.Expression Identifier
type Term = Expr.Term Identifier

data TopLevel
  = Fdef Type Identifier Parameters Body
  deriving (Show)

type File = [TopLevel]

data Statement
  = Return Expression
  | Declaration Identifier (Maybe Expression)
  | Expression Expression
  deriving (Show)

irStatement :: Statement -> IR.Builder [IR.Statement]
irStatement (Return e) = pure . IR.Return <$> traverse IR.lookup e
irStatement (Expression e) = pure . IR.Expression <$> traverse IR.lookup e
irStatement (Declaration name Nothing) = IR.declare name >> pure []
irStatement (Declaration name (Just value)) = do
  IR.declare name
  pure . IR.Expression <$> traverse IR.lookup (Expr.Assignment name value)

irBlock :: [C.Statement] -> IR.Builder IR.Block
irBlock stmts = IR.Block . concat <$> traverse irStatement stmts

irFile :: C.File -> Either Text IR.TopLevel
irFile [Fdef returnType name params body] = IR.runBuilder $ IR.Fdef returnType name params <$> irBlock body
irFile _ = error "multiple toplevels not supported"