module C
  ( module C
  , module Expr
  ) where

import Prelude hiding (init)

import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Text (Text)

import Expr hiding (Expression)
import qualified Expr
import qualified IR
import Util

type Expression = Expr.Expression Identifier

data TopLevel
  = FunctionDeclaration Type Identifier [Parameter Type Identifier]
  | FunctionDefinition Type Identifier [Parameter Type Identifier] [Statement]
  | GlobalDeclaration Type Identifier
  | GlobalDefinition Type Identifier Expression
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

irType :: Identifier -> IR.Builder IR.Type
irType "int" = pure IR.Int
irType t = throwError $ "unknown type: " <> tshow t

irParameter :: Parameter Identifier Identifier -> IR.Builder (Parameter IR.Type Identifier)
irParameter param = do
  typ <- irType $ paramType param
  pure $ Parameter { paramName = paramName param, paramType = typ }

irValue :: Expression -> IR.Builder IR.Value
irValue (Literal (Integer i)) = pure $ IR.Int64 $ fromInteger i
irValue e = throwError $ "cannot compute at compile time: " <> tshow e

irTopLevel :: TopLevel -> IR.Builder ()
irTopLevel (FunctionDeclaration returnType name params) = do
  returnType' <- irType returnType
  paramTypes <- traverse (irType . Expr.paramType) params
  let typ = IR.Function returnType' paramTypes
  IR.declareGlobal name typ

irTopLevel (FunctionDefinition returnType name params body) = do
  returnType' <- irType returnType
  params' <- traverse irParameter params
  IR.defineFunction name returnType' params' $ concat <$> traverse irStatement body'
  where
    body'
      | name == "main" = body <> [Return (Literal (Integer 0))]
      | otherwise      = body

irTopLevel (GlobalDeclaration typ name) = do
  typ' <- irType typ
  IR.declareGlobal name typ'

irTopLevel (GlobalDefinition typ name value) = do
  typ' <- irType typ
  value' <- irValue value
  IR.defineGlobal name typ' value'

irFile :: C.File -> Either Text (Map Identifier IR.Definition)
irFile toplevels = IR.evalBuilder do
  traverse_ irTopLevel toplevels
  IR.defaultGlobals
