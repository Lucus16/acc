module SPL.Resolver where

import Prelude hiding (lookup)

import Control.Monad.State (State, gets, modify, runState)
import Control.Monad.Writer (runWriterT, WriterT, tell)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Functor (void)

import qualified Data.Map as Map

import SPL.Syntax

import Pretty
import Util (tshow)

type Name = Text

data ResolverError
  = Shadows (Sourced Name) (Sourced Name)
  | Undefined (Sourced Name)

instance Pretty ResolverError where
  pretty (x `Shadows` y) = do
    pretty (source x) >> text ": error:" >> line
    indentBy "\t" $ do
      pretty (unSourced x) >> text " shadows earlier binding at:" >> line
      pretty (source y) >> line
  pretty (Undefined x) = do
    pretty (source x) >> text ": error:" >> line
    indentBy "\t" $ text "undefined: " >> pretty (unSourced x) >> line

data ResolverState = ResolverState
  { rsNameCount :: Map Name Int
  , rsScope :: Map Name Ident
  , rsSourceMap :: Map Ident (Sourced Name)
  }

type Resolver = WriterT [ResolverError] (State ResolverState)

block :: Resolver a -> Resolver a
block inner = do
  outer <- gets rsScope
  result <- inner
  modify $ \s -> s { rsScope = outer }
  pure result

fresh :: Sourced Name -> Resolver Ident
fresh (Sourced src base) = do
  n <- gets $ fromMaybe 0 . Map.lookup base . rsNameCount
  let ident = Ident $ base <> "_" <> tshow n
  modify $ \s -> s
    { rsNameCount = Map.insert base (n + 1) (rsNameCount s)
    , rsSourceMap = Map.insert ident (Sourced src base) (rsSourceMap s)
    }
  pure ident

getSource :: Ident -> Resolver (Sourced Name)
getSource i = gets $ fromMaybe err . Map.lookup i . rsSourceMap
  where err = error $ "generated ident " <> show i <> " was not stored in rsSourceMap"

declare :: Sourced Name -> Resolver Ident
declare name = do
  found <- gets $ Map.lookup (unSourced name) . rsScope
  case found of
    Just x -> getSource x >>= (tell . pure . (name `Shadows`))
    Nothing -> pure ()
  ident <- fresh name
  modify $ \s -> s { rsScope = Map.insert (unSourced name) ident (rsScope s) }
  pure ident

lookup :: Sourced Name -> Resolver Ident
lookup name = do
  found <- gets $ Map.lookup (unSourced name) . rsScope
  case found of
    Just x -> pure x
    Nothing -> do
      tell [Undefined name]
      declare name

implicit :: Sourced Name -> Resolver Ident
implicit name = do
  found <- gets $ Map.lookup (unSourced name) . rsScope
  maybe (declare name) pure found

runResolver :: [Name] -> Resolver a -> Either [ResolverError] (a, Map Ident (Sourced Name))
runResolver builtins r = case errors of
  [] -> Right (resolved, rsSourceMap)
  _  -> Left errors
  where
    ((resolved, errors), ResolverState { rsSourceMap }) = runState (runWriterT r) initialState
    initialState = ResolverState
      { rsNameCount = Map.empty
      , rsScope = Map.fromList $ fmap (\builtin -> (builtin, Ident builtin)) builtins
      , rsSourceMap = Map.fromList $ fmap (\builtin -> (Ident builtin, Sourced Internal builtin)) builtins
      }

declareIfFundec :: Sourced (Statement Name) -> Resolver ()
declareIfFundec (Sourced src (Fundec name _ _ _)) = void $ declare $ Sourced src name
declareIfFundec _ = pure ()

resolveType :: Sourced (Type Name) -> Resolver (Type Ident)
resolveType (Sourced src typ)= traverse (implicit . Sourced src) typ

resolveExpression :: SourcedExpr Name -> Resolver (SourcedExpr Ident)
resolveExpression (Sourced src (Boolean b)) = pure $ Sourced src $ Boolean b
resolveExpression (Sourced src (Character c)) = pure $ Sourced src $ Character c
resolveExpression (Sourced src (Integer i)) = pure $ Sourced src $ Integer i
resolveExpression (Sourced src (Variable v)) = fmap (Sourced src . Variable) $ lookup $ Sourced src v
resolveExpression (Sourced src (Access expr field)) = do
  expr' <- resolveExpression expr
  field' <- lookup $ Sourced src field
  pure $ Sourced src $ Access expr' field'

resolveExpression (Sourced src (Builtin builtin args)) =
  Sourced src . Builtin builtin <$> traverse resolveExpression args

resolveExpression (Sourced src (Call fun args)) = do
  fun' <- resolveExpression fun
  args' <- traverse resolveExpression args
  pure $ Sourced src $ Call fun' args'

resolveStatement :: Sourced (Statement Name) -> Resolver (Sourced (Statement Ident))
resolveStatement (Sourced src (Comment c)) = pure $ Sourced src $ Comment c
resolveStatement (Sourced src EmptyLine) = pure $ Sourced src EmptyLine
resolveStatement (Sourced src (Fundec name params typ body)) = block $ do
  name' <- lookup $ Sourced src name
  params' <- traverse (declare . Sourced src) params
  typ' <- traverse (resolveType . Sourced src) typ
  body' <- resolveStatements body
  pure $ Sourced src $ Fundec name' params' typ' body'

resolveStatement (Sourced src (Vardec typ name body)) = do
  typ' <- traverse (resolveType . Sourced src) typ
  name' <- declare $ Sourced src name
  body' <- resolveExpression body
  pure $ Sourced src $ Vardec typ' name' body'

resolveStatement (Sourced src (Return expr)) =
  Sourced src . Return <$> traverse resolveExpression expr

resolveStatement (Sourced src (Expression expr)) =
  Sourced src . Expression <$> resolveExpression expr

resolveStatement (Sourced src (Assign var fields expr)) = do
  var' <- lookup $ Sourced src var
  fields' <- traverse (lookup . Sourced src) fields
  expr' <- resolveExpression expr
  pure $ Sourced src $ Assign var' fields' expr'

resolveStatement (Sourced src (If cond t f)) = do
  cond' <- resolveExpression cond
  t' <- resolveStatements t
  f' <- resolveStatements f
  pure $ Sourced src $ If cond' t' f'

resolveStatement (Sourced src (While cond body)) = do
  cond' <- resolveExpression cond
  body' <- resolveStatements body
  pure $ Sourced src $ While cond' body'

resolveStatements :: [Sourced (Statement Name)] -> Resolver [Sourced (Statement Ident)]
resolveStatements stmts = do
  for_ stmts declareIfFundec
  for stmts resolveStatement

resolve :: [Sourced (Statement Name)] -> Either [ResolverError] ([Sourced (Statement Ident)], Map Ident (Sourced Name))
resolve = runResolver builtins . resolveStatements
  where builtins = ["fst", "snd", "hd", "tail", "print", "isEmpty"]
