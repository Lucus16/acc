module SPL.Infer where

import Data.Text (Text)
import Control.Monad.Writer (MonadWriter, Writer, tell)
import Control.Monad.State (StateT)
import Data.Map (Map)

import SPL.Syntax

data Constraint = SubtypeOf (Type Text) (Type Text)

constrain :: MonadWriter (Sourced Constraint) m => Type Text -> Type Text -> Source -> m ()
constrain found expected src = tell $ Sourced src $ found `SubtypeOf` expected

statementConstraints :: Type id -> Statement id -> StateT (Map id (Type id)) (Writer (Sourced Constraint)) ()
statementConstraints returnType (Return (Sourced eSrc e)) = do
  eType <- expressionConstraints e
  constrain eType returnType eSrc

statementConstraints _ (Fundec name params mbType body) = do
  expected <- declare name mbType
  paramTypes <- for params $ \param -> declare param Nothing
  returnType <- freshType [ name, "result" ]
  let found = FunctionOf paramTypes returnType
  constrain found expected bodySrc
  for_ body $ statementConstraints returnType

expressionConstraints :: Type id -> SourcedExpr id -> Writer (Sourced Constraint) ()
expressionConstraints resultType (Sourced callSrc (Call (Sourced funSrc fun) args)) = do
  funType <- lookup fun
  case funType of
    FunctionOf paramTypes returnType | length paramTypes == length args
      -> do
        for_ (zip paramTypes args) $ uncurry expressionConstraints
        constrain returnType resultType callSrc

    _ -> do
      paramTypes <- for args $ const freshType
      returnType <- freshType
      constrain funType (FunctionOf paramTypes returnType) funSrc
      for_ (zip paramTypes args) $ uncurry expressionConstraints
      constrain returnType resultType callSrc

--unify :: Type -> Type -> Infer Bool
--unify Bool Bool = pure True
--unify Char Char = pure True
--unify Int  Int  = pure True
--unify Void Void = pure True
--unify (ListOf x) (ListOf y) = unify x y
--unify (FunctionOf args1 ret1) (FunctionOf args2 ret2) =
--  if length args1 /= length args2
--     then pure False
--     else undefined
--
--unify (For All a l) r = withFreshLeft a $ unify l r
--unify (Typevar a) r = do
--  l <- lookup a
--  unify l r

-- I should build steps that work first:
-- - constraint generation

-- When encountering a call while inferring, first infer the called function
-- unless we were already working on that. At the end of inferring a function,
-- simplify the type.

-- To infer a function, first infer all functions it calls that aren't being
-- inferred already. Then simplify its type only if all functions it calls have
-- been inferred and aren't still being inferred.

-- The simplification step is not so different from constraint resolution. It
-- seems a two-pass inference algorithm is unavoidable in the presence of
-- recursive functions.

-- Only when comparing forall types can one type be a subtype of another.

-- Infer computes the result type of an expression and then restricts the
-- place it gets stored in to that type.

-- To infer a call, get the callee type, restrict it to be a function type with
-- the given number of arguments, then for each argument, infer the type of its
-- expression, then check it matches the corresponding parameter, at the end,
-- return the return type of the function.

-- To infer a fundec, create new typevars for every parameter and for the return
-- type and store them, then restrict each parameter whenever it is used and
-- finally return the type of the result.

-- If I pass a function foo to a function bar and then call foo somewhere in
-- bar, I won't know the type of foo yet. I can restrict the type of foo to a
-- function with a specific number of args at that point. I will create new
-- typevars for each of that function's parameters and for its return type.
-- I can then restrict each of those parameters to support at least the types
-- inferred from the arguments.

-- I should fuzz the compiler!

--inferExpr :: Expr' id (SourcedExpr id) -> Infer id Type
--inferExpr (Boolean _) = pure Bool
--inferExpr (Character _) = pure Char
--inferExpr (Integer _) = pure Int
--inferExpr EmptyList = ListOf <$> newType "elem"
--inferExpr (Variable (Sourced _ var)) = lookup var
--inferExpr (Not (Sourced eSrc e)) = do
--  eType <- inferExpr e
--  restrict eType Bool 1 "!" eSrc
--  pure Bool
--
--inferExpr (Add (Sourced lSrc l) (Sourced rSrc r)) = do
--  lType <- inferExpr l
--  restrict lType Int 1 "+" lSrc
--  rType <- inferExpr r
--  restrict rType Int 2 "+" rSrc
--  pure Int

-- renderTypeError
--renderSource eSrc
--  <> "  The first argument of `!` should be: Bool"
--  <> "                  but was found to be: " <> renderType eType

a :: Type Text
a = Typevar "a"

b :: Type Text
b = Typevar "b"

builtinType :: Builtin -> Type Text
builtinType Negate         = FunctionOf [Int] Int
builtinType Add            = FunctionOf [Int, Int] Int
builtinType Subtract       = FunctionOf [Int, Int] Int
builtinType Multiply       = FunctionOf [Int, Int] Int
builtinType Divide         = FunctionOf [Int, Int] Int
builtinType Modulo         = FunctionOf [Int, Int] Int

builtinType Equal          = For Eq  "a" (FunctionOf [a, a] Bool)
builtinType NotEqual       = For Eq  "a" (FunctionOf [a, a] Bool)
builtinType GreaterOrEqual = For Ord "a" (FunctionOf [a, a] Bool)
builtinType GreaterThan    = For Ord "a" (FunctionOf [a, a] Bool)
builtinType LessOrEqual    = For Ord "a" (FunctionOf [a, a] Bool)
builtinType LessThan       = For Ord "a" (FunctionOf [a, a] Bool)

builtinType Not            = FunctionOf [Bool] Bool
builtinType And            = FunctionOf [Bool, Bool] Bool
builtinType Or             = FunctionOf [Bool, Bool] Bool

builtinType Prepend        = For All "a" (FunctionOf [a, ListOf a] (ListOf a))
builtinType EmptyList      = For All "a" (ListOf a)
