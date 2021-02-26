{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Assembler
  ( compile
  ) where

import Control.Monad.State (State, execState, gets, modify)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified C
import qualified Expr
import qualified IR
import Util (Error, tshow)

data EmitState = EmitState
  { eLabelNum :: Int
  , eLines :: [Text]
  }

type Emitter = State EmitState

runEmitter :: Emitter () -> [Text]
runEmitter emitter = reverse $ eLines $ execState emitter initialEmitState
  where
    initialEmitState = EmitState
      { eLabelNum = 0
      , eLines = []
      }

newLabel :: Emitter Text
newLabel = do
  labelNum <- gets eLabelNum
  modify $ \s -> s { eLabelNum = labelNum + 1 }
  pure $ "anon" <> tshow labelNum

emit :: Text -> Emitter ()
emit line = modify $ \s -> s { eLines = line : eLines s }

--saving :: Text -> Text -> Emitter a -> Emitter a
--saving rin rout comp = do
--  emit $ "push %" <> rin
--  result <- comp
--  emit $ "pop %" <> rout
--  pure result

compile :: C.File -> Either Error Text
compile file = do
  ir <- C.irFile file
  pure $ Text.unlines $ runEmitter $ asm ir

class Asm a where
  asm :: a -> Emitter ()

instance Asm a => Asm [a] where
  asm = traverse_ asm

instance Asm IR.TopLevel where
  asm (IR.Fdef _returnType name _parameters locals body) = do
    emit $ ".globl " <> name
    emit $ name <> ":"
    emit "pushq %rbp"
    emit "movq %rsp, %rbp"
    emit $ "subq $" <> tshow locals <> ", %rsp"
    asm body

instance Asm IR.Expression where
  asm (Expr.Assignment (IR.BPOffset var) value) = do
    asm value
    emit $ "movq %rax, -" <> tshow var <> "(%rbp)"

  asm (Expr.Term (Expr.Variable var)) = asm var
  asm (Expr.Term (C.Literal (C.Integer i))) = emit $ "movq $" <> tshow i <> ", %rax"
  asm (C.Unary op e) = asm e >> asm op
  asm (C.Binary C.And l r) = do
    end <- newLabel
    asm l
    emit "cmpq $0, %rax"
    emit $ "je " <> end
    asm r
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "setne %al"
    emit $ end <> ":"

  asm (C.Binary C.Or l r) = do
    end <- newLabel
    asm l
    emit "cmpq $0, %rax"
    emit $ "jne " <> end
    asm r
    emit "cmpq $0, %rax"
    emit $ end <> ":"
    emit "movq $0, %rax"
    emit "setne %al"

  asm (C.Ternary cond t f) = do
    fLabel <- newLabel
    end <- newLabel
    asm cond
    emit "cmpq $0, %rax"
    emit $ "je " <> fLabel
    asm t
    emit $ "jmp " <> end
    emit $ fLabel <> ":"
    asm f
    emit $ end <> ":"

  asm (C.Binary op l r) = do
    case (unaryAsm "rax" l, unaryAsm "rcx" r) of
      (_, Just rasm) -> asm l >> rasm >> asm op
      (Just lasm, _) -> asm r >> emit "movq %rax, %rcx" >> lasm >> asm op
      (Nothing, Nothing) -> do
        asm r
        emit "pushq %rax"
        asm l
        emit "popq %rcx"
        asm op

    where
      unaryAsm :: Text -> Expr.Expression id -> Maybe (Emitter ())
      unaryAsm reg (Expr.Term (C.Literal (C.Integer i))) = Just $ do
        emit $ "movq $" <> tshow i <> ", %" <> reg

      unaryAsm reg (C.Unary C.Neg e) = do
        a <- unaryAsm reg e
        Just $ do
          a
          emit $ "neg %" <> reg

      unaryAsm reg (C.Unary C.Inv e) = do
        a <- unaryAsm reg e
        Just $ do
          a
          emit $ "not %" <> reg

      unaryAsm _reg _ = Nothing

instance Asm C.Unary where
  asm C.Neg = emit "neg %rax"
  asm C.Inv = emit "not %rax"
  asm C.Not = do
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "sete %al"

compareAsm :: Emitter ()
compareAsm = emit "cmpq %rcx, %rax" >> emit "movq $0, %rax"

-- lhs in rax, rhs in rcx, out in rcx
instance Asm C.Binary where
  asm C.Div = emit "cqo" >> emit "idivq %rcx"
  asm C.Mul = emit "imul %rcx"
  asm C.Add = emit "add %rcx, %rax"
  asm C.Sub = emit "sub %rcx, %rax"
  asm C.Eq  = compareAsm >> emit "sete %al"
  asm C.Neq = compareAsm >> emit "setne %al"
  asm C.Lt  = compareAsm >> emit "setl %al"
  asm C.Leq = compareAsm >> emit "setle %al"
  asm C.Gt  = compareAsm >> emit "setg %al"
  asm C.Geq = compareAsm >> emit "setge %al"
  asm C.Com = pure ()
  asm op = error $ "operator not yet implemented: " <> show op

instance Asm IR.BPOffset where
  asm (IR.BPOffset i) = emit $ "movq -" <> tshow i <> "(%rbp), %rax"

instance Asm IR.Statement where
  asm (IR.If cond tBlock []) = do
    end <- newLabel
    asm cond
    emit "cmpq $0, %rax"
    emit $ "je " <> end
    asm tBlock
    emit $ end <> ":"

  asm (IR.If cond tBlock fBlock) = do
    fLabel <- newLabel
    end <- newLabel
    asm cond
    emit "cmpq $0, %rax"
    emit $ "je " <> fLabel
    asm tBlock
    emit $ "jmp " <> end
    emit $ fLabel <> ":"
    asm fBlock
    emit $ end <> ":"

  asm (IR.Expression e) = asm e
  asm (IR.Return e) = do
    asm e
    emit "movq %rbp, %rsp"
    emit "popq %rbp"
    emit "ret"
