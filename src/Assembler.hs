{-# LANGUAGE OverloadedStrings #-}

module Assembler
  ( Asm
  , asm
  , compile
  , emit
  , newLabel
  ) where

import Control.Monad.State (State, execState, modify, state)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text

import AST
import Util (tshow)

type Emitter = State (Int, [Text])

newLabel :: Emitter Text
newLabel = state $ \(i, a) -> ("anon" <> tshow i, (i + 1, a))

emit :: Text -> Emitter ()
emit line = modify $ \(i, a) -> (i, line:a)

compile :: Asm a => a -> Text
compile x =
  let (_, lines) = execState (asm x) (0, [])
   in Text.unlines (reverse lines)

class Asm a where
  asm :: a -> Emitter ()

instance Asm a => Asm [a] where
  asm = traverse_ asm

instance Asm TopLevel where
  asm (Fdef returnType name parameters body) = do
    emit $ ".globl " <> name
    emit $ name <> ":"
    emit "push %ebp"
    emit "movq %esp, %ebp"
    asm body

instance Asm Expression where
  asm (Term (Literal (Integer i))) = emit $ "movq $" <> tshow i <> ", %rax"
  asm (Unary op e) = asm e >> asm op
  asm (Binary And l r) = do
    end <- newLabel
    asm l
    emit "cmpq $0, %rax"
    emit $ "je " <> end
    asm r
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "setne %al"
    emit $ end <> ":"

  asm (Binary Or l r) = do
    end <- newLabel
    asm l
    emit "cmpq $0, %rax"
    emit $ "jne " <> end
    asm r
    emit "cmpq $0, %rax"
    emit $ end <> ":"
    emit "movq $0, %rax"
    emit "setne %al"

  asm (Binary op l r) = do
    case (unaryAsm "rax" l, unaryAsm "rcx" r) of
      (_, Just rasm) -> asm l >> rasm >> asm op
      (Just lasm, _) -> asm r >> emit "movq %rax, %rcx" >> lasm >> asm op
      (Nothing, Nothing) -> do
        asm r
        emit "push %rax"
        asm l
        emit "pop %rcx"
        asm op
    where
      unaryAsm reg (Binary _ _ _) = Nothing
      unaryAsm reg (Term (Literal (Integer i))) = Just $ do
        emit $ "movq $" <> tshow i <> ", %" <> reg

      unaryAsm reg (Unary Neg e) = do
        a <- unaryAsm reg e
        Just $ do
          a
          emit $ "neg %" <> reg

      unaryAsm reg (Unary Inv e) = do
        a <- unaryAsm reg e
        Just $ do
          a
          emit $ "not %" <> reg

      unaryAsm reg (Unary Not e) = Nothing

instance Asm Unary where
  asm Neg = emit "neg %rax"
  asm Inv = emit "not %rax"
  asm Not = do
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "sete %al"

compareAsm = emit "cmpq %rcx, %rax" >> emit "movq $0, %rax"

instance Asm Binary where
  asm Div = emit "cqo" >> emit "idivq %rcx"
  asm Mul = emit "imul %rcx"
  asm Add = emit "add %rcx, %rax"
  asm Sub = emit "sub %rcx, %rax"
  asm Eq  = compareAsm >> emit "sete %al"
  asm Neq = compareAsm >> emit "setne %al"
  asm Lt  = compareAsm >> emit "setl %al"
  asm Leq = compareAsm >> emit "setle %al"
  asm Gt  = compareAsm >> emit "setg %al"
  asm Geq = compareAsm >> emit "setge %al"

instance Asm Statement where
  asm (Return e) = do
    asm e
    emit "movq %ebp, %esp"
    emit "pop %ebp"
    emit "ret"
