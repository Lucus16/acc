{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Assembler
  ( compile
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, gets, modify)
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
  , eBreak :: Maybe Text
  , eContinue :: Maybe Text
  }

type Emitter = StateT EmitState (Either Text)

runEmitter :: Emitter () -> Either Text [Text]
runEmitter emitter = reverse . eLines <$> execStateT emitter initialEmitState
  where
    initialEmitState = EmitState
      { eLabelNum = 0
      , eLines = []
      , eBreak = Nothing
      , eContinue = Nothing
      }

newLabel :: Text -> Emitter Text
newLabel name = do
  labelNum <- gets eLabelNum
  modify $ \s -> s { eLabelNum = labelNum + 1 }
  pure $ name <> tshow labelNum

emit :: Text -> Emitter ()
emit line = modify $ \s -> s { eLines = line : eLines s }

block :: (Text -> Text -> Emitter a) -> Emitter a
block inner = do
  outerBreak <- gets eBreak
  outerContinue <- gets eContinue
  breakLabel <- newLabel "break"
  continueLabel <- newLabel "continue"
  modify $ \s -> s { eBreak = Just breakLabel, eContinue = Just continueLabel }
  label continueLabel
  result <- inner breakLabel continueLabel
  label breakLabel
  modify $ \s -> s { eBreak = outerBreak, eContinue = outerContinue }
  pure result

label :: Text -> Emitter ()
label name = emit $ name <> ":"

jmp :: Text -> Emitter ()
jmp name = emit $ "jmp " <> name

--saving :: Text -> Text -> Emitter a -> Emitter a
--saving rin rout comp = do
--  emit $ "push %" <> rin
--  result <- comp
--  emit $ "pop %" <> rout
--  pure result

compile :: C.File -> Either Error Text
compile file = do
  ir <- C.irFile file
  assembly <- runEmitter $ asm ir
  pure $ Text.unlines assembly

class Asm a where
  asm :: a -> Emitter ()

instance Asm a => Asm [a] where
  asm = traverse_ asm

instance Asm IR.TopLevel where
  asm (IR.Fdef _returnType name _parameters locals body) = do
    emit $ ".globl " <> name
    label name
    emit "pushq %rbp"
    emit "movq %rsp, %rbp"
    emit $ "subq $" <> tshow locals <> ", %rsp"
    asm body

instance Asm IR.Expression where
  asm (Expr.Assignment (IR.BPOffset var) value) = do
    asm value
    emit $ "movq %rax, -" <> tshow var <> "(%rbp)"

  asm (Expr.Term (Expr.Variable var)) = asm var
  asm (Expr.Term (C.Literal (C.Integer 0))) = emit "xorq %rax, %rax"
  asm (Expr.Term (C.Literal (C.Integer i))) = emit $ "movq $" <> tshow i <> ", %rax"
  asm (C.Unary op e) = asm e >> asm op
  asm (C.Binary C.Com l r) = asm l >> asm r
  asm (C.Binary C.And l r) = do
    end <- newLabel "endand"
    asm l
    emit "cmpq $0, %rax"
    emit $ "je " <> end
    asm r
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "setne %al"
    label end

  asm (C.Binary C.Or l r) = do
    end <- newLabel "endor"
    asm l
    emit "cmpq $0, %rax"
    emit $ "jne " <> end
    asm r
    emit "cmpq $0, %rax"
    label end
    emit "movq $0, %rax"
    emit "setne %al"

  asm (C.Ternary cond t f) = do
    fLabel <- newLabel "elseternary"
    end <- newLabel "endternary"
    asm cond
    emit "cmpq $0, %rax"
    emit $ "je " <> fLabel
    asm t
    emit $ "jmp " <> end
    label fLabel
    asm f
    label end

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

-- lhs in rax, rhs in rcx, out in rax
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
  asm op = error $ "operator not yet implemented: " <> show op

instance Asm IR.BPOffset where
  asm (IR.BPOffset i) = emit $ "movq -" <> tshow i <> "(%rbp), %rax"

instance Asm IR.Statement where
  asm (IR.Expression e) = asm e
  asm IR.Break = gets eBreak >>= maybe (throwError "break outside loop") jmp
  asm IR.Continue = gets eContinue >>= maybe (throwError "continue outside loop") jmp
  asm (IR.Return e) = do
    asm e
    emit "movq %rbp, %rsp"
    emit "popq %rbp"
    emit "ret"

  asm (IR.If cond tBlock []) = do
    end <- newLabel "endif"
    asm cond
    emit "cmpq $0, %rax"
    emit $ "je " <> end
    asm tBlock
    label end

  asm (IR.If cond tBlock fBlock) = do
    fLabel <- newLabel "else"
    end <- newLabel "endif"
    asm cond
    emit "cmpq $0, %rax"
    emit $ "je " <> fLabel
    asm tBlock
    emit $ "jmp " <> end
    label fLabel
    asm fBlock
    label end

  asm (IR.DoWhile body cond) = block $ \_break continue -> do
    asm body
    asm cond
    emit "cmpq $0, %rax"
    emit $ "jne " <> continue

  asm (IR.While cond body) = block $ \break_ continue -> do
    asm cond
    emit "cmpq $0, %rax"
    emit $ "je " <> break_
    asm body
    emit $ "jmp " <> continue
