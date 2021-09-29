{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Assembler
  ( compile
  ) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import qualified C
import Expr (Binary(..), Expression(..), Unary(..))
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

label :: Text -> Emitter ()
label name = emit $ name <> ":"

globalLabel :: Text -> Emitter ()
globalLabel name = do
  emit $ ".globl " <> name
  emit ".align 8"
  emit $ name <> ":"

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
  let (funcs, vars) = Map.partition isFunc ir
  assembly <- runEmitter do
    emit ".data"
    traverse_ (uncurry asmDefinition) (Map.assocs vars)
    emit ".text"
    traverse_ (uncurry asmDefinition) (Map.assocs funcs)
  pure $ Text.unlines assembly
  where
    isFunc IR.FunctionDefinition { } = True
    isFunc _ = False

asmDefinition :: Text -> IR.Definition -> Emitter ()
asmDefinition name definition = globalLabel name >> asm definition

pushExpr :: IR.Expression -> Emitter ()
pushExpr e = asm e >> emit "pushq %rax"

class Asm a where
  asm :: a -> Emitter ()

instance Asm a => Asm [a] where
  asm = traverse_ asm

instance Asm IR.Definition where
  asm (IR.FunctionDefinition _returnType locals body) = do
    emit "pushq %rbp"
    emit "movq %rsp, %rbp"
    unless (locals == 0) $ emit $ "subq $" <> tshow locals <> ", %rsp"
    asm body

  asm (IR.GlobalDefinition IR.Int (IR.Int64 i)) = do
    emit $ ".long " <> tshow i

  asm (IR.GlobalDefinition IR.Function{} _) = error "bad GlobalDefinition Function"

instance Asm IR.Expression where
  asm (Assignment (IR.BPOffset var) value) = do
    asm value
    emit $ "movq %rax, " <> tshow var <> "(%rbp)"

  asm (Assignment (IR.SPOffset var) value) = do
    asm value
    emit $ "movq %rax, -" <> tshow var <> "(%rsp)"

  asm (Assignment (IR.Label name IR.Function { }) _) =
    throwError $ "cannot assign to function " <> tshow name

  asm (Assignment (IR.Label name _typ) value) = do
    asm value
    emit $ "movq %rax, " <> name

  asm (Call (Variable (IR.Label f (IR.Function _ret params))) args) = do
    unless (length args == length params) $ throwError $
      "expected " <> tshow (length params) <> " arguments but got " <> tshow (length args)
    traverse_ pushExpr $ reverse args
    emit $ "call " <> f
    unless (null args) $ emit $ "add $" <> tshow (8 * length args) <> ", %rsp"

  asm (Call (Variable (IR.Label f _)) _)
    = throwError $ "cannot call " <> tshow f <> " because it's not a function"

  asm (Call _ _) = error "only direct calls are supported so far"

  asm (Variable var) = asm var
  asm (Literal (C.Integer 0)) = emit "xorq %rax, %rax"
  asm (Literal (C.Integer i)) = emit $ "movq $" <> tshow i <> ", %rax"
  asm (Unary op e) = asm e >> asm op
  asm (Binary Com l r) = asm l >> asm r
  asm (Binary And l r) = do
    end <- newLabel "endand"
    asm l
    emit "cmpq $0, %rax"
    emit $ "je " <> end
    asm r
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "setne %al"
    label end

  asm (Binary Or l r) = do
    end <- newLabel "endor"
    asm l
    emit "cmpq $0, %rax"
    emit $ "jne " <> end
    asm r
    emit "cmpq $0, %rax"
    label end
    emit "movq $0, %rax"
    emit "setne %al"

  asm (Ternary cond t f) = do
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

  asm (Binary op l r) = do
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
      unaryAsm :: Text -> Expression id -> Maybe (Emitter ())
      unaryAsm reg (Literal (C.Integer i)) = Just $ do
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

      unaryAsm _reg _ = Nothing

instance Asm Unary where
  asm Neg = emit "neg %rax"
  asm Inv = emit "not %rax"
  asm Not = do
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "sete %al"

compareAsm :: Emitter ()
compareAsm = emit "cmpq %rcx, %rax" >> emit "movq $0, %rax"

-- lhs in rax, rhs in rcx, out in rax
instance Asm Binary where
  asm Div = emit "cqo" >> emit "idivq %rcx"
  asm Mul = emit "imul %rcx"
  asm Add = emit "add %rcx, %rax"
  asm Sub = emit "sub %rcx, %rax"
  asm Mod = emit "cqo" >> emit "idivq %rcx" >> emit "movq %rdx, %rax"
  asm Eq  = compareAsm >> emit "sete %al"
  asm Neq = compareAsm >> emit "setne %al"
  asm Lt  = compareAsm >> emit "setl %al"
  asm Leq = compareAsm >> emit "setle %al"
  asm Gt  = compareAsm >> emit "setg %al"
  asm Geq = compareAsm >> emit "setge %al"
  asm op = error $ "operator not yet implemented: " <> show op

instance Asm IR.Reference where
  asm (IR.BPOffset i) = emit $ "movq " <> tshow i <> "(%rbp), %rax"
  asm (IR.Label l _) = emit $ "movq " <> l <> ", %rax"
  asm (IR.SPOffset i) = emit $ "movq -" <> tshow i <> "(%rsp), %rax"

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

  asm (IR.Loop iterationCount condition body update) = do
    breakLabel <- newLabel "break"
    continueLabel <- newLabel "continue"
    bodyLabel <- newLabel "loopbody"
    checkLabel <- newLabel "loopcheck"

    outerBreak <- gets eBreak
    outerContinue <- gets eContinue
    modify $ \s -> s { eBreak = Just breakLabel, eContinue = Just continueLabel }

    case iterationCount of
      IR.ZeroOrMore -> jmp checkLabel
      IR.OneOrMore -> pure ()

    label bodyLabel
    asm body

    label continueLabel
    asm update

    label checkLabel
    asm condition
    emit "cmpq $0, %rax"
    emit $ "jne " <> bodyLabel

    label breakLabel

    modify $ \s -> s { eBreak = outerBreak, eContinue = outerContinue }
