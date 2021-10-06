{-# LANGUAGE FlexibleInstances #-}

module Assembler
  ( compile
  ) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

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
  emit $ "global " <> name
  emit "align 8"
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
    emit "section .data"
    traverse_ (uncurry asmDefinition) (Map.assocs vars)
    emit "section .text"
    maybe (pure ()) (asmDefinition "main") (Map.lookup "main" funcs)
    traverse_ (uncurry asmDefinition) (Map.assocs $ Map.delete "main" funcs)
  pure $ Text.unlines assembly
  where
    isFunc IR.FunctionDefinition { } = True
    isFunc _ = False

asmDefinition :: Text -> IR.Definition -> Emitter ()
asmDefinition name definition = globalLabel name >> asm definition

push :: IR.Expression -> Emitter ()
push e = asm e >> emit "push rax"

-- | jumpIfNot skips to skipLabel if the condition is false.
jumpIfNot :: IR.Expression -> Text -> Emitter ()
jumpIfNot (Unary Not cond) target = jumpIf cond target

jumpIfNot (Binary And lhs rhs) target = do
  jumpIfNot lhs target
  jumpIfNot rhs target

jumpIfNot (Binary Or lhs rhs) target = do
  end <- newLabel "endor"
  jumpIf lhs end
  jumpIfNot rhs target
  label end

jumpIfNot (Binary op lhs rhs) target = do
  asmOperands lhs rhs
  case op of
    Eq  -> emit "cmp rax, rcx" >> emit ("jne " <> target)
    Neq -> emit "cmp rax, rcx" >> emit ("je "  <> target)
    Lt  -> emit "cmp rax, rcx" >> emit ("jge " <> target)
    Leq -> emit "cmp rax, rcx" >> emit ("jg "  <> target)
    Gt  -> emit "cmp rax, rcx" >> emit ("jle " <> target)
    Geq -> emit "cmp rax, rcx" >> emit ("jl "  <> target)
    _   -> asm op >> emit "cmp rax, 0" >> emit ("je " <> target)

jumpIfNot cond target = do
  asm cond
  emit "cmp rax, 0"
  emit $ "je " <> target

jumpIf :: IR.Expression -> Text -> Emitter ()
jumpIf (Unary Not cond) target = jumpIfNot cond target

jumpIf (Binary Or lhs rhs) target = do
  jumpIf lhs target
  jumpIf rhs target

jumpIf (Binary And lhs rhs) target = do
  end <- newLabel "endand"
  jumpIfNot lhs end
  jumpIf rhs target
  label end

jumpIf (Binary op lhs rhs) target = do
  asmOperands lhs rhs
  case op of
    Eq  -> emit "cmp rax, rcx" >> emit ("je "  <> target)
    Neq -> emit "cmp rax, rcx" >> emit ("jne " <> target)
    Lt  -> emit "cmp rax, rcx" >> emit ("jl "  <> target)
    Leq -> emit "cmp rax, rcx" >> emit ("jle " <> target)
    Gt  -> emit "cmp rax, rcx" >> emit ("jg "  <> target)
    Geq -> emit "cmp rax, rcx" >> emit ("jge " <> target)
    _   -> asm op >> emit "cmp rax, 0" >> emit ("jne " <> target)

jumpIf cond target = do
  asm cond
  emit "cmp rax, 0"
  emit $ "jne " <> target

class Asm a where
  asm :: a -> Emitter ()

instance Asm a => Asm [a] where
  asm = traverse_ asm

instance Asm IR.Definition where
  asm (IR.FunctionDefinition _returnType locals body) = do
    emit "push rbp"
    emit "mov rbp, rsp"
    unless (locals == 0) $ emit $ "sub rsp, " <> tshow locals
    asm body

  asm (IR.GlobalDefinition IR.Int (IR.Int64 i)) = do
    emit $ "dq " <> tshow i

  asm (IR.GlobalDefinition IR.Function{} _) = error "bad GlobalDefinition Function"

instance Asm IR.Expression where
  asm (Assignment (IR.BPOffset var) value) = do
    asm value
    emit $ "mov [rbp+" <> tshow var <> "], rax"

  asm (Assignment (IR.SPOffset var) value) = do
    asm value
    emit $ "mov [rsp+" <> tshow var <> "], rax"

  asm (Assignment (IR.Label name IR.Function { }) _) =
    throwError $ "cannot assign to function " <> tshow name

  asm (Assignment (IR.Label name _typ) value) = do
    asm value
    emit $ "mov [" <> name <> "], rax"

  asm (Call (Variable (IR.Label f (IR.Function _ret params))) args) = do
    unless (length args == length params) $ throwError $
      "expected " <> tshow (length params) <> " arguments but got " <> tshow (length args)
    traverse_ push $ reverse args
    emit $ "call " <> f
    unless (null args) $ emit $ "add rsp, " <> tshow (8 * length args)

  asm (Call (Variable (IR.Label f _)) _)
    = throwError $ "cannot call " <> tshow f <> " because it's not a function"

  asm (Call _ _) = error "only direct calls are supported so far"

  asm (Variable var) = asm var
  asm (Literal (C.Integer 0)) = emit "xor rax, rax"
  asm (Literal (C.Integer i)) = emit $ "mov rax, " <> tshow i
  asm (Unary op e) = asm e >> asm op
  asm (Binary Com l r) = asm l >> asm r
  asm (Binary And l r) = do
    end <- newLabel "endand"
    asm l
    emit "cmp rax, 0"
    emit $ "je " <> end
    asm r
    emit "cmp rax, 0"
    emit "mov rax, 0"
    emit "setne al"
    label end

  asm (Binary Or l r) = do
    end <- newLabel "endor"
    asm l
    emit "cmp rax, 0"
    emit $ "jne " <> end
    asm r
    emit "cmp rax, 0"
    label end
    emit "mov rax, 0"
    emit "setne al"

  asm (Ternary cond t f) = do
    fLabel <- newLabel "elseternary"
    end <- newLabel "endternary"
    jumpIfNot cond fLabel
    asm t
    emit $ "jmp " <> end
    label fLabel
    asm f
    label end

  asm (Binary op l r) = asmOperands l r >> asm op

-- | asmOperands places the lhs in rax and the rhs in rcx.
asmOperands :: IR.Expression -> IR.Expression -> Emitter ()
asmOperands l r = case (unaryAsm "rax" l, unaryAsm "rcx" r) of
  (_, Just rasm) -> asm l >> rasm
  (Just lasm, _) -> asm r >> emit "mov rcx, rax" >> lasm
  (Nothing, Nothing) -> push r >> asm l >> emit "pop rcx"

  where
    unaryAsm :: Text -> Expression id -> Maybe (Emitter ())
    unaryAsm reg (Literal (C.Integer i)) = Just $ do
      emit $ "mov " <> reg <> ", " <> tshow i

    unaryAsm reg (Unary Neg e) = do
      a <- unaryAsm reg e
      Just $ do
        a
        emit $ "neg " <> reg

    unaryAsm reg (Unary Inv e) = do
      a <- unaryAsm reg e
      Just $ do
        a
        emit $ "not " <> reg

    unaryAsm _reg _ = Nothing

instance Asm Unary where
  asm Neg = emit "neg rax"
  asm Inv = emit "not rax"
  asm Not = do
    emit "cmp rax, 0"
    emit "mov rax, 0"
    emit "sete al"

compareAsm :: Emitter ()
compareAsm = emit "cmp rax, rcx" >> emit "mov rax, 0"

-- lhs in rax, rhs in rcx, out in rax
instance Asm Binary where
  asm Div = emit "cqo" >> emit "idiv rcx"
  asm Mul = emit "imul rcx"
  asm Add = emit "add rax, rcx"
  asm Sub = emit "sub rax, rcx"
  asm Mod = emit "cqo" >> emit "idiv rcx" >> emit "mov rax, rdx"
  asm Eq  = compareAsm >> emit "sete al"
  asm Neq = compareAsm >> emit "setne al"
  asm Lt  = compareAsm >> emit "setl al"
  asm Leq = compareAsm >> emit "setle al"
  asm Gt  = compareAsm >> emit "setg al"
  asm Geq = compareAsm >> emit "setge al"
  asm op = error $ "operator not yet implemented: " <> show op

instance Asm IR.Reference where
  asm (IR.BPOffset i) = emit $ "mov rax, [rbp+" <> tshow i <> "]"
  asm (IR.Label l _) = emit $ "mov rax, [" <> l <> "]"
  asm (IR.SPOffset i) = emit $ "mov rax, [rsp+" <> tshow i <> "]"

instance Asm IR.Statement where
  asm (IR.Expression e) = asm e
  asm IR.Break = gets eBreak >>= maybe (throwError "break outside loop") jmp
  asm IR.Continue = gets eContinue >>= maybe (throwError "continue outside loop") jmp
  asm (IR.Return e) = do
    asm e
    emit "mov rsp, rbp"
    emit "pop rbp"
    emit "ret"

  asm (IR.If cond tBlock []) = do
    end <- newLabel "endif"
    jumpIfNot cond end
    asm tBlock
    label end

  asm (IR.If cond tBlock fBlock) = do
    fLabel <- newLabel "else"
    end <- newLabel "endif"
    jumpIfNot cond fLabel
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
    jumpIf condition bodyLabel

    label breakLabel

    modify $ \s -> s { eBreak = outerBreak, eContinue = outerContinue }
