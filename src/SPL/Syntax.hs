module SPL.Syntax where

data Source = Source !FilePath !Int !Int !Int !Int
data Sourced a = Sourced
  { source :: Source
  , unSourced :: a
  } deriving (Functor)

data Builtin
  = Negate
  | Not
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  | And
  | Or
  | Tuple
  | Prepend
  | Print
  | IsEmpty
  | EmptyList
  deriving (Show)

data Field
  = Head
  | Tail
  | First
  | Second
  deriving (Show)

data Expr' id f
  = Integer Integer
  | Boolean Bool
  | Character Char
  | Call id [f (Expr' id f)]
  | Builtin Builtin [f (Expr' id f)]
  | Variable id
  | Access (f (Expr' id f)) Field

type SourcedExpr id = Sourced (Expr' id Sourced)

data Statement id
  = If (SourcedExpr id) [Statement id] [Statement id]
  | While (SourcedExpr id) [Statement id]
  | Assign id [Field] (SourcedExpr id)
  | Expression (SourcedExpr id)
  | Return (SourcedExpr id)
  | Vardec (Maybe (Type id)) id (SourcedExpr id)
  | Fundec id [id] (Maybe (Type id)) [Sourced (Statement id)]

data Class = Ord | Eq | Show | All deriving (Eq, Ord, Show)

data Type id
  = Bool
  | Char
  | For Class id (Type id)
  | FunctionOf [Type id] (Type id)
  | Int
  | ListOf (Type id)
  | TupleOf [Type id]
  | Typevar id
  | Void
  deriving (Show)
