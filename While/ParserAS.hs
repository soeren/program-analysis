module While.ParserAS where

-- Variable Identifier
type VarIdent = String

-- Function Identifier
type FuncIdent = String

-- Variable Type
type VarType = String

-- Label
type Label = Int

-- Program
-- It contains list of variable definitions, a list of functions
-- and a statement(which can be a sequence of statements)
data Prog
    = Prog [Define] [Func] Stat
    deriving (Eq, Show)

-- Arithmetic expression
-- Either Variable, Integer value, arithmetic operator or a
-- function call
data AExp
  = Var VarIdent
  | IntLit Integer
  | AOp String AExp AExp
  | FuncCall FuncIdent [AExp]
  deriving (Eq, Show)

-- Boolean expression
-- Either unary operator, boolean value, binary boolean operator, or
-- relative operator
data BExp
  = BUnOp String BExp
  | BoolLit Bool
  | BOp String BExp BExp
  | RelOp String AExp AExp
  deriving (Eq, Show)

-- Definition of variables with type and name
data Define
  = Define VarType VarIdent
  deriving (Show, Eq)

-- Program statements
-- Either an assignment, a skip, a read, a write, a sequence of
-- statements, an if, a while or a return
data Stat
  = Assign VarIdent AExp Label
  | Skip Label
  | Read VarIdent Label
  | Write AExp Label
  | Seq [Stat]
  | If BExp Label Stat Stat
  | While BExp Label Stat
  | Return AExp Label
  deriving (Show, Eq)

-- Function arguments
-- An argument for a function, it contains the variable
-- type, the variable identifier and its label
data Arg = Arg VarType VarIdent
           deriving (Eq, Show)

-- Function
-- It contains the return type, the function identifier,
-- a list of its arguments, a list of variable definitions,
-- a statement(which can be a sequence of statements) and
-- its label
data Func
    = Func VarType FuncIdent [Arg] [Define] Stat Label
      deriving (Eq, Show)
