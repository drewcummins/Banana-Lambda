module Language.Grammar
( UnOp(..)
, BinOp(..)
, NnOp(..)
, Expression(..)
, Statement(..)
) where



data Expression	= Unary UnOp Expression
		| Binary BinOp Expression Expression
		| Nnary NnOp [ Expression ]
		| FunctionCall String [ Expression ]
		| ListCall String Expression
		| ExprList [ Expression ]
		| Null
		| ABool Bool
		| Atomic String 
		| AInt Integer deriving (Show, Eq)

data UnOp	= Factorial | Negate deriving (Show, Eq)

data BinOp	= Plus 
		| Minus 
		| Power 
		| Times 
		| Divide 
		| Equation
		| Equal 
		| GreaterThan
		| GreaterThanEqual
		| LessThan
		| LessThanEqual deriving (Show, Eq)

data NnOp	= NPlus | NTimes deriving (Show, Eq)

data Statement	= Evaluate Expression
		| Assignment String Expression
		| Define String [ Expression ] Statement
		| If Expression Statement Statement
		| Loop Expression Statement
		| Program [ Statement ] deriving (Show, Eq)


