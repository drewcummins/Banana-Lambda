module Language.Parser
( parseProgram
) where


import Control.Monad(liftM)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Language.Grammar

def = emptyDef	{ commentStart = "/*"
		, commentEnd = "*/"
		, identStart = letter
		, identLetter = alphaNum
		, opStart = oneOf "!+-^*/=:()[]<>"
		, opLetter = oneOf "!+-^*/=:()[]<>"
		, reservedOpNames = [ 	"!", "+", "-", "*", "/", "=", 
					":=", "()", "[", "]", "(", ")", 
					":", "==", ">", ">=", "<", "<=",
					"=" ] 
		, reservedNames = [ 	"if", "then", "else", "endif", 
					"while", "do", "enddo",
					"true", "false", "null",
					"print", "def", "enddef", "return" ] 
		}	

TokenParser	{ parens = m_parens
		, identifier = m_identifier
		, reserved = m_reserved
		, reservedOp = m_reservedOp
		, semiSep = m_semiSep
		, commaSep = m_commaSep
		, brackets = m_brackets
		, integer = m_integer
		, whiteSpace = m_whiteSpace } = makeTokenParser def

exprParser :: Parser Expression
exprParser = buildExpressionParser table term <?> "expression"

table = [ [ Postfix (m_reservedOp "!" >> return (Unary Factorial)) ]
	, [ Prefix (m_reservedOp "-" >> return (Unary Negate)) ]
	, [ Infix (m_reservedOp "^" >> return (Binary Power)) AssocLeft ]
	, [ Infix (m_reservedOp "*" >> return (Binary Times)) AssocLeft ]
	, [ Infix (m_reservedOp "/" >> return (Binary Divide)) AssocLeft ]
	, [ (Infix (m_reservedOp "+" >> return (Binary Plus)) AssocLeft), (Infix (m_reservedOp "-" >> return (Binary Minus)) AssocLeft) ]
	, [ Infix (m_reservedOp "==" >> return (Binary Equal)) AssocLeft ]
	, [ Infix (m_reservedOp "=" >> return (Binary Equation)) AssocLeft ]
	] 

term = 	m_parens exprParser
	<|> try functionCallParser
	<|> try listCallParser
	<|> liftM Atomic m_identifier
	<|> liftM AInt m_integer
	<|> listParser
	<|> (m_reserved "null" >> return Null)
	<|> (m_reserved "true" >> return (ABool True))
	<|> (m_reserved "false" >> return (ABool False))
		


assignmentParser :: Parser Statement
assignmentParser = 	do
			var <- m_identifier
			m_reservedOp ":="
			expression <- exprParser
			return (Assignment var expression)

listParser :: Parser Expression
listParser = do
		m_reservedOp "["
		elements <- m_commaSep exprParser
		m_reservedOp "]"
		return (ExprList elements)

functionCallParser :: Parser Expression
functionCallParser = do
			def <- m_identifier
			m_reserved "("
			args <- m_commaSep exprParser
			m_reserved ")"
			return (FunctionCall def args)

listCallParser :: Parser Expression
listCallParser = do
			def <- m_identifier
			m_reserved "["
			i <- exprParser
			m_reserved "]"
			return (ListCall def i)


stmtParser :: Parser Statement
stmtParser = fmap Program (m_semiSep stmt)
	where
		stmt = 	try assignmentParser
			<|> do
				m_reserved "def"
				def <- m_identifier
				m_reserved "("
				args <- m_commaSep (liftM Atomic m_identifier)
				m_reserved ")"
				m_reserved ":"
				program <- stmtParser
				m_reserved ":"
				return (Define def args program)
			<|> do
				m_reserved "if"
				expression <- exprParser
				m_reserved "then"
				left <- stmtParser
				m_reserved "else"
				right <- stmtParser
				m_reserved "endif"
				return (If expression left right)
			<|> do
				m_reserved "while"
				expression <- exprParser
				m_reserved ":"
				stmt <- stmtParser
				m_reserved ":"
				return (Loop expression stmt)
			<|> do
				m_reserved "print"
				expression <- exprParser
				return (Evaluate expression)
			<|> do
				m_reserved "return"
				expression <- exprParser
				return (Evaluate expression)
			<|> do
				expression <- exprParser
				return (Evaluate expression)




parseProgram :: String -> Either ParseError Statement
parseProgram input = parse stmtParser "" input

