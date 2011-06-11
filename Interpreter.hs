import Language.Parser
import Language.Grammar
import qualified Data.Map as Map
import Control.Exception
import Control.Monad(liftM)


data Register = ExprRegister Expression | StmtRegister [ Expression ] Statement deriving (Show, Eq)
type Memory = Map.Map String Register
data Stack = Global Memory | Local Memory
type Eval a = (Memory, Either String a) 


evalExpr :: (Integral a) => Expression -> Memory -> Eval a
evalExpr Null mem = (mem, Left "null")
evalExpr (FunctionCall def vals) mem = do
	let func = Map.lookup def mem
	case func of
		Nothing -> (mem, Left (def ++ "()"))
		Just (StmtRegister args f) -> (mem, eval)
			where
				(_, eval) = evalStmt f (Map.union closure mem)
				closure = assignArgs args vals Map.empty

evalExpr (Atomic x) mem = do
	let atom = Map.lookup x mem
	case atom of
		Nothing -> (mem, Left x)
		Just (ExprRegister val) -> evalExpr val mem

evalExpr (AInt x) mem = (mem, Right (fromIntegral x))
evalExpr (Binary Times x y) mem = evalBinary (*) "" (evalExpr x mem) (evalExpr y mem)
evalExpr (Binary Plus x y) mem = evalBinary (+) " + " (evalExpr x mem) (evalExpr y mem)
evalExpr (Binary Minus x y) mem = evalBinary (-) " - " (evalExpr x mem) (evalExpr y mem)
evalExpr (Binary Power x y) mem = evalBinary (^) "^" (evalExpr x mem) (evalExpr y mem)
evalExpr (Binary Divide x y) mem = evalBinary div " / " (evalExpr x mem) (evalExpr y mem)

evalExpr (Unary Factorial x) mem = do	
	let (m, fx) = evalExpr x mem
	case fx of 
		Left sym -> (m, Left (sym ++ "!"))
		Right num -> (m, Right (fromInteger (factorial $ toInteger num)))

evalExpr (Unary Negate x) mem = do
	let (m, nx) = evalExpr x mem
	case nx of
		Left sym -> (m, Left ("-" ++ sym))
		Right num -> (m, Right (-1 * num))

evalExpr (ABool False) m	= (m, Left "false")
evalExpr (ABool True) m		= (m, Left "true")

evalExpr (Binary Equal l r) m 
	| (evalExpr l m) == (evalExpr r m) = (m, Left "true")
	| otherwise = (m, Left "false")

assignArgs :: [ Expression ] -> [ Expression ] -> Memory -> Memory
assignArgs [] _ mem = mem
assignArgs _ [] mem = mem
assignArgs [ x ] [ y ] mem = assignArg x y mem
assignArgs (x:xs) (y:ys) mem = assignArg x y (assignArgs xs ys mem)

assignArg :: Expression -> Expression -> Memory -> Memory
assignArg (Atomic arg) val mem = Map.insert arg (ExprRegister val) mem


evalBinary :: (Integral a) => (a -> a -> a) -> String -> Eval a -> Eval a -> Eval a
evalBinary op name (mem, (Left x)) (_, (Left y)) = (mem, Left (x ++ name ++ y))
evalBinary op name (mem, (Left x)) (_, (Right y)) = (mem, Left (x ++ name ++ (show y)))
evalBinary op name (mem, (Right x)) (_, (Left y)) = (mem, Left ((show x) ++ name ++ y))
evalBinary op name (mem, (Right x)) (_, (Right y)) = (mem, Right (op x y))



evalStmt :: (Integral a) => Statement -> Memory -> Eval a

evalStmt (Assignment var value) mem = (wm, eval)
	where
		wm 	= Map.insert var (ExprRegister value) mem
		eval 	= Left ("Assigned " ++ (show value) ++ " to " ++ var)

evalStmt (Evaluate expr) mem = evalExpr expr mem

evalStmt (Define def args (Program func)) mem = (wm, eval)
	where
		wm 	= Map.insert def (StmtRegister args (Program (reverse func))) mem
		eval 	= Left ("Defined " ++ def)

evalStmt (Evaluate (Atomic x)) mem = do
	let search = Map.lookup x mem
	case search of
		Nothing -> (mem, Left x)
		Just ex -> evalRegister ex mem
			where
				evalRegister :: (Integral a) => Register -> Memory -> Eval a
				evalRegister (ExprRegister expr) m = evalExpr expr m
				--evalRegister (StmtRegister args func) m = (m, Left (x ++ "()"))

evalStmt (If (Binary Equal l r) ts es) m = 
	if (evalExpr l m) == (evalExpr r m) 
		then evalStmt ts m 
		else evalStmt es m

evalStmt (Loop (Binary Equal l r) loop) m = 
	if (evalExpr l m) /= (evalExpr r m) 
		then evalStmt (Loop (Binary Equal l r) loop) m 
		else (m, Left "end loop")

evalStmt (Evaluate (FunctionCall def vals)) mem = evalExpr (FunctionCall def vals) mem

-- very test-mode here				
evalStmt (Program (x:xs)) mem = evalStmt x m
	where
		(m,_) = evalStmt (Program xs) mem
evalStmt (Program [ x ] ) mem = evalStmt x mem
evalStmt (Program []) mem = (mem, Left "endy")


runProgram :: String -> IO ()
runProgram x = do
	let prog = parseProgram x
	case prog of
		Left err -> print err
		Right (Program program) -> runProg program Map.empty
			where
				runProg :: [ Statement ] -> Memory -> IO ()
				runProg [] _ = return ()
				runProg (x:xs) m = do
					let (mem, eval) = evalStmt x m
					print eval
					runProg xs mem


factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * (factorial (x - 1))

main = runInterpreter Map.empty

runInterpreter :: Memory -> IO ()
runInterpreter mem = do
	statement <- getLine
	let prog = parseProgram statement
	case prog of
		Left err -> putStrLn ("> " ++ (show err))
		Right (Program program) -> runProg program mem
			where
				runProg :: [ Statement ] -> Memory -> IO ()
				runProg [] m = runInterpreter m
				runProg (x:xs) m = do
					let (mem, eval) = evalStmt x m
					case eval of
						Left str -> putStrLn ("> " ++ show str)
						Right val -> putStrLn ("> " ++ (show val))
					runProg xs mem







