{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Map
import Text.Read
import Data.Maybe

data Block 
    = Brac Part
    deriving Show 

data Part 
    = PA Statement Part
    | Conditional IfStatement Part
    | Loop WhileLoop Part
    | Ignore Comment Part
    | End
    deriving Show 

data Comment 
    = Comm String
    deriving Show

data Statement 
    = ArithStmt String ArithExpr
    | Print ArithExpr
    deriving Show

data IfStatement
    = IfElseStmt BoolExpr Block Block
    deriving Show

data WhileLoop
    = While BoolExpr Block 
    deriving Show

data BoolExpr 
    = T
    | F
    | ArithBool ArithBoolExpr
    deriving Show

data ArithBoolExpr
    = Greater ArithExpr ArithExpr
    | Less ArithExpr ArithExpr
    | Equal ArithExpr ArithExpr
    | NotEq ArithExpr ArithExpr
    deriving Show

data ArithExpr 
    = ArithExpr FactorExpr Expr
    deriving Show 

data Expr 
    = Plus FactorExpr Expr
    | Minus FactorExpr Expr
    | E1 
    deriving Show 

data FactorExpr 
    = FactorExpr Numbers HiExpr
    deriving Show 

data HiExpr 
    = Mul Numbers HiExpr
    | Div Numbers HiExpr
    | Mod Numbers HiExpr
    | E2 
    deriving Show

data Numbers 
    = Digit Integer
    | BracExpr ArithExpr
    | VarNum String
    deriving Show 

-- Functions below takes the input string and parses it. 
-- Scanerless Parser: Lexing + Parsing 
block :: [String] -> (Block, [String])
block ("{":_block)
    = do 
    let (_part, "}":rem) = part _block
    (Brac _part, rem)

part :: [String] -> (Part, [String])
part ("/*":_rest)
    = do 
    let (_comm, "*/":rem) = comment _rest
    let (_part, rem') = part rem 
    (Ignore _comm _part, rem')
part ("if":"(":_rest)
    = do
    let (_cond, rem) = ifStatement _rest
    let (_part, rem') = part rem
    (Conditional _cond _part, rem')
part ("while":"(":_rest)
    = do 
    let (_loop, rem) = whileLoop _rest 
    let (_part, rem') = part rem
    (Loop _loop _part, rem')
part ("print":_part)
    = do
    let (_stmt, rem) = statement ("print":_part)
    let (_part, rem') = part rem
    (PA _stmt _part, rem')
part ("let":_part)
    = do
    let (_stmt, rem) = statement ("let":_part)
    let (_part, rem') = part rem
    (PA _stmt _part, rem')
part _end 
    = (End, _end)

comment :: [String] -> (Comment, [String])
comment (_comm:"*/":rem)
    = (Comm _comm, "*/":rem)

whileLoop :: [String] -> (WhileLoop, [String])
whileLoop _rest 
    = do 
    let (_cond, ")":rem) = boolExpr _rest 
    let (_body, rem') = block rem 
    (While _cond _body, rem')

statement :: [String] -> (Statement, [String])
statement ("print":_stmt)
    = do
    let (_arithExpr, ";":rem) = arithExpr _stmt
    (Print _arithExpr, rem)
statement ("let":var:"=":_stmt)
    = do
    let (_arithExpr, ";":rem) = arithExpr _stmt
    (ArithStmt var _arithExpr, rem)

ifStatement :: [String] -> (IfStatement, [String])
ifStatement _rest
    = do 
    let (_boolexp, ")":"then":rem) = boolExpr _rest 
    let (_condTrue, "else":rem') = block rem
    let (_condFalse, rem'') = block rem' 
    (IfElseStmt _boolexp _condTrue _condFalse, rem'')

boolExpr :: [String] -> (BoolExpr, [String])
boolExpr ("True":rem) = (T, rem)
boolExpr ("False":rem) = (F, rem)
boolExpr other 
    = do
    let (_arithBool, rem) =  arithBoolExpr other 
    (ArithBool _arithBool, rem)

arithBoolExpr :: [String] -> (ArithBoolExpr, [String])
arithBoolExpr abe 
    = do 
    let (_arithExpr, rem) = arithExpr abe 
    let (x:xs) = rem 
    case x of 
        ">" -> do 
                let (_expr, rem) = arithExpr xs
                (Greater _arithExpr _expr, rem)
        "<" -> do 
                let (_expr, rem) = arithExpr xs
                (Less _arithExpr _expr, rem)
        "==" -> do 
                let (_expr, rem) = arithExpr xs
                (Equal _arithExpr _expr, rem)
        "!=" -> do 
                let (_expr, rem) = arithExpr xs
                (NotEq _arithExpr _expr, rem)

arithExpr :: [String] -> (ArithExpr, [String])
arithExpr _arithExpr 
    = do 
    let (_factorExpr, rem) = factorExpr _arithExpr
    let (_expr, rem') = expr rem
    (ArithExpr _factorExpr _expr, rem')

expr :: [String] -> (Expr, [String])
expr ("+":_expr)
    = do
    let (_factorExpr, rem) = factorExpr _expr
    let (_expr, rem') = expr rem
    (Plus _factorExpr _expr, rem')
expr ("-":_expr)
    = do
    let (_factorExpr, rem) = factorExpr _expr
    let (_expr, rem') = expr rem
    (Minus _factorExpr _expr, rem')
expr _expr = (E1, _expr)

factorExpr :: [String] -> (FactorExpr, [String])
factorExpr _factorExpr
    = do 
    let (_numbers, rem) = numbers _factorExpr
    let (_hiExpr, rem') = hiExpr rem
    (FactorExpr  _numbers _hiExpr, rem')

hiExpr :: [String] -> (HiExpr, [String])
hiExpr ("*":_hiExpr)
    = do
    let (_numbers, rem) = numbers _hiExpr
    let (_hiExpr, rem') = hiExpr rem
    (Mul _numbers _hiExpr, rem')
hiExpr ("/":_hiExpr)
    = do
    let (_numbers, rem) = numbers _hiExpr
    let (_hiExpr, rem') = hiExpr rem
    (Div _numbers _hiExpr, rem')
hiExpr ("%":_hiExpr)
    = do
    let (_numbers, rem) = numbers _hiExpr
    let (_hiExpr, rem') = hiExpr rem
    (Mod _numbers _hiExpr, rem')
hiExpr _hiExpr = (E2 , _hiExpr)


numbers :: [String] -> (Numbers, [String])
numbers ("(":_bracExpr) = do
  let (_expr, ")":_rem) = arithExpr _bracExpr
  (BracExpr _expr, _rem)
numbers (start:rem)
  | isNothing isNum = (VarNum start, rem)
  | otherwise = (Digit (read start :: Integer), rem)
  where isNum = readMaybe start :: Maybe Integer

-- Stores variable values
type Environment = Map String [Integer] 

-- Functions below take the output of the parser and evaluates it
evalBlock :: Block -> Environment -> Environment
evalBlock (Brac _part) 
    = evalPart _part

evalPart :: Part -> Environment -> Environment
evalPart (Ignore _comm _part) env 
    = evalPart _part env
evalPart (PA _stmt _part) env 
    = do 
    let env' = evalStatement _stmt env
    evalPart _part env'
evalPart (Conditional _cond _part) env 
    = do 
    let env' = evalIfStatement _cond env 
    evalPart _part env'
evalPart (Loop _loop _part) env 
    = do 
    let env' = evalWhileLoop _loop env 
    evalPart _part env'
evalPart End env 
    = env

evalWhileLoop :: WhileLoop -> Environment -> Environment
evalWhileLoop (While _cond _body) env 
    = do 
    let cond = evalBoolExpr _cond env 
    if cond 
        then do
            let env' = evalBlock _body env 
            evalWhileLoop (While _cond _body) env'
        else env

evalIfStatement :: IfStatement -> Environment -> Environment
evalIfStatement (IfElseStmt _boolExpr _condTrue _condFalse) env 
    = do 
    let cond = evalBoolExpr _boolExpr env   
    let env' = if cond then evalBlock _condTrue env else evalBlock _condFalse env
    env'
            
evalBoolExpr :: BoolExpr -> Environment -> Bool 
evalBoolExpr T env = True 
evalBoolExpr F env = False  
evalBoolExpr (ArithBool abe) env 
    = evalArithBoolExpr abe env 
    
evalArithBoolExpr :: ArithBoolExpr -> Environment -> Bool 
evalArithBoolExpr (Greater a1 a2) env
    = do
    let c1 = evalArithExpr a1 env
    let c2 = evalArithExpr a2 env
    c1 > c2 
evalArithBoolExpr (Less a1 a2) env
    = do
    let c1 = evalArithExpr a1 env
    let c2 = evalArithExpr a2 env
    c1 < c2 
evalArithBoolExpr (Equal a1 a2) env
    = do
    let c1 = evalArithExpr a1 env
    let c2 = evalArithExpr a2 env
    c1 == c2     
evalArithBoolExpr (NotEq a1 a2) env
    = do
    let c1 = evalArithExpr a1 env
    let c2 = evalArithExpr a2 env
    c1 /= c2   

evalStatement :: Statement -> Environment -> Environment
evalStatement (ArithStmt var _arithExpr) env 
    = do
    let output = evalArithExpr _arithExpr env
    insert var [output] env
evalStatement (Print _arithExpr) env 
    = do
    let output = evalArithExpr _arithExpr env
    if member "print" env 
        then do 
            let prev = env ! "print"
            insert "print" (prev ++ [output]) env
        else insert "print" [output] env

evalArithExpr :: ArithExpr -> Environment -> Integer
evalArithExpr (ArithExpr f1 e2) env = evalExpr (evalFactorExpr f1 env) e2 env

evalExpr :: Integer -> Expr -> Environment -> Integer
evalExpr num E1 env = num
evalExpr num (Plus f1 e2) env = evalExpr (num + evalFactorExpr f1 env) e2 env
evalExpr num (Minus f1 e2) env = evalExpr (num - evalFactorExpr f1 env) e2 env

evalFactorExpr :: FactorExpr -> Environment -> Integer
evalFactorExpr (FactorExpr n1 h2) env = evalHiExpr (evalNumbers n1 env) h2 env

evalHiExpr :: Integer -> HiExpr -> Environment -> Integer
evalHiExpr num E2 env = num
evalHiExpr num (Mul n1 h2) env = evalHiExpr (num * evalNumbers n1 env) h2 env
evalHiExpr num (Div n1 h2) env = evalHiExpr (num `div` evalNumbers n1 env) h2 env
evalHiExpr num (Mod n1 h2) env = evalHiExpr (num `mod` evalNumbers n1 env) h2 env

evalNumbers :: Numbers -> Environment -> Integer
evalNumbers (Digit n) env = n
evalNumbers (BracExpr _bracExpr) env = evalArithExpr _bracExpr env
evalNumbers (VarNum s) env = head (env ! s)


showEnv :: [(String, [Integer])] -> IO ()
showEnv [] = putStr ""
showEnv ((str, int):rem) 
    = do
    if str /= "print"
        then putStrLn (str ++ " " ++ show (head int))
        else putStrLn (str ++ " " ++ listToString int "")
    showEnv rem

listToString :: [Integer] -> String -> String 
listToString [] str = str ++ ""
listToString (xs:x) str = listToString x (str ++ show xs ++ " ")

main :: IO()
main = do
    contents <- readFile "prog.txt"
    let g = fst . block . words $ contents
    let h = evalBlock g empty
    showEnv . assocs $ h
