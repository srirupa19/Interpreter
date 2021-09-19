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
    | BoolStmt String LogicExpr
    | StrStmt String StrExpr
    | PrintNum ArithExpr
    | PrintStr StrExpr
    | PrintBool LogicExpr
    deriving Show

data StrExpr
    = Add Sentences StrExpr
    | E4
    deriving Show

data Sentences
    = P Phrases
    | VarStr String
    deriving Show

data Phrases
    = Str String Phrases
    | E3
    deriving Show

data IfStatement
    = IfElseStmt LogicExpr Block Block
    deriving Show

data WhileLoop
    = While LogicExpr Block
    deriving Show

data LogicExpr 
    = LE BoolExpr LogicBool
    deriving Show

data LogicBool 
    = And BoolExpr LogicBool
    | Or BoolExpr LogicBool
    | E5
    deriving Show

data BoolExpr
    = T
    | F
    | ArithBool ArithBoolExpr
    | BracBool LogicExpr
    | VarBool String
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
    = FactorExpr SignExpr HiExpr
    deriving Show

data HiExpr
    = Mul SignExpr HiExpr
    | Div SignExpr HiExpr
    | Mod SignExpr HiExpr
    | E2
    deriving Show

data SignExpr
    = Neg Numbers
    | Pos Numbers
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
part ("int":_part)
    = do
    let (_stmt, rem) = statement ("int":_part)
    let (_part, rem') = part rem
    (PA _stmt _part, rem')
part ("bool":_part)
    = do
    let (_stmt, rem) = statement ("bool":_part)
    let (_part, rem') = part rem
    (PA _stmt _part, rem')
part ("string":_part)
    = do
    let (_stmt, rem) = statement ("string":_part)
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
    let (_cond, ")":rem) = logicExpr _rest
    let (_body, rem') = block rem
    (While _cond _body, rem')

statement :: [String] -> (Statement, [String])
statement ("print":"%i":_stmt)
    = do
    let (_arithExpr, ";":rem) = arithExpr _stmt
    (PrintNum _arithExpr, rem)
statement ("print":"%s":_stmt)
    = do
    let (_strExpr, rem) = strExpr _stmt
    (PrintStr _strExpr, rem)
statement ("print":"%b":_stmt)
    = do
    let (_logicExpr, ";":rem) = logicExpr _stmt
    (PrintBool _logicExpr, rem)
statement ("int":var:"=":_stmt)
    = do
    let (_arithExpr, ";":rem) = arithExpr _stmt
    (ArithStmt var _arithExpr, rem)
statement ("bool":var:"=":_stmt)
    = do
    let (_boolExpr, ";":rem) = logicExpr _stmt
    (BoolStmt var _boolExpr, rem)
statement ("string":var:"=":_stmt)
    = do
    let (_strExpr, rem) = strExpr _stmt
    (StrStmt var _strExpr, rem)

strExpr :: [String] -> (StrExpr, [String])
strExpr rem
    = do
    let (_sentence, s:rem') = sentences rem
    if s == "+"
        then do
        let (_str, rem'') = strExpr rem'
        (Add _sentence _str, rem'')
        else (Add _sentence E4, rem')

sentences :: [String] -> (Sentences, [String])
sentences ("'":rem)
    = do
    let (_phrase, rem') = phrases rem
    (P _phrase, rem')
sentences (s:rem) = (VarStr s, rem)


phrases :: [String] -> (Phrases, [String])
phrases ("'":rest) = (E3, rest)
phrases (word:rest)
    = do
    let (words, rest') = phrases rest
    (Str word words, rest')

ifStatement :: [String] -> (IfStatement, [String])
ifStatement _rest
    = do
    let (_boolexp, ")":"then":rem) = logicExpr _rest
    let (_condTrue, "else":rem') = block rem
    let (_condFalse, rem'') = block rem'
    (IfElseStmt _boolexp _condTrue _condFalse, rem'')

logicExpr :: [String] -> (LogicExpr, [String])
logicExpr _logic 
    = do 
    let (_boolExpr, rem) = boolExpr _logic 
    let (_logicBool, rem') = logicBool rem 
    (LE _boolExpr _logicBool, rem')

logicBool :: [String] -> (LogicBool, [String])
logicBool ("&":rem)
    = do 
    let (_boolExpr, rem') = boolExpr rem 
    let (_logicBool, rem'') = logicBool rem' 
    (And _boolExpr _logicBool, rem'')
logicBool ("|":rem)
    = do 
    let (_boolExpr, rem') = boolExpr rem 
    let (_logicBool, rem'') = logicBool rem' 
    (Or _boolExpr _logicBool, rem'')
logicBool rem = (E5, rem)

boolExpr :: [String] -> (BoolExpr, [String])
boolExpr ("True":rem) = (T, rem)
boolExpr ("False":rem) = (F, rem)
boolExpr ("(":rem) 
    = do 
    let (_le, ")":rem') = logicExpr rem 
    (BracBool _le, rem')
boolExpr (s:a:rem) 
    = do
    if s == "bool::" 
        then (VarBool a, rem) 
        else do
        let (_arithBool, rem') =  arithBoolExpr (s:a:rem)
        (ArithBool _arithBool, rem')

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
    let (_numbers, rem) = signExpr _factorExpr
    let (_hiExpr, rem') = hiExpr rem
    (FactorExpr  _numbers _hiExpr, rem')

hiExpr :: [String] -> (HiExpr, [String])
hiExpr ("*":_hiExpr)
    = do
    let (_numbers, rem) = signExpr _hiExpr
    let (_hiExpr, rem') = hiExpr rem
    (Mul _numbers _hiExpr, rem')
hiExpr ("/":_hiExpr)
    = do
    let (_numbers, rem) = signExpr _hiExpr
    let (_hiExpr, rem') = hiExpr rem
    (Div _numbers _hiExpr, rem')
hiExpr ("%":_hiExpr)
    = do
    let (_numbers, rem) = signExpr _hiExpr
    let (_hiExpr, rem') = hiExpr rem
    (Mod _numbers _hiExpr, rem')
hiExpr _hiExpr = (E2 , _hiExpr)

signExpr :: [String] -> (SignExpr, [String])
signExpr ("-":rem)
    = do
    let (_numbers, rem') = numbers rem
    (Neg _numbers, rem')
signExpr rem
    = do
    let (_numbers, rem') = numbers rem
    (Pos _numbers, rem')

numbers :: [String] -> (Numbers, [String])
numbers ("(":_bracExpr) = do
  let (_expr, ")":_rem) = arithExpr _bracExpr
  (BracExpr _expr, _rem)
numbers (start:rem)
  | isNothing isNum = (VarNum start, rem)
  | otherwise = (Digit (read start :: Integer), rem)
  where isNum = readMaybe start :: Maybe Integer

-- Stores variable values
type Environment = Map String [String]

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
    let cond = evalLogicExpr _cond env
    if cond
        then do
            let env' = evalBlock _body env
            evalWhileLoop (While _cond _body) env'
        else env

evalIfStatement :: IfStatement -> Environment -> Environment
evalIfStatement (IfElseStmt _boolExpr _condTrue _condFalse) env
    = do
    let cond = evalLogicExpr _boolExpr env
    let env' = if cond then evalBlock _condTrue env else evalBlock _condFalse env
    env'

evalLogicExpr :: LogicExpr -> Environment -> Bool 
evalLogicExpr (LE be lb) env = evalLogicBool (evalBoolExpr be env) lb env

evalLogicBool :: Bool -> LogicBool ->Environment -> Bool 
evalLogicBool b (And be lb) env = evalLogicBool (b && evalBoolExpr be env) lb env
evalLogicBool b (Or be lb) env = evalLogicBool (b || evalBoolExpr be env) lb env
evalLogicBool b E5 env = b 

evalBoolExpr :: BoolExpr -> Environment -> Bool
evalBoolExpr T env = True
evalBoolExpr F env = False
evalBoolExpr (ArithBool abe) env
    = evalArithBoolExpr abe env
evalBoolExpr (BracBool le) env 
    = evalLogicExpr le env
evalBoolExpr (VarBool b) env 
    = do
    let boolStr = head (env ! b)
    let boo = read boolStr :: Bool
    boo 

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
    insert var [show output] env
evalStatement (BoolStmt var _boolExpr) env
    = do
    let output = evalLogicExpr _boolExpr env
    insert var [show output] env
evalStatement (StrStmt var _strExpr) env
    = do
    let output = evalStrExpr _strExpr env
    insert var [output] env
evalStatement (PrintNum _arithExpr) env
    = do
    let output = evalArithExpr _arithExpr env
    if member "print" env
        then do
            let prev = env ! "print"
            insert "print" (prev ++ [show output]) env
        else insert "print" [show output] env
evalStatement (PrintStr _strExpr) env
    = do
    let output = evalStrExpr _strExpr env
    if member "print" env
        then do
            let prev = env ! "print"
            insert "print" (prev ++ [show output]) env
        else insert "print" [show output] env
evalStatement (PrintBool _logicExpr) env
    = do
    let output = evalLogicExpr _logicExpr env
    if member "print" env
        then do
            let prev = env ! "print"
            insert "print" (prev ++ [show output]) env
        else insert "print" [show output] env

evalStrExpr :: StrExpr -> Environment -> String
evalStrExpr (Add sent str) env = evalSentences sent env ++ evalStrExpr str env
evalStrExpr E4 env = ""

evalSentences :: Sentences -> Environment -> String
evalSentences (P ph) env = evalPhrases ph env
evalSentences (VarStr s) env
    = do
    let varStr = head (env ! s)
    varStr

evalPhrases :: Phrases -> Environment -> String
evalPhrases (Str word str) env = word ++ " " ++ evalPhrases str env
evalPhrases E3 env = ""

evalArithExpr :: ArithExpr -> Environment -> Integer
evalArithExpr (ArithExpr f1 e2) env = evalExpr (evalFactorExpr f1 env) e2 env

evalExpr :: Integer -> Expr -> Environment -> Integer
evalExpr num E1 env = num
evalExpr num (Plus f1 e2) env = evalExpr (num + evalFactorExpr f1 env) e2 env
evalExpr num (Minus f1 e2) env = evalExpr (num - evalFactorExpr f1 env) e2 env

evalFactorExpr :: FactorExpr -> Environment -> Integer
evalFactorExpr (FactorExpr n1 h2) env = evalHiExpr (evalSignExpr n1 env) h2 env

evalHiExpr :: Integer -> HiExpr -> Environment -> Integer
evalHiExpr num E2 env = num
evalHiExpr num (Mul n1 h2) env = evalHiExpr (num * evalSignExpr n1 env) h2 env
evalHiExpr num (Div n1 h2) env = evalHiExpr (num `div` evalSignExpr n1 env) h2 env
evalHiExpr num (Mod n1 h2) env = evalHiExpr (num `mod` evalSignExpr n1 env) h2 env

evalSignExpr :: SignExpr -> Environment -> Integer
evalSignExpr (Pos num) env = evalNumbers num env
evalSignExpr (Neg num) env = evalNumbers num env * toInteger (-1)

evalNumbers :: Numbers -> Environment -> Integer
evalNumbers (Digit n) env = n
evalNumbers (BracExpr _bracExpr) env = evalArithExpr _bracExpr env
evalNumbers (VarNum s) env
    = do
    let numStr = head (env ! s)
    let num = read numStr :: Integer
    num

showEnv :: [(String, [String])] -> IO ()
showEnv [] = putStr ""
showEnv ((str, int):rem)
    = do
    if str /= "print"
        then putStrLn (str ++ " " ++ head int)
        else putStrLn (str ++ " " ++ listToString int "")
    showEnv rem

listToString :: [String] -> String -> String
listToString [] str = str ++ ""
listToString (xs:x) str = listToString x (str ++ xs ++ " ")

main :: IO()
main = do
    contents <- readFile "prog.txt"
    let g = fst . block . words $ contents
    let h = evalBlock g empty
    showEnv . assocs $ h
