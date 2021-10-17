{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char
    ( isSpace, isDigit, isAlpha, isAlphaNum, isLower, isUpper )
import Data.Map
import Data.List
import Text.Read
import Control.Applicative

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a, String)]
parse (P f) = f

item :: Parser Char
item = P (\case
        [] -> []
        (x : xs) -> [(x, xs)])

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (const [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)


ends :: Char -> Char -> Bool
ends l c = l /= c

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else Control.Applicative.empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

str :: Parser String
str = do
    many (sat (ends '\"'))

comment :: Parser String
comment = do
    many (sat (ends '#'))

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

data Block
    = Brac Part
    deriving Show

data Part
    = PA Statement Part
    | Conditional IfStatement Part
    | Loop WhileLoop Part
    | Ignore String Part
    | End
    deriving Show

data IfStatement
    = IfElseStmt LogicExpr Block Block
    deriving Show

data WhileLoop
    = While LogicExpr Block
    deriving Show

data Statement
    = ArithStmt String Expr
    | BoolStmt String LogicExpr
    | StrStmt String StrExpr
    | PrintNum Expr
    | PrintStr StrExpr
    | PrintBool LogicExpr
    deriving Show

data StrExpr
    = Add Sentences StrExpr
    | E4 Sentences
    deriving Show

data Sentences
    = Str String
    | VarStr String
    deriving Show

data LogicExpr
    = And BoolExpr LogicExpr
    | Or BoolExpr LogicExpr
    | E5 BoolExpr
    deriving Show

data BoolExpr
    = T
    | F
    | ArithBool ArithBoolExpr
    | BracBool LogicExpr
    | VarBool String
    deriving Show

data ArithBoolExpr
    = Greater Expr Expr
    | Less Expr Expr
    | Equal Expr Expr
    | NotEq Expr Expr
    deriving Show

data Expr
    = Plus HiExpr Expr
    | Minus HiExpr Expr
    | E1 HiExpr
    deriving Show

data HiExpr
    = Mul SignExpr HiExpr
    | Div SignExpr HiExpr
    | Mod SignExpr HiExpr
    | E2 SignExpr
    deriving Show

data SignExpr
    = Digit Int
    | BracExpr Expr
    | VarNum String
    deriving Show

block :: Parser Block
block
    = do
    symbol "{"
    x1 <- part
    symbol "}"
    return (Brac x1)

part :: Parser Part
part
    = do
    x1 <- statement
    symbol ";"
    PA x1 <$> part
    <|> do
    symbol "while"
    x1 <- whileLoop
    Loop x1 <$> part
    <|> do
    x1 <- ifStatement
    Conditional x1 <$> part
    <|> do
    char '#'
    x1 <- comment
    symbol "#"
    Ignore x1 <$> part
    <|> do
    return End

ifStatement :: Parser IfStatement
ifStatement
    = do
    symbol "if"
    symbol "("
    x1 <- logicExpr
    symbol ")"
    symbol "then"
    x2 <- block
    symbol "else"
    IfElseStmt x1 x2 <$> block

whileLoop :: Parser WhileLoop
whileLoop
    = do
    symbol "("
    x1 <- logicExpr
    symbol ")"
    While x1 <$> block

statement :: Parser Statement
statement
    = do
    symbol "int"
    x1 <- identifier
    symbol "="
    ArithStmt x1 <$> expr
    <|> do
    symbol "bool"
    x1 <- identifier
    symbol "="
    BoolStmt x1 <$> logicExpr
    <|> do
    symbol "string"
    x1 <- identifier
    symbol "="
    StrStmt x1 <$> strExpr
    <|> do
    symbol "print"
    symbol "("
    symbol "%i"
    symbol ","
    x1 <- expr
    symbol ")"
    return (PrintNum x1)
    <|> do
    symbol "print"
    symbol "("
    symbol "%b"
    symbol ","
    x1 <- logicExpr
    symbol ")"
    return (PrintBool x1)
    <|> do
    symbol "print"
    symbol "("
    symbol "%c"
    symbol ","
    x1 <- strExpr
    symbol ")"
    return (PrintStr x1)

strExpr :: Parser StrExpr
strExpr
    = do
    x1 <- sentences
    symbol "+"
    Add x1 <$> strExpr
    <|> E4 <$> sentences

sentences :: Parser Sentences
sentences
    = do
    char '\"'
    x1 <- str
    symbol "\""
    return (Str x1)
    <|> do
    VarStr <$> identifier

logicExpr :: Parser LogicExpr
logicExpr
    = do
    x1 <- boolExpr
    symbol "&&"
    And x1 <$> logicExpr
    <|> do
    x1 <- boolExpr
    symbol "||"
    Or x1 <$> logicExpr
    <|> do E5 <$> boolExpr

boolExpr :: Parser BoolExpr
boolExpr
    = do
    symbol "True"
    return T
    <|> do
    symbol "False"
    return F
    <|> do
    symbol "("
    x1 <- logicExpr
    symbol ")"
    return (BracBool x1)
    <|> do
    ArithBool <$> arithBoolExpr
    <|> do
    VarBool <$> identifier

arithBoolExpr :: Parser ArithBoolExpr
arithBoolExpr
    = do
    x1 <- expr
    symbol ">"
    Greater x1 <$> expr
    <|> do
    x1 <- expr
    symbol "<"
    Less x1 <$> expr
    <|> do
    x1 <- expr
    symbol "=="
    Equal x1 <$> expr
    <|> do
    x1 <- expr
    symbol "!="
    NotEq x1 <$> expr

expr :: Parser Expr
expr
    = do
    x1 <- hiExpr
    symbol "+"
    Plus x1 <$> expr
    <|> do
    x1 <- hiExpr
    symbol "-"
    Minus x1 <$> expr
    <|> do
    E1 <$> hiExpr

hiExpr :: Parser HiExpr
hiExpr
    = do
    x1 <- signExpr
    symbol "*"
    Mul x1 <$> hiExpr
    <|> do
    x1 <- signExpr
    symbol "/"
    Div x1 <$> hiExpr
    <|> do
    x1 <- signExpr
    symbol "%"
    Mod x1 <$> hiExpr
    <|> do
    E2 <$> signExpr

signExpr :: Parser SignExpr
signExpr
    = do
    symbol "("
    x1 <- expr
    symbol ")"
    return (BracExpr x1)
    <|> do Digit <$> integer
    <|> do VarNum <$> identifier

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
evalLogicExpr (And be le) env = evalBoolExpr be env && evalLogicExpr le env
evalLogicExpr (Or be le) env = evalBoolExpr be env || evalLogicExpr le env
evalLogicExpr (E5 be) env = evalBoolExpr be env

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
    let c1 = evalExpr a1 env
    let c2 = evalExpr a2 env
    c1 > c2
evalArithBoolExpr (Less a1 a2) env
    = do
    let c1 = evalExpr a1 env
    let c2 = evalExpr a2 env
    c1 < c2
evalArithBoolExpr (Equal a1 a2) env
    = do
    let c1 = evalExpr a1 env
    let c2 = evalExpr a2 env
    c1 == c2
evalArithBoolExpr (NotEq a1 a2) env
    = do
    let c1 = evalExpr a1 env
    let c2 = evalExpr a2 env
    c1 /= c2

evalStatement :: Statement -> Environment -> Environment
evalStatement (ArithStmt var _arithExpr) env
    = do
    let output = evalExpr _arithExpr env
    Data.Map.insert var [show output] env
evalStatement (BoolStmt var _boolExpr) env
    = do
    let output = evalLogicExpr _boolExpr env
    Data.Map.insert var [show output] env
evalStatement (StrStmt var _strExpr) env
    = do
    let output = evalStrExpr _strExpr env
    Data.Map.insert var [output] env
evalStatement (PrintNum _arithExpr) env
    = do
    let output = evalExpr _arithExpr env
    if member "print" env
        then do
            let prev = env ! "print"
            Data.Map.insert "print" (prev ++ [show output]) env
        else Data.Map.insert "print" [show output] env
evalStatement (PrintStr _strExpr) env
    = do
    let output = evalStrExpr _strExpr env
    if member "print" env
        then do
            let prev = env ! "print"
            Data.Map.insert "print" (prev ++ [show output]) env
        else Data.Map.insert "print" [show output] env
evalStatement (PrintBool _logicExpr) env
    = do
    let output = evalLogicExpr _logicExpr env
    if member "print" env
        then do
            let prev = env ! "print"
            Data.Map.insert "print" (prev ++ [show output]) env
        else Data.Map.insert "print" [show output] env

evalStrExpr :: StrExpr -> Environment -> String
evalStrExpr (Add sent str) env = evalSentences sent env ++ evalStrExpr str env
evalStrExpr (E4 sent) env = evalSentences sent env

evalSentences :: Sentences -> Environment -> String
evalSentences (Str ph) env = ph
evalSentences (VarStr s) env
    = do
    let varStr = head (env ! s)
    varStr

evalExpr :: Expr -> Environment -> Int
evalExpr (E1 e1) env = evalHiExpr e1 env
evalExpr (Plus f1 e2) env = evalHiExpr f1 env + evalExpr e2 env
evalExpr (Minus f1 e2) env = evalHiExpr f1 env - evalExpr e2 env

evalHiExpr :: HiExpr -> Environment -> Int
evalHiExpr (E2 h1) env = evalSignExpr h1 env
evalHiExpr (Mul n1 h2) env = evalSignExpr n1 env * evalHiExpr h2 env
evalHiExpr (Div n1 h2) env = evalSignExpr n1 env `div` evalHiExpr h2 env
evalHiExpr (Mod n1 h2) env = evalSignExpr n1 env `mod` evalHiExpr h2 env

evalSignExpr :: SignExpr -> Environment -> Int
evalSignExpr (Digit n) env = n
evalSignExpr (BracExpr _bracExpr) env = evalExpr _bracExpr env
evalSignExpr (VarNum s) env
    = do
    let numStr = head (env ! s)
    let num = read numStr :: Int
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
    let a = parse block contents
    let b = fst . head $ a
    let h = evalBlock b Data.Map.empty
    showEnv . assocs $ h


