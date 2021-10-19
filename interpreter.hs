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
    = Stmt String AllExpr
    | Print AllExpr
    deriving Show

data AllExpr
    = Add Sentences AllExpr
    | E4 Sentences
    deriving Show

data Sentences
    = Str String
    | B LogicExpr
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
    deriving Show

data ArithBoolExpr
    = Greater Expr Expr
    | Less Expr Expr
    | Equal Expr Expr
    | NotEq Expr Expr
    | A Expr
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
    | BracExpr AllExpr
    | Var String
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
    x1 <- identifier
    symbol "="
    Stmt x1 <$> allExpr
    <|> do
    symbol "print"
    symbol "("
    x1 <- allExpr
    symbol ")"
    return (Print x1)

allExpr :: Parser AllExpr
allExpr
    = do
    x1 <- sentences
    symbol "++"
    Add x1 <$> allExpr
    <|> E4 <$> sentences

sentences :: Parser Sentences
sentences
    = do
    char '\"'
    x1 <- str
    symbol "\""
    return (Str x1)
    <|> do
    B <$> logicExpr

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
    ArithBool <$> arithBoolExpr

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
    <|> do
    A <$> expr

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
    x1 <- allExpr
    symbol ")"
    return (BracExpr x1)
    <|> do Digit <$> integer
    <|> do Var <$> identifier

-- Stores variable values
type Environment = Map (String, Int) [String]

-- Functions below take the output of the parser and evaluates it
evalBlock :: Block -> Int -> Environment -> Environment
evalBlock (Brac _part)
    = evalPart _part

evalPart :: Part -> Int -> Environment -> Environment
evalPart (Ignore _comm _part) i env
    = evalPart _part i env
evalPart (PA _stmt _part) i env
    = do
    let env' = evalStatement _stmt i env
    evalPart _part i env'
evalPart (Conditional _cond _part) i env
    = do
    let env' = evalIfStatement _cond i env
    evalPart _part i env'
evalPart (Loop _loop _part) i env
    = do
    let env' = evalWhileLoop _loop i env
    evalPart _part i env'
evalPart End i env
    = env

evalWhileLoop :: WhileLoop -> Int -> Environment -> Environment
evalWhileLoop (While _cond _body) i env
    = do
    let cond = evalLogicExpr _cond i env
    if read cond :: Bool
        then do
            let env' = evalBlock _body i env
            evalWhileLoop (While _cond _body) i env'
        else env

evalIfStatement :: IfStatement -> Int-> Environment -> Environment
evalIfStatement (IfElseStmt _boolExpr _condTrue _condFalse) i env
    = do
    let cond = evalLogicExpr _boolExpr i env
    let env' = if read cond :: Bool then evalBlock _condTrue i env else evalBlock _condFalse i env
    env'

evalLogicExpr :: LogicExpr -> Int -> Environment -> String
evalLogicExpr (And be le) i env = show((read (evalBoolExpr be i env) :: Bool) && read(evalLogicExpr le i env) :: Bool)
evalLogicExpr (Or be le) i env = show((read (evalBoolExpr be i env) :: Bool) || read(evalLogicExpr le i env) :: Bool)
evalLogicExpr (E5 be) i env = evalBoolExpr be i env

evalBoolExpr :: BoolExpr -> Int -> Environment -> String
evalBoolExpr T i env = show True
evalBoolExpr F i env = show False
evalBoolExpr (ArithBool abe) i env
    = evalArithBoolExpr abe i env

evalArithBoolExpr :: ArithBoolExpr -> Int -> Environment -> String
evalArithBoolExpr (Greater a1 a2) i env
    = do
    let c1 = evalExpr a1 i env
    let c2 = evalExpr a2 i env
    show ((read c1 :: Int) > (read c2 :: Int))
evalArithBoolExpr (Less a1 a2) i env
    = do
    let c1 = evalExpr a1 i env
    let c2 = evalExpr a2 i env
    show ((read c1 :: Int) < (read c2 :: Int))
evalArithBoolExpr (Equal a1 a2) i env
    = do
    let c1 = evalExpr a1 i env
    let c2 = evalExpr a2 i env
    show ((read c1 :: Int) == (read c2 :: Int))
evalArithBoolExpr (NotEq a1 a2) i env
    = do
    let c1 = evalExpr a1 i env
    let c2 = evalExpr a2 i env
    show ((read c1 :: Int) /= (read c2 :: Int))
evalArithBoolExpr (A a) i env = evalExpr a i env

evalStatement :: Statement -> Int -> Environment -> Environment
evalStatement (Stmt var _allExpr) i env
    = do
    let output = evalAllExpr _allExpr i env
    Data.Map.insert (var, i) [output] env
evalStatement (Print _allExpr) i env
    = do
    let output = evalAllExpr _allExpr i env
    if member ("print", 0) env
        then do
            let prev = env ! ("print", 0)
            Data.Map.insert ("print", 0) (prev ++ [output]) env
        else Data.Map.insert ("print", 0) [output] env

evalAllExpr :: AllExpr -> Int -> Environment -> String
evalAllExpr (Add sent str) i env = evalSentences sent i env ++ evalAllExpr str i env
evalAllExpr (E4 sent) i env = evalSentences sent i env

evalSentences :: Sentences -> Int -> Environment -> String
evalSentences (Str ph) i env = ph
evalSentences (B b) i env = evalLogicExpr b i env

evalExpr :: Expr -> Int -> Environment -> String
evalExpr (E1 e1) i env = evalHiExpr e1 i env
evalExpr (Plus f1 e2) i env = show ((read (evalHiExpr f1 i env) :: Int) + read (evalExpr e2 i env) :: Int)
evalExpr (Minus f1 e2) i env = show ((read (evalHiExpr f1 i env) :: Int) - read(evalExpr e2 i env) :: Int)

evalHiExpr :: HiExpr -> Int -> Environment -> String
evalHiExpr (E2 h1) i env = evalSignExpr h1 i env
evalHiExpr (Mul n1 h2) i env = show ((read (evalSignExpr n1 i env) :: Int) * read (evalHiExpr h2 i env) :: Int)
evalHiExpr (Div n1 h2) i env = show ((read (evalSignExpr n1 i env) :: Int) `div` read (evalHiExpr h2 i env) :: Int)
evalHiExpr (Mod n1 h2) i env =  show ((read (evalSignExpr n1 i env) :: Int) `mod` read (evalHiExpr h2 i env) :: Int)

evalSignExpr :: SignExpr -> Int -> Environment -> String
evalSignExpr (Digit n) i env = show n
evalSignExpr (BracExpr _bracExpr) i env = evalAllExpr _bracExpr i env
evalSignExpr (Var s) i env
    = do
    let numStr = head (env ! (s, i))
    numStr

showEnv :: [((String, Int), [String])] -> IO ()
showEnv [] = putStr ""
showEnv ((str, int):rem)
    = do
    if str /= ("print", 0)
        then putStrLn (fst str ++ " " ++ head int)
        else putStrLn (fst str ++ " " ++ listToString int "")
    showEnv rem

listToString :: [String] -> String -> String
listToString [] str = str ++ ""
listToString (xs:x) str = listToString x (str ++ xs ++ " ")

main :: IO()
main = do
    contents <- readFile "prog.txt"
    let a = parse block contents
    let b = fst . head $ a
    let h = evalBlock b 0 Data.Map.empty
    showEnv . assocs $ h