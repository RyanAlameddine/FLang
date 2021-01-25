# FLang

FLang is a low(ish)-level functional programming language designed (by me) and inspired by the book "Implementing Functional Languages" by Simon Peyton Jones and David R Lester. 

## Parser



### 1. Tokenization

```haskell
type Token = (LineNumber, String)
type LineNumber = Int

tokens :: String -> [Token] 
```

The tokens function performs a lexical analysis on the input string and returns a list of tokens based upon their conformity to the following patterns:
```haskell
isNewl    = (=='\n')                               --if a newline character
isSpace   = isSpace                                --if a whitespace char
isCChar   = (=='#')                                --if the comment character
isParen   = matchAny [(=='('), (==')')]            --if a parenthesis character
isDigit   = isDigit                                --if a digit
isAlpha   = isAlpha                                --if an letter of the alphabet
isIdChar  = matchAny [isAlpha, isNumber, (=='_')]  --if part of an identifier (alpha, number, or underscore)
isOper x  = not (isParen x) && isSymbol x          --if an operator token (symbol not parenthesis) 
```

### 2. Parsing

The Monad Parser is defined as follows:

```haskell
--Returns a list of (output, remaining tokens) which represents
--the list of all possible parses (ambiguities to be collapsed)
newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])] }

instance Functor Parser where
    fmap f (Parser p) = Parser (fmap transform p)
        where
            transform = map $ first f

instance Applicative Parser where
    pure val = Parser $ \input -> [(val, input)]
    (Parser f) <*> (Parser a) = Parser $ \input -> do
                                            (func, input' ) <- f input 
                                            (val , input'') <- a input'
                                            return (func val, input'')

instance Monad Parser where
    (Parser a) >>= f = Parser $ \input -> do
                            (val , input') <- a input
                            runParser (f val) input'

instance Alternative Parser where
    empty = Parser $ const []
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input
```

The base parser pSat (which parses for a satisfied condition) is defined and all other parsers are defined as combinations of pSat using the Monadic functions.

```haskell
pSat :: (String -> Bool) -> Parser String
pSat f = Parser satisfy
    where
        satisfy ((_, t):ts) | f t = [(t, ts)]
        satisfy _ = []
```

A parser combinator library is built off of this satisfy function. Here are some sample parsers:

```haskell

--takes in a string and returns a parser which parses that literal
pLit :: String -> Parser String
pLit = pSat . (==)

--parses a variable identifier
pVar :: Parser String
pVar = pSat check
    where
        --starts with alphanumeric and is not a keyword
        check token@(c:_) = isAlpha c && token `notElem` keywords

--parses a number
pNum :: Parser Int
pNum = read <$> pSat check
    where 
        check = isJust . (readMaybe :: String -> Maybe Int)

--zero or more instances of a parser where all values are combined into a list
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = return [] <|> pOneOrMore p

--one or more instances of a parser where all values are combined into a list
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = do
            val <- p
            others <- pZeroOrMore p
            return $ val : others
```            

<img src="/BNFSyntax.png" width=500> 

To solve the issues with left recursion (and implement operator precedence), this grammar is updated to the following:

<img src="/ExprSyntax.png" width=300> 

After these basic parsers are defined, all other parsers are just simple transliteration of the language syntax (using simple function composition):

```haskell
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreSep pScDefn (pLit ";")

pSc :: Parser CoreScDefn
pSc = pair3 pVar (pZeroOrMore pVar) (pLit "=" *> pExpr)

pExpr1 = pExprLayer pExpr2 [("|", pExpr1)]
pExpr2 = pExprLayer pExpr3 [("&", pExpr2)]
pExpr3 = pExprLayer pExpr4 $ map (,pExpr4) relOps
pExpr4 = pExprLayer pExpr5 [("+", pExpr4), ("-", pExpr5)]
pExpr5 = pExprLayer pExpr6 [("*", pExpr5), ("/", pExpr6)]

pDefns :: Parser [CoreDefn]
pDefns = pOneOrMoreSep pDefn (pLit ";")

pDefn :: Parser CoreDefn
pDefn = pair2 pVar (pLit "=" *> pExpr)

pAlts :: Parser [CoreAlt]
pAlts = pOneOrMoreSep pAlt (pLit ";")

pAlt :: Parser CoreAlt
pAlt = pair3 cnum cvars pExpr
    where
        cnum  = pLit "<" *> pNum <* pLit ">"
        cvars = pZeroOrMore pVar <* pLit "->"
```

## Internal Representation

After being parsed, the program will be represented

```haskell
type Program a = [ScDefn a]
type CoreProgram = Program Name

--name of supercombinator, arguments, and body
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

data Expr a = 
    EVar Name               -- Variable
    | ENum Int              -- Number
    | EConstr Int Int       -- Constructor: tag, arity
    | EAp (Expr a) (Expr a) -- Application
    | ELet                  -- Let(rec) expression
        IsRec               --   True=recursive
        [Defn]              --   Definitions
        (Expr a)            --   Expression Body
    | ECase                 -- Case expression
        (Expr a)            --   Expression being checked
        [Alter a]           --   Alternatives
    | Elam [a] (Expr a)     -- Lambda
    deriving Show

type CoreExpr = Expr Name 

type Name = String

type IsRec = Bool

--(variable name being bound, expression to which it is bound)
type Defn a = (a, Expr a)
type CoreDefn = Defn Name

--case expressions: expression to analyze and list of alternatives
--each alternative has tag, bound variables, and expression to the right of the arrow
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

```
    
## FLang Prelude

The Prelude (standard library) of FLang consists of the following functions:
```haskell
id x   = x ;
K  x y = x ;
K1 x y = y ;
S f g x       = (f x) (g x) ;
compose f g x = f (g x)     ;
twice f       = compose f f ;
```

## Pretty Printer

This project also includes a pretty printer which can transform the `CoreProgram` data into a formatted string which represents the program.

```haskell
data Seq = SNil
        | SStr String
        | SAppend Seq Seq
        | SIndent Seq
        | SNewline

prnt :: CoreProgram -> String
```

To combat the inefficiency of the (++) operator (which would otherwise be needed often), the prnt function generates the string using Seq, a binary tree with values only in the leaf nodes (but also holds additional formatting information). These Seq trees are eventually `flatten`ed into a regular string once at the end of pretty printing.
