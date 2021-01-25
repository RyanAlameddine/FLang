{-# LANGUAGE TupleSections #-}

module Core where

import Data.Char
import Control.Monad
import Control.Arrow
import Control.Applicative
import Text.Read (readMaybe)
import Data.Maybe
import Debug.Trace

------------------------
-- LANGUAGE STRUCTURE --
------------------------

data Expr a = 
    EVar Name               -- Variable
    | ENum Int              -- Number
    | EConstr Int Int       -- Constructor: tag, arity
    | EAp (Expr a) (Expr a) -- Application
    | ELet                  -- Let(rec) expression
        IsRec               --   True=recursive
        [Defn a]            --   Definitions
        (Expr a)            --   Expression Body
    | ECase                 -- Case expression
        (Expr a)            --   Expression being checked
        [Alter a]           --   Alternatives
    | Elam [a] (Expr a)     -- Lambda
    deriving Show

-- x + y is 
-- EAp (EAp (EVar "+") (Evar "x")) (EVar "y")

--Name: `An expr of * is either an EVar containing a name, 
--or an ELam containing a list of values of type * and an expr of values of type * and an expr of *'.
type CoreExpr = Expr Name 

type Name = String

--Whether or not the let expression is recursive
type IsRec = Bool

--Variable definition with name and expression value
type Defn a   = (a, Expr a)
type CoreDefn = Defn Name

--Each definition is (variable name being bound, expression to which it is bound)

--case expressions: expression to analyze and list of alternatives
--each alternative has tag, bound variables, and expression to the right of the arrow
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

type Program a = [ScDefn a]
type CoreProgram = Program Name

--name of supercombinator, arguments, and body
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

--True if expression has no 'internal structure'
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

--picks out the list of variables bound by the definitions
bindersOf :: [(a, b)] -> [a]
bindersOf = map fst 
    
--extracts the list of right-hand sides to which they are bound
rhssOf :: [(a, b)] -> [b]
rhssOf = map snd 

--PRELUDE DEFINITIONS

preludeDefs :: CoreProgram
preludeDefs = [ ("id", ["x"], EVar "x"),        -- id x = x ;
                ("K" , ["x", "y"], EVar "x"),   -- K x y = x ;
                ("K1", ["x", "y"], EVar "y"),   -- K1 x y = y ;
                ("S" , ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),   -- S f g x = (f x) (g x) ;
                ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),               -- compose f g x = f (g x) ;
                ("twice"  , ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]                  -- twice f = compose f f ;


--pprint :: CoreProgram -> String
--pprint 

--BAD PRETTY PRINTER CODE
--THIS IS QUADRATIC:

--prntExpr :: CoreExpr -> String
--prntExpr (ENum e) = show e
--prntExpr (EVar e) = show e
--prntExpr (EAp e1 e2) = prntExpr e1 ++ " " ++ prntAExpr e2
--
----print with parens unless atomic
--prntAExpr :: CoreExpr -> String
--prntAExpr e 
--    | isAtomicExpr e =        prntExpr e
--    | otherwise      = "(" ++ prntExpr e ++ ")"

-- SEQUENCE DATA TYPE

data Seq = SNil
        | SStr String
        | SAppend Seq Seq
        | SIndent Seq
        | SNewline
        deriving Show


sNil :: Seq
sNil = SNil
sStr :: String -> Seq
sStr = SStr --check for newline in string and replace with sNewline
sAppend :: Seq -> Seq -> Seq
sAppend SNil n    = n
sAppend n    SNil = n
sAppend n1   n2   = SAppend n1 n2

sNum :: Int -> Seq
sNum = sStr . show

sNewline :: Seq
sNewline = SNewline
sIndent :: Seq -> Seq
sIndent = SIndent
sSpace :: Int -> Seq
sSpace = sStr . flip replicate ' '

sConcat :: [Seq] -> Seq
sConcat = foldl sAppend SNil

sIntercalate :: Seq -> [Seq] -> Seq
sIntercalate _ [] = sNil
sIntercalate s (x:xs) = x `sAppend` prependToAll s xs
    where
        prependToAll s = sConcat . map (sAppend s)

sFWNum :: Int -> Int -> Seq --pads left int to specified width
sFWNum width i = padding `sAppend` sNum i
    where padding = sSpace $ width - length (show i)

sLayn :: [Seq] -> Seq -- 1. item\n   2. item\n etc
sLayn = sConcat . zipWith line [1..]
    where line i seq = sConcat [sNum i, sStr ". ", sIndent seq, sNewline] 

flatten :: Int -> [(Seq, Int)] -> String
flatten col ((SNewline     , i) : seqs) = '\n' : replicate i ' ' ++ flatten i seqs  --newLine and add indent
flatten col ((SIndent seq  , _) : seqs) = flatten col ((seq, col) : seqs)           --set indent (doesnt update string)
flatten col ((SStr s       , i) : seqs) = s ++ flatten (col + length s) seqs        --prepend string to next
flatten col ((SAppend s1 s2, i) : seqs) = flatten col ((s1, i):(s2, i):seqs)        --flatten tree to list
flatten _ _ = []

--flatten [] = ""
--flatten (SNil : seqs) = flatten seqs
--flatten (SStr s : seqs) = s ++ flatten seqs
--flatten (SAppend s1 s2 : seqs) = flatten (s1 : s2 : seqs)

sDisplay :: Seq -> String
sDisplay x = flatten 0 [(x, 0)]

--------------------
-- PRETTY PRINTER --
--------------------

prnt :: CoreProgram -> String
prnt = sDisplay . prntProg

--prints out program
prntProg :: CoreProgram -> Seq
prntProg = foldr process sNil
  where
    process x xs = sConcat [pprScDefn x, sStr "; ", sNewline, xs]

--prints out function def
pprScDefn :: CoreScDefn -> Seq
pprScDefn (name, args, expr) = sConcat [sStr name, sSpace 1, prntArgs args, sStr "= ", sIndent (prntExpr expr)]

--prints out function args
prntArgs :: [String] -> Seq
prntArgs = foldr cncat sNil
    where cncat x rs = sConcat [sStr x, sSpace 1, rs]

--prints out expression
prntExpr :: CoreExpr -> Seq
prntExpr (EVar e)    = sStr e
prntExpr (ENum e)    = sStr $ show e
prntExpr (EAp (EAp (EVar op) e1) e2)
    | head op `elem` opChars = sConcat[sStr "(", prntExpr e1, sStr $ " " ++ op ++ " ", prntAExpr e2, sStr ")"]
prntExpr (EAp e1 e2) = sConcat [ prntExpr e1, sStr " ", prntAExpr e2 ]
prntExpr (ELet r dfns e) = sConcat [
        sStr (if r then "reclet" else "let"), sNewline, -- "let" or "reclet"
        sStr " ", sIndent (prntDefns dfns), sNewline, -- definintions
        sStr "in ", prntExpr e ] -- "in" {expressions}
prntExpr (ECase expr alters) = sConcat [ 
        sStr "case ", sIndent (prntExpr expr), sStr " of", sNewline, -- case expr of 
        foldr cncat sNil alters ] --all the alternatives
    where
      cncat x xs = sConcat [sIndent (prntAlter x), sNewline, xs]


--print with parens unless atomic
prntAExpr :: CoreExpr -> Seq
prntAExpr e 
    | isAtomicExpr e =        prntExpr e
    | otherwise      = sStr "(" `sAppend` prntExpr e `sAppend` sStr ")"

prntAlter :: CoreAlt -> Seq
prntAlter (tag, vars, expr) = sConcat [
        sStr ("<" ++ show tag ++ ">: "), 
        chng vars, 
        sStr " -> ", 
        sIndent(prntExpr expr) ]
  where
    chng tgs = foldr (\x xs -> sConcat [sStr x, sStr " ",xs]) sNil tgs

--prints out variable definitions
prntDefns :: [(Name, CoreExpr)] -> Seq
prntDefns dfns = sIntercalate sep (map prntDefn dfns)
    where sep = sConcat [ sStr ";", sNewline]

--prints out variable definition
prntDefn :: (Name, CoreExpr) -> Seq
prntDefn (name, expr) = sConcat [ sStr name, sStr " = ", sIndent (prntExpr expr)]



---------------
-- TOKENIZER --
---------------

--token string is never empty
type Token = (LineNumber, String)

type LineNumber = Int

--lexical analysis
tokens :: [Char] -> [Token] 
tokens = tknRn 0

--tokens func with line number
tknRn :: LineNumber -> [Char] -> [Token] 
tknRn l [] = []
tknRn l (x:xs) 
    | isNewl  x = tknRn (l + 1) xs      --ignore newline and increase line count
    | isSpace x = tknRn l xs            --ignore whitespace
    | isCChar x = tknRn l afterComment  --ignore comments 
    | isParen x = (l, [x]) : tknRn l xs --tokenize parenthesis
    | isDigit x = tokenWhile isDigit    --tokenize numbers
    | isAlpha x = tokenWhile isIdChar   --tokenize identifiers
    | isOper  x = tokenWhile isOper     --tokenize operators
    | otherwise = error $ show x
        where
            tokenWhile f = tokenTake f : tknRn l (tokenDrop f)  --gets a token while f is true and prepends to the rest
            tokenTake  f = (l, x : takeWhile f xs)              --gets a token while a condition started at x is true
            tokenDrop  f =     dropWhile f xs                   --all text after the above token
            afterComment = tokenDrop (not.isNewl)               --skips till newline


isOper, isCChar, isIdChar, isParen, isNewl :: Char -> Bool
isNewl    = (=='\n')                                --if a newline character
isCChar   = (=='#')                                 --if the comment character
isParen   = matchAny [(=='('), (==')')]             --if a parenthesis character
isIdChar  = matchAny [isAlpha, isNumber, (=='_')]   --if part of an identifier (alpha, number, or underscore)
isOper    = (`elem` opChars)                          --if an operator token (symbol not parenthesis) 


--point free match for the mems
--match f cs val = f $ cs <*> pure val
--match f c = f . flip map c . flip ($)
--match f = (.) (f) . (flip (.) (flip ($)) . flip map)
--match = (flip (.) (flip (.) (flip ($)) . flip map)) . (.)
match :: ([Bool] -> Bool) -> [a -> Bool] -> a -> Bool
match f cs val = f $ cs <*> pure val

--returns true if value matches all conditions in the list
matchAll :: [a -> Bool] -> a -> Bool
matchAll = match and

--returns true if value matches any conditions in the list
matchAny :: [a -> Bool] -> a -> Bool
matchAny = match or

--------------------
-- PARSER LIBRARY --
--------------------

parse :: String -> CoreProgram
parse = syntax . tokens

--List of tokens -> list of possible parses
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
    --(Parser a) >>= f = Parser $ \tokens -> concat [runParser (f val) tokens' | (val, tokens') <- a tokens]
    (Parser a) >>= f = Parser $ \input -> do
                            (val , input') <- a input
                            runParser (f val) input'

instance Alternative Parser where
    empty = Parser $ const []
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input --maybe make this more defined

--checks if string (token) matches condition, outputs that string
pSat :: (String -> Bool) -> Parser String
pSat f = Parser satisfy
    where
        satisfy ((_, t):ts) | f t = [(t, ts)]
        satisfy _ = []

--takes in a string and returns a parser which parses that literal
pLit :: String -> Parser String
pLit = pSat . (==)

--parses a variable identifier
pVar :: Parser Name
pVar = pSat check
    where
        --starts with alphanumeric and is not a keyword
        check token@(c:_) = isAlpha c && token `notElem` keywords

--parses a number
pNum :: Parser Int
pNum = read <$> pSat check
    where 
        check = isJust . (readMaybe :: String -> Maybe Int)

--make these more efficient by removing ambiguity?

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = return [] <|> pOneOrMore p

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = do
            val <- p
            others <- pZeroOrMore p
            return $ val : others

--parse one or more of a with b in between each a
pOneOrMoreSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreSep a b = do
                val    <- a             --check for a
                others <- pre           --check for all the other b-a
                return $ val : others   --return a:others
    where pre = return [] <|> do 
            _   <- b                    --check for b and discard result
            val <- a                    --check for al
            others <- pre --check if zero or more
            return $ val : others       --return a:others
{-# ANN pOneOrMoreSep ("HLint: ignore Reduce duplication" :: String) #-}

-- pOneOrMoreSep :: Parser a -> Parser s -> Parser [a]
-- pOneOrMoreSep a s = 

--applies alternative (<|>) to each parser in order
pAnyOf :: [Parser a] -> Parser a
pAnyOf = foldl1 (<|>)

--returns a parser which only returns the last element
--fmapLast f = undefined

--runs parsers in sequence and returns tuple of all results
pair2 :: Applicative f => f a -> f b -> f (a, b)
pair2 a b = (,) <$> a <*> b

pair3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
pair3 a b c = (,,) <$> a <*> b <*> c

--example parsers

--hello or goodbye
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" <|> pLit "goodbye"

--hello or goodbye followed by a variable name
pGreeting :: Parser (String, String)
pGreeting = pair2 pHelloOrGoodbye pVar

--zero or more greetings
pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

--Parses to the amount of greetings in a row
pGreetingsN :: Parser Int
pGreetingsN = length <$> pZeroOrMore pGreeting

sampleTokensG :: [Token]
sampleTokensG = map f ["hello", ".", "hello", "Johnny", "Boy", "!"]
    where
        f str = (0, str)

-----------------
-- CORE PARSER --
-----------------

syntax :: [Token] -> CoreProgram
syntax ts= (takeFirst . runParser pProgram) ts
    where 
        takeFirst ((prog,[]) : others) = prog
        takeFirst (parse     : others) = takeFirst others
        takeFirst other                = error ("Syntax error" ++ show (runParser pProgram ts)) --TODO: Add line column?

--IMPORTANT NOTE:
--All parser below are simply transliterations of the language grammar (found in ./BNFSyntax.png or ./README.md):

--parses [supercombinators] from program
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreSep pSc (pLit ";")

--parses (functionName, [variable names], expression) from "functionName var1 var2 var3 = expression"
pSc :: Parser CoreScDefn
pSc = pair3 pVar (pZeroOrMore pVar) (pLit "=" *> pExpr)

--parses an expression into a CoreExpr
-- pExpr :: Parser CoreExpr
-- pExpr = pAnyOf [application, infixApp, letDef, letRecDef, caseDef, lambda, pAExpr]
--     where
--         letPattern :: IsRec -> String -> Parser (Expr Name)
--         letPattern isRec letStr = liftM2 (ELet isRec) (pLit letStr *> pDefns) (pLit "in" *> pExpr)

--         application, infixApp, letDef, letRecDef, caseDef, lambda :: Parser CoreExpr
--         application = liftM2 EAp pExpr pAExpr           -- func expr -> EAp (func expr)
--         infixApp    = liftM3 ifx pExpr pBinop pExpr     -- a * b     -> EAp (EAp (EVar "*") a) b
--             where ifx p1 op = EAp (EAp (EVar op) p1)
--         letDef      = letPattern False "let"            -- let x = 5 in f    -> ELet False [("x", 5)] f
--         letRecDef   = letPattern True  "letrec"         -- letrec x = 5 in f -> ELet True  [("x", 5)] f
--         caseDef     = liftM2 ECase cs pAlts             -- case x of cases   -> ECase PVar("x") [alts]
--             where cs = pLit "case" *> pExpr <* pLit "of" 
--         lambda      = liftM2 Elam (pOneOrMore pVar) lm  -- \x y = expr -> Elam ["x", "y"] expr
--             where lm = pLit "." *> pExpr

-- expr -> expr aexpr is left recursive :(
-- expr -> aexpr1 ... aexprn (n >= 1)


--parses an expression into a CoreExpr
-- pExpr :: Parser CoreExpr
-- pExpr = pAnyOf [letDef, letRecDef, caseDef, lambda, pExpr1]
--     where
--         letPattern :: IsRec -> String -> Parser (Expr Name)
--         letPattern isRec letStr = liftM2 (ELet isRec) (pLit letStr *> pDefns) (pLit "in" *> pExpr)

--         letDef, letRecDef, caseDef, lambda :: Parser CoreExpr
--         letDef      = letPattern False "let"            -- let x = 5 in f    -> ELet False [("x", 5)] f
--         letRecDef   = letPattern True  "letrec"         -- letrec x = 5 in f -> ELet True  [("x", 5)] f
--         caseDef     = liftM2 ECase cs pAlts             -- case x of cases   -> ECase PVar("x") [alts]
--             where cs = pLit "case" *> pExpr <* pLit "of" 
--         lambda      = liftM2 Elam (pOneOrMore pVar) lm  -- \x y = expr -> Elam ["x", "y"] expr
--             where lm = pLit "." *> pExpr

pLet :: IsRec -> String -> Parser (Expr Name) -- letExpr
pLet isRec letStr = liftM2 (ELet isRec) (pLit letStr *> pDefns) (pLit "in" *> pExpr)

pCase :: Parser CoreExpr --caseExpr
pCase = liftM2 ECase cs pAlts 
    where cs = pLit "case" *> pExpr <* pLit "of" 

pLam :: Parser CoreExpr --lambda
pLam  = liftM2 Elam (pOneOrMore pVar) lm
    where lm = pLit "." *> pExpr

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


data PartialExpr = NoOp | Op Name CoreExpr

--Represents an operator and the expression on the right
mkOp :: Parser Name -> Parser CoreExpr -> Parser PartialExpr
mkOp = liftA2 Op

--Applies the infix operator to the adjacent expressions
assembleOp :: Parser CoreExpr -> Parser PartialExpr -> Parser CoreExpr
assembleOp = liftA2 asmblOp
    where
        asmblOp :: CoreExpr -> PartialExpr -> CoreExpr
        asmblOp e1 NoOp = e1
        asmblOp e1 (Op op e2) = EAp (EAp (EVar op) e1) e2

--EXPR PARSERS BEGIN

pExpr :: Parser CoreExpr
pExpr = pAnyOf [pLet False "let", pLet True "letrec", pCase, pLam, pExpr1]

--Gets a layer of the language grammar that represents a precedence order
--The first param is the next parser to be checked if this fails
--The second is a list of ("operator", expression on the right) for all
--expressions at this level of precedence
pExprLayer :: Parser CoreExpr -> [(Name, Parser CoreExpr)] -> Parser CoreExpr
pExprLayer next partials = assembleOp next (pPartial partials) -- <|> next
    where
        pPartial :: [(Name, Parser CoreExpr)] -> Parser PartialExpr
        pPartial strs = pAnyOf $ pure NoOp:map (uncurry (mkOp . pLit)) strs

pExpr1 = pExprLayer pExpr2 [("|", pExpr1)]
pExpr2 = pExprLayer pExpr3 [("&", pExpr2)]
pExpr3 = pExprLayer pExpr4 $ map (,pExpr4) relOps
pExpr4 = pExprLayer pExpr5 [("+", pExpr4), ("-", pExpr5)]
pExpr5 = pExprLayer pExpr6 [("*", pExpr5), ("/", pExpr6)]

pExpr6 :: Parser CoreExpr
pExpr6 = fmap (foldr1 EAp) (pOneOrMore pAExpr)

pAExpr :: Parser CoreExpr
pAExpr = pAnyOf [EVar <$> pVar, ENum <$> pNum, pConst, pParenExpr]
    where
        pConst = liftA2 EConstr (pLit "Pack{" *> pNum) (pNum <* pLit "}")
        pParenExpr = pLit "(" *> pExpr <* pLit ")"

--EXPR PARSERS END

-- pBinop :: Parser Name
-- pBinop = pAnyOf [arithOps, relOps, boolOps]

opChars  = ';':map head (arithOps ++ relOps ++ boolOps)
arithOps = ["+", "-", "*", "/"]
relOps   = ["<", "<=", "==", ">=", ">"]
boolOps  = ["&", "|"]
-- turns each string to a parser, then runs pAnyOf
--transform = pAnyOf . map pLit

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

sampleProgram :: String
sampleProgram = "f = 3 ;\
\g x y = let z = x in z ;\
\h x = case let y = x in y of\
\       <1> -> 2 ;\
\       <2> -> 5 ;\
\f2 x y = case x of \
\       <1> -> case y of\
\           <1> -> 1 ; \
\       <2> -> 2 ; \
\div x y = x / (y * x) + ((y - y) - x)"

sampleTokens :: [Token]
sampleTokens = tokens sampleProgram

testFunc = runParser (pLit "test" <|> pLit "hmm")