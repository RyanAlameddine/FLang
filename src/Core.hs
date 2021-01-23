module Core where

import Data.Char
import Control.Monad
import Control.Arrow
import Control.Applicative
import Text.Read
import Data.Maybe

------------------------
-- LANGUAGE STRUCTURE --
------------------------

data Expr a = 
    EVar Name     -- Variable
    | ENum Int              -- Number
    | EConstr Int Int       -- Constructor: tag, arity
    | EAp (Expr a) (Expr a) -- Application
    | ELet                  -- Let(rec) expression
        IsRec               --   True=recursive
        [(a, Expr a)]       --   Definitions
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
recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

--Each definition is (variable name being bound, expression to which it is bound)

--picks out the list of variables bound by the definitions
bindersOf :: [(a, b)] -> [a]
bindersOf = map fst --bindersOf defns = [name | (name, rhs) <- defns]
    

--extracts the list of right-hand sides to which they are bound
rhssOf :: [(a, b)] -> [b]
rhssOf = map snd --rhssOf defns = [rhs | (name, rhs) <- defns]


--case expressions: expression to analyze and list of alternatives
--each alternative has tag, bound variables, and expression to the right of the arrow
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

--True if expression has no 'internal structure'
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

--name of supercombinator, arguments, and body
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram
preludeDefs = [ ("id", ["x"], EVar "x"), -- id x = x ;
                ("K" , ["x", "y"], EVar "x"), -- K x y = x ;
                ("K1", ["x", "y"], EVar "y"), -- K1 x y = y ;
                ("S" , ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))), -- S f g x = (f x) (g x) ;
                ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))), -- compose f g x = f (g x) ;
                ("twice"  , ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ] -- compose f f ;


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
sConcat = foldl1 sAppend 

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
flatten col ((SNewline     , i) : seqs) = '\n' : replicate i ' ' ++ flatten i seqs    --newLine and add indent
flatten col ((SIndent seq  , _) : seqs) = flatten col ((seq, col) : seqs)             --set indent (doesnt update string)
flatten col ((SStr s       , _) : seqs) = s ++ flatten col seqs                       --prepend string to next
flatten col ((SAppend s1 s2, _) : seqs) = flatten col ((s1, col):(s2, col):seqs)      --flatten tree to list
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
prntExpr (ENum e)    = sStr (show e)
prntExpr (EAp e1 e2) = sConcat [ prntExpr e1, sStr " ", prntAExpr e2 ]
prntExpr (ELet r dfns e) = sConcat [
        sStr (if r then "reclet" else "let"), sNewline, -- "let" or "reclet"
        sStr " ", sIndent (prntDefns dfns), sNewline, -- definintions
        sStr "in ", prntExpr e ] -- "in" {expressions}
prntExpr (ECase expr alters)
  = sConcat [ sStr "case ", sIndent (prntExpr expr), sStr " of", sNewline, -- case expr of 
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
        sStr (show "<" ++ show tag ++ ">: "), 
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

