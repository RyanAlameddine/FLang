module Core where

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
preludeDefs = [ ("id", ["x"], EVar "x"), -- id x = x
                ("K" , ["x", "y"], EVar "x"), -- K x y = x
                ("K1", ["x", "y"], EVar "y"), -- K1 x y = y
                ("S" , ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))), -- S f g x = (f x) (g x)
                ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))), -- f (g x)
                ("twice"  , ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ] -- compose f f