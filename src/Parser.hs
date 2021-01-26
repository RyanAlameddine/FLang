{-# LANGUAGE TupleSections #-}
module Parser where

import Language
import ParserCombinator
import Control.Applicative
import Control.Monad
import Data.Char

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

--parses a variable's identifier
pVar :: Parser Name
pVar = pSat check
    where
        --starts with alphanumeric and is not a keyword
        check token@(c:_) = isAlpha c && token `notElem` keywords

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