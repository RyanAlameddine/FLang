module PPrinter where

import Language
import Seq

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