{-# LANGUAGE TupleSections #-}

module TiMachine where

import Language
import Utils
import Parser
import Data.Traversable
import Debug.Trace

--template instantitation machine

--normal order reduction

--until there are no more redexes 
    --select the outermost one
    --reduce it
    --update the root of the redex with the result
--end

runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

type TiDump = [TiStack]

type TiHeap = Heap Node

getHeap (_, _, heap, _, _) = heap

data Node = NAp Addr Addr                       --function application node
            | NSupercomb Name [Name] CoreExpr   --supercombinator (function name, param names, expression)
            | NNum Int                          --primitive integer type
            | NInd Addr                         --indirection
            | NPrim Name Primitive              --primitive

data Primitive = Neg | Add | Sub | Mul | Div

type TiGlobals = ASSOC Name Addr

newtype TiStats = TiStats { tiStatGetSteps::Int }

tiStatInitial :: TiStats
tiStatInitial = TiStats 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (TiStats s) = TiStats $ s + 1

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (s, d, h, g, stats) = (s, d, h, g, f stats)

extraPreludeDefs = []

primitives :: ASSOC Name Primitive
primitives = [ ("negate", Neg),
               ("+", Add), ("-", Sub), 
               ("*", Mul), ("/", Div) ]

--compiler
compile :: CoreProgram -> TiState
compile program 
    = (initStack, [], initHeap, globals, tiStatInitial)
        where
            scDefs = program ++ preludeDefs ++ extraPreludeDefs
            (initHeap, globals) = buildInitHeap scDefs

            initStack = [mainAddr]
            mainAddr = aLookup globals "main" (error "main not defined")

buildInitHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitHeap defs = (heap2, scs ++ prims)
    where
        (heap1, scs)   = mapAccumL allocateSc hInitial defs
        (heap2, prims) = mapAccumL allocatePrim heap1 primitives

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
    where (heap', addr) = hAlloc heap (NPrim name prim)

--evaluator
eval state = state : rest
    where
        rest | tiFinal state = []
            | otherwise = eval next
        next = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatIncSteps

tiFinal :: TiState -> Bool
tiFinal ([addr], _, heap, _, _) = isDataNode $ hLookup heap addr
tiFinal ([], _, _, _, _) = error "Empty Stack"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode _ = False

step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) = dispatch $ hLookup heap $ head stack
    where
        dispatch (NNum n)                  = numStep state
        dispatch (NAp a1 a2)               = apStep  state a1 a2
        dispatch (NSupercomb sc args body) = scStep  state sc args body
        dispatch (NPrim n p)               = prmStep state p
        dispatch (NInd a)                  = indStep state a

numStep :: TiState -> TiState
numStep ([n], stack:dump, heap, globals, stats) = (stack, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1:stack, dump, heap, globals, stats)

prmStep :: TiState -> Primitive -> TiState
prmStep state Neg = primNeg state

indStep :: TiState -> Addr -> TiState
indStep (_:stack, dump, heap, globals, stats) addr = (addr:stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) name argNames body = (stack', dump, heap', globals, stats)
    where
        stack'
            | argLen <= length stack = rootAddr : drop argLen stack
            | otherwise = error "Supercombinator is not applied to enough args"
        argLen = length argNames + 1
        --heap'' = hUpdate heap' rootAddr (NInd resultAddr)                --heap after updates and indirects
        heap' = instantiateAndUpdate body rootAddr heap env--heap after instantiation
        env = argBindings ++ globals
        rootAddr = stack !! (argLen - 1)
        argBindings = zip argNames $ getArgs heap stack

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map getArg stack
    where 
        getArg :: Int -> Addr
        getArg addr = arg --trace ("Found Arg " ++ show (hLookup heap addr) ++ ": " ++ show (hLookup heap arg)) arg
            where (NAp f arg) = hLookup heap addr

--takes an expression, heap, and environment associating names with addresses
--creates an instance of the expression in the heap, and returns a new heap
--and address of the root of the instance.
instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiate (ENum n   ) heap env = hAlloc heap $ NNum n
instantiate (EAp e1 e2) heap env = instantiateAp heap env e1 e2 hAlloc
instantiate (EVar v   ) heap env = (heap, aLookup env v $ error $ "Undefined Name " ++ show v)
instantiate (EConstr tag ar    ) heap env = instantitateConstr tag ar heap env
instantiate (ELet isrc defs bdy) heap env = instantiateLet isrc defs bdy heap env instantiate
instantiate (ECase e alts      ) heap env = error "Can't instantiate case exprs"

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> ASSOC Name Addr -> TiHeap
instantiateAndUpdate (ENum n   ) addr heap env = hUpdate heap addr $ NNum n
instantiateAndUpdate (EAp e1 e2) addr heap env = instantiateAp heap env e1 e2 (`hUpdate` addr)
instantiateAndUpdate (EVar v   ) addr heap env = hUpdate heap addr $ NInd $ aLookup env v $ error $ "Undefined Name " ++ show v
instantiateAndUpdate (EConstr tag ar    ) addr heap env = instantitateConstr tag ar heap env
instantiateAndUpdate (ELet isrc defs bdy) addr heap env = instantiateLet isrc defs bdy heap env (`instantiateAndUpdate` addr)
instantiateAndUpdate (ECase e alts      ) addr heap env = error "Can't instantiate case exprs"

instantiateAp heap env e1 e2 f = f heap2 (NAp a1 a2) 
    where
        (heap1, a1) = instantiate e1 heap  env
        (heap2, a2) = instantiate e2 heap1 env

instantitateConstr tag arity heap env = error "Not Implemented"

instantiateLet isRec defs body heap env instF = instF body heap' env'
        where
            getEnv | isRec  = env'
                | otherwise = env
            env'  = vals ++ env
            (heap', vals) = mapAccumL mapper heap defs
            mapper hp (name, expr) = (name,) <$> instantiate expr hp getEnv

--primitives

primNeg :: TiState -> TiState
primNeg ([prim, ap], dump, heap, globals, stats) 
    | evaluated = ([ap]   , dump           , hUpdate heap ap (NNum (-n)), globals, stats)
    | otherwise = ([nAddr], [prim,ap]:dump , heap                       , globals, stats)
    where
        [nAddr] = getArgs heap [prim, ap]
        nNode   = hLookup heap nAddr
        evaluated = isDataNode nNode
        n = let (NNum num) = nNode in num

------------------
--Pretty Printer--
------------------
showResults :: [TiState] -> String
showResults states = sDisplay (sConcat [sLayn $ map showState states, showStats $ last states])

showResultsAndHeap states = sDisplay (sConcat [sLayn $ map showStateAndHeap states, showStats $ last states])

showState :: TiState -> Seq
showState (stack, dump, heap, globals, stats) = sConcat [showStack heap stack, sNewline]

showStateAndHeap (stack, dump, heap, globals, stats) = sConcat [showStack heap stack, sNewline, showHeap heap, sNewline]

showStack :: TiHeap -> TiStack -> Seq
showStack heap stack = sConcat 
                        [sStr "Stk [", 
                        sIndent $ sIntercalate sNewline $ map showStackItem stack,
                        sStr " ]"]
    where 
        showStackItem addr = sConcat [showFWAddr addr, sStr ": ", showStkNode heap $ hLookup heap addr]

showStkNode :: TiHeap -> Node -> Seq
showStkNode heap (NAp f arg) = sConcat [sStr "NAp ", showFWAddr f,
                                        sStr " ", showFWAddr arg, sStr " (",
                                        showNode $ hLookup heap arg, sStr ")"]
showStkNode _ node = showNode node

showNode (NSupercomb name args body) = sStr ("NSupercomb " ++ name)
showNode (NAp a1 a2) = sConcat [sStr "NAp ", showAddr a1, sStr " "   , showAddr a2]
showNode (NNum n)    = sStr "NNum " `sAppend` sNum n
showNode (NInd a)    = sConcat [sStr "NInd ", showAddr a]
showNode (NPrim n p) = sStr "NPrim " `sAppend` sStr n

showAddr addr = sStr $ show addr

--show address in field of width 4
showFWAddr addr = let str = show addr in sSpace (4 - length str) `sAppend` sStr str 

showStats (stack, dump, heap, globals, stats) = sConcat [sNewline, sNewline, 
                                                        sStr "Total Number of Steps = ", 
                                                        sNum (tiStatGetSteps stats)]

showHeap :: TiHeap -> Seq
showHeap (Heap _ _ items) = sIntercalate (sStr ", " `sAppend` sNewline) (map toSeq items)
    where
        toSeq (addr, item) = sConcat [showFWAddr addr, sStr ": ", showNode item]