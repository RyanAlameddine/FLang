module GMachine where

import Language
import Parser
import Utils
import Data.Traversable


runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse

data GmState = GmState { getCode :: GmCode, getStack :: GmStack, getHeap :: GmHeap, getGlobals :: GmGlobals, getStats :: GmStats }

type GmCode = [Instruction]

data Instruction = Unwind
                | Pushglobal Name
                | Pushint Int
                | Push Int
                | Mkap
                | Update Int     
                | Pop Int
                deriving (Eq, Show)

type GmStack = [Addr]

type GmHeap = Heap Node

data Node = NNum Int
            | NAp Addr Addr
            | NGlobal Int GmCode 
            | NInd Addr
            deriving (Eq, Show)

type GmGlobals = ASSOC Name Addr

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps = (+1)

--state evaluator

eval :: GmState -> [GmState]
eval state = state:rest
    where
        rest | gmFinal state = []
             | otherwise      = eval nextState
        nextState = doAdmin $ step state

doAdmin :: GmState -> GmState
doAdmin state = state{getStats=statIncSteps $ getStats state}

gmFinal :: GmState -> Bool
gmFinal GmState{getCode=[]} = True
gmFinal _                = False

step :: GmState -> GmState
step state = dispatch current state{getCode=rest}
    where (current:rest) = getCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Update n)     = update n
dispatch (Pop n)        = pop n
dispatch Unwind         = unwind

pushglobal :: Name -> GmState -> GmState
pushglobal f state = state{getStack = a : getStack state}
    where a = aLookup (getGlobals state) f $ error $ "Undeclared global " ++ f

-- pushint :: Int -> GmState -> GmState
-- pushint n state = state{getHeap = heap', getStack = a : getStack state}
--     where (heap', a) = hAlloc (getHeap state) (NNum n)
pushint :: Int -> GmState -> GmState
pushint n state 
    | addr /= elseAddr = state{getStack = stack'}
    | otherwise  = state{getStack = stack', getHeap = heap', getGlobals = (show n, addr) : getGlobals state}
    where 
        stack' = addr : getStack state
        addr = aLookup (getGlobals state) (show n) elseAddr --addr = hGetAddr (getHeap state) node elseAddr
        (heap', elseAddr) = hAlloc (getHeap state) $ NNum n


mkap :: GmState -> GmState
mkap state = state{getHeap = heap, getStack = a:as}
    where 
        (heap, a)  = hAlloc (getHeap state) (NAp a1 a2)
        (a1:a2:as) = getStack state

push :: Int -> GmState -> GmState
push n state = state{getStack = an : stack}
    where 
        stack = getStack state
        an = stack !! n

-- push n state = state{getStack = a:as}
--     where 
--         as = getStack state
--         a  = getArg $ hLookup (getHeap state) (as !! (n + 1))

getArg :: Node -> Addr
getArg (NAp _ a2) = a2

-- slide :: Int -> GmState -> GmState
-- slide n state = state{getStack = a : drop n as}
--     where (a:as) = getStack state

update :: Int -> GmState -> GmState
update n state = state{getStack = setAt as n addr, getHeap = heap'} --n + 1??
    where
        (a:as) = getStack state
        (heap', addr) = hAlloc (getHeap state) (NInd a)

pop :: Int -> GmState -> GmState
pop n state = state{getStack = drop n $ getStack state}

unwind :: GmState -> GmState
unwind state = newState $ hLookup heap a
    where
        (a:as) = getStack state
        heap   = getHeap  state

        newState (NNum n) = state
        newState (NAp a1 a2) = state{getCode = [Unwind], getStack = a1:a:as}
        newState (NGlobal n c)
            | length as < n = error "Attempting to unwind with not enough arguments to apply"
            | otherwise     = state{getCode = c, getStack = rearrange n heap (a:as)}
        newState (NInd addr) = state{getCode = [Unwind], getStack = addr : as}

--replace each application node with it's right child (the actual values)
rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as = take n as' ++ drop n as
    where as' = map (getArg . hLookup heap) $ tail as


compile :: CoreProgram -> GmState
compile program = GmState{getCode=initialCode, getStack=[], getHeap=heap, getGlobals=globals, getStats=statInitial}
    where (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
    where compiled = map compileSc (preludeDefs ++ program) ++ compiledPrimitives

type GmCompiledSc = (Name, Int, GmCode) --(Name, number of args needed to reduce, compiled code instructions)

compiledPrimitives :: [GmCompiledSc]
compiledPrimitives = []

allocateSc :: GmHeap -> GmCompiledSc -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr)) 
    where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSc
compileSc (name, env, body) = (name, length env, compileR body env')
    where env' = zip env [0..]

--compiler schemes
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

--R-scheme
compileR :: GmCompiler
compileR e env = compileC e env ++ [Update d, Pop d, Unwind] 
    where d = length env --this might be wrong maybe?
--compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]

--C-scheme
compileC :: GmCompiler
compileC (EVar v) env
    | elem v $ aDomain env = [Push n]
    | otherwise            = [Pushglobal v]
        where n = aLookup env v $ error "Couldn't find v, but we just checked 'elem v'. This is practically impossible to throw."
compileC (ENum n)    env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v, m) <- env]

--pretty printer
showResults :: [GmState] -> String
showResults states = let s = head states in sDisplay $ sConcat 
    [
        sStr "Supercombinator definintions", 
        sNewline,
        sIntercalate sNewline $ map (showSc s) (getGlobals s),
        sNewline, sNewline, 
        sStr "State transitions",
        sNewline, sNewline,
        sLayn $ map showState states,
        sNewline, sNewline,
        showStats $ last states
    ]

showSc :: GmState -> (Name, Addr) -> Seq
showSc s (name, addr) 
    = sConcat [sStr "Code for ", sStr name, sNewline,
                showInstructions code, sNewline, sNewline]
    where (NGlobal arity code) = hLookup (getHeap s) addr 

showInstructions :: GmCode -> Seq
showInstructions is 
    = sConcat [sStr " Code:{",
                sIndent $ sIntercalate sNewline $ map showInstruction is,
                sStr "}", sNewline]

showInstruction :: Instruction -> Seq
showInstruction Unwind         = sStr "Unwind"
showInstruction (Pushglobal f) = sStr "Pushglobal " `sAppend` sStr f
showInstruction (Push n)       = sStr "Push "       `sAppend` sNum n
showInstruction (Pushint n)    = sStr "Pushint "    `sAppend` sNum n
showInstruction Mkap           = sStr "Mkap"
showInstruction (Update n)     = sStr "Update "     `sAppend` sNum n
showInstruction (Pop n)        = sStr "Pop "        `sAppend` sNum n

showState :: GmState -> Seq
showState s = sConcat [showStack s, sNewline,
                        showInstructions $ getCode s, sNewline]

showStack :: GmState -> Seq
showStack s = sConcat [sStr " Stack:[", sIndent $ sIntercalate sNewline $ map (showStackItem s) (reverse $ getStack s), sStr "]"]

showStackItem :: GmState -> Addr -> Seq
showStackItem s a = sConcat [showAddr a, sStr ": ",
                                showNode s a $ hLookup (getHeap s) a]

showAddr :: Addr -> Seq
showAddr = sStr . show

showNode :: GmState -> Addr -> Node -> Seq
showNode s a (NNum n)      = sNum n
showNode s a (NAp a1 a2 )  = sConcat [sStr "Ap ", showAddr a1, 
                                     sStr " ", showAddr a2]
showNode s a (NGlobal n g) = let v = head [n | (n, b) <- getGlobals s, a==b] 
                             in sConcat [sStr "Global ", sStr v]
showNode s a (NInd addr)   = let v = hLookup (getHeap s) addr 
                             in sConcat [sStr "Indirect ", showNode s addr v]


showStats :: GmState -> Seq
showStats s = sConcat [sStr "Steps taken = ", sNum $ getStats s]