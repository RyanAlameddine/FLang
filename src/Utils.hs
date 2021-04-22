module Utils where

import Data.Traversable

--------------------
-- HEAP DATA TYPE --
--------------------

data Heap a = Heap Int [Addr] [(Addr, a)] --(size, unused addresses, map)
type Addr = Int

instance Functor Heap where
    fmap f (Heap s a ((addr, val):nodes)) = Heap s a $ map mapper nodes
        where
            mapper (addr, val) = (addr, f val)

instance Foldable Heap where
    foldMap f (Heap s a nodes) = mconcat $ map (f.snd) nodes

instance Traversable Heap where
    traverse f (Heap s a nodes) = fmap (Heap s a) pairs
        where 
            vals = traverse f (map snd nodes)
            addrs = map fst nodes
            pairs = fmap (zip addrs) vals


hInitial :: Heap a
hInitial = Heap 0 [1..] []

hAlloc   :: Heap a -> a -> (Heap a, Addr)
hAlloc  (Heap s (next:free) cts) n   = (Heap (s+1) free     ((next, n) : cts), next)

hUpdate  :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap s free        cts) a n =  Heap s     free     ((a,n) : remove cts a)

hFree    :: Heap a -> Addr -> Heap a
hFree   (Heap s free        cts) a   =  Heap (s-1) (a:free) (remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (Heap s free cts) a = aLookup cts a (error $ "can't find node " ++ show a ++ " in heap")

hGetAddr :: (Eq a) => Heap a -> a -> Addr -> Addr
hGetAddr (Heap _ _   cts) a def = match [addr | (addr, val) <- cts, val == a]
    where 
        match [ ] = def
        match [v] = v
        match _   = error "hGetAddr found more than one match in the heap"

hAddrs  :: Heap a -> [Addr]
hAddrs  (Heap s _    cts)   = [addr | (addr, node) <- cts]

hSize   :: Heap a -> Int
hSize   (Heap s _    _  )   = s

hToList :: Heap a -> [a]
hToList = foldl (flip (:)) [] 

remove :: [(Int,a)] -> Int -> [(Int,a)]
remove [] a = error ("Attempt to update or free nonexistent address #" ++ show a)
remove ((a',n):ts) a | a == a' = ts
    | a /= a' = (a',n) : remove ts a

type ASSOC a b = [(a,b)]

aLookup :: Eq a => ASSOC a b -> a -> b -> b
aLookup [] k' def    = def
aLookup ((k, v):kvs) k' def 
    | k == k' = v
    | k /= k' = aLookup kvs k' def

aDomain = map fst
aRange  = map snd

setAt :: [a] -> Int -> a -> [a]
setAt (x:xs) 0 v = v:xs
setAt (x:xs) i v = x : setAt xs (i - 1) v
setAt _ i _ = error $ "Specified integer " ++ show i ++ " was outside of bounds of the list"

------------------------
-- SEQUENCE DATA TYPE --
------------------------

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
flatten col ((SNil         , i) : seqs) = flatten col seqs                          --skip the nil
flatten _ [] = ""

--flatten [] = ""
--flatten (SNil : seqs) = flatten seqs
--flatten (SStr s : seqs) = s ++ flatten seqs
--flatten (SAppend s1 s2 : seqs) = flatten (s1 : s2 : seqs)

sDisplay :: Seq -> String
sDisplay x = flatten 0 [(x, 0)]