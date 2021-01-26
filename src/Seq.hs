module Seq where

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