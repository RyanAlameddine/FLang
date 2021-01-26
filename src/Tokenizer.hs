module Tokenizer where

import Core
import ParserCombinator
import Data.Char

---------------
-- TOKENIZER --
---------------

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