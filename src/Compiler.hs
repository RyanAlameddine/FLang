module Compiler where

import Language
import ParserCombinator
import Parser
import Tokenizer
import PPrinter
import Control.Applicative

parse :: String -> CoreProgram
parse = syntax . tokens


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