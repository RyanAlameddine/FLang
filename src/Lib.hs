module Lib
    ( someFunc
    ) where

import Parser
import GMachine

someFunc :: IO ()
someFunc = putStrLn "someFunc"
--parse :: [Char] -> CoreProgram
--compile :: CoreProgram -> TiState
--eval :: TiState -> [TiState]
--showResults :: [TiState] -> [Char]

main :: IO ()
--main = putStr $ runProg "main = S K K 3"
-- main = putStr $ runProg "pair x y f = f x y ;\
-- \fst p = p K ;\
-- \snd p = p K1 ;\
-- \f x y = letrec\
-- \        a = pair x b ;\
-- \        b = pair y a\
-- \    in\
-- \    fst (snd (snd (snd a))) ;\
-- \main = f 3 4"
main = putStr $ runProg "main = S K K 1"

revr :: String -> String
revr = foldl (flip (:)) []