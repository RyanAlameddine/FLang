module ParserCombinator where


import Control.Applicative
import Data.Maybe
import Text.Read
import Control.Arrow
import Data.Char

--token string is never empty
type Token = (LineNumber, String)

type LineNumber = Int

--------------------
-- PARSER LIBRARY --
--------------------

--List of tokens -> list of possible parses
newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])] }

instance Functor Parser where
    fmap f (Parser p) = Parser (fmap transform p)
        where
            transform = map $ first f

instance Applicative Parser where
    pure val = Parser $ \input -> [(val, input)]
    (Parser f) <*> (Parser a) = Parser $ \input -> do
                                            (func, input' ) <- f input 
                                            (val , input'') <- a input'
                                            return (func val, input'')

instance Monad Parser where
    --(Parser a) >>= f = Parser $ \tokens -> concat [runParser (f val) tokens' | (val, tokens') <- a tokens]
    (Parser a) >>= f = Parser $ \input -> do
                            (val , input') <- a input
                            runParser (f val) input'

instance Alternative Parser where
    empty = Parser $ const []
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input --maybe make this more defined

--checks if string (token) matches condition, outputs that string
pSat :: (String -> Bool) -> Parser String
pSat f = Parser satisfy
    where
        satisfy ((_, t):ts) | f t = [(t, ts)]
        satisfy _ = []

--takes in a string and returns a parser which parses that literal
pLit :: String -> Parser String
pLit = pSat . (==)

--parses a number
pNum :: Parser Int
pNum = read <$> pSat check
    where 
        check = isJust . (readMaybe :: String -> Maybe Int)

--make these more efficient by removing ambiguity?

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = return [] <|> pOneOrMore p

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = do
            val <- p
            others <- pZeroOrMore p
            return $ val : others

--parse one or more of a with b in between each a
pOneOrMoreSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreSep a b = do
                val    <- a             --check for a
                others <- pre           --check for all the other b-a
                return $ val : others   --return a:others
    where pre = return [] <|> do 
            _   <- b                    --check for b and discard result
            val <- a                    --check for al
            others <- pre --check if zero or more
            return $ val : others       --return a:others
{-# ANN pOneOrMoreSep ("HLint: ignore Reduce duplication" :: String) #-}

-- pOneOrMoreSep :: Parser a -> Parser s -> Parser [a]
-- pOneOrMoreSep a s = 

--applies alternative (<|>) to each parser in order
pAnyOf :: [Parser a] -> Parser a
pAnyOf = foldl1 (<|>)

--returns a parser which only returns the last element
--fmapLast f = undefined

--runs parsers in sequence and returns tuple of all results
pair2 :: Applicative f => f a -> f b -> f (a, b)
pair2 a b = (,) <$> a <*> b

pair3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
pair3 a b c = (,,) <$> a <*> b <*> c

--example parsers

--hello or goodbye
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" <|> pLit "goodbye"

--hello or goodbye followed by a variable name
pGreeting :: Parser (String, String)
pGreeting = pair2 pHelloOrGoodbye $ pSat $ all isAlpha

--zero or more greetings
pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

--Parses to the amount of greetings in a row
pGreetingsN :: Parser Int
pGreetingsN = length <$> pZeroOrMore pGreeting

sampleTokensG :: [Token]
sampleTokensG = map f ["hello", ".", "hello", "Johnny", "Boy", "!"]
    where
        f str = (0, str)