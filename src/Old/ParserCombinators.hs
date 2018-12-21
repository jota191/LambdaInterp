{-# LANGUAGE InstanceSigs #-}
-- | Parser combinators
-- | Juan García Garland (Nov. 2016)
-- | License: GPLv3

module ParserCombinators where

import Control.Monad
import Control.Applicative

-- | Datatype for parsers
data Parser s a = Parser {runP :: s -> [(a,s)]}


-- | We will use monadic parsers
instance Monad (Parser s) where
  return a = Parser $ \s -> [(a,s)]
  p >>= q  = Parser $ \s -> concat [runP (q a) s' | (a,s') <- runP p s]


-- | Parser Combinators

pFail :: Parser s a
pFail = Parser $ \s -> []


pSucceed :: a -> Parser s a
pSucceed a = Parser $ \s -> [(a,s)]

instance Alternative (Parser s) where
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  p <|> q = Parser $ \s -> runP p s ++ runP q s
  empty   =  Parser $ \char -> empty
--infixr 6 <|>

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap = liftM

instance Applicative (Parser s) where
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) = ap
  pure = return

pList :: Parser s a -> Parser s [a]
pList p = (:) <$> p <*> pList p
       <|> pSucceed [] 
