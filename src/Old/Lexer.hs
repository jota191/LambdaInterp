
module Lexer where

import Data.Char
import Model (Name)
import Exception (Exc)

-- Tokens, with attributes if the associated lexeme is not a keyword

data Token = TLambda
           | TPoint
           | TVar Name
           | TLParen
           | TRParen
           | WhiteSpace
           deriving (Eq,Show)


scan :: String -> Exc (Token,String)
scan ('.':xs) = return (TPoint, xs)
scan ('λ':xs) = return (TLambda,xs)
scan ('l':'a':'m':'b':'d':'a':xs) = return (TLambda,xs)
scan ('l':'a':'m':xs) = return (TLambda,xs)
scan ('(':xs) = return (TLParen,xs)
scan (')':xs) = return (TRParen,xs)

scan (' ':xs) = return (WhiteSpace ,xs')
  where xs' = dropWhile ((==)' ') xs 


scan (n:xs) = return (TVar ([n]++ame) ,xs')
  where ame   = takeWhile isAlpha xs
        xs'   = dropWhile isAlpha xs 


lexer :: String -> Exc [Token]
lexer [] = return []
lexer inp
  = scan inp >>= \(tok,tail) ->
    lexer tail >>= \toks ->
    return (tok:toks)
