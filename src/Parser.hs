
module Parser where

import ParserCombinators
import Control.Applicative
import Lexer
import Model
import Exception

type PParser = Parser [Token]


pToken :: Token -> PParser Token
pToken t
  = Parser $ \s -> case s of
                     []     -> []
                     (t':ts) -> if t==t'
                                then [(t,ts)]
                                else []

pTLambda = pToken TLambda
pTPoint  = pToken TPoint
pTLParen = pToken TLParen
pTRParen = pToken TRParen


pTVar :: PParser String
pTVar = Parser $ \ts -> case ts of
                          (TVar name):ts' -> [(name,ts')]
                          _               -> []


pVar :: PParser LambExp
pVar = (\name -> Var name) <$> pTVar

pAbs :: PParser LambExp
pAbs = (\lam var pnt term -> Abs var term) <$>
       pTLambda <*> pTVar <*> pTPoint <*> pTerm

pApp :: PParser LambExp
pApp = (\_ t u _-> App t u) <$> pTLParen <*> pTerm <*> pTerm <*> pTRParen

pTerm :: PParser LambExp
pTerm = pAbs <|> pApp <|> pVar

test = (runP pTerm) . filter (/= WhiteSpace) . extract . lexer

