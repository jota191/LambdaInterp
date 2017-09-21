
-- | Exception module for P
-- | Juan García Garland (Nov. 2016)

{-# LANGUAGE DeriveFunctor #-}

module Exception where

import Control.Monad

data Exc a = Return a
           | Error String
           deriving (Read,Show,Functor)




instance Applicative Exc where
  (<*>) = ap
  pure = return
instance Monad Exc where
  return  = Return
  m >>= f = case m of
              Error s  -> Error s
              Return a -> f a 
  fail    = Error

extract :: Exc a -> a
extract (Return a) = a
extract (Error s)  = error s
