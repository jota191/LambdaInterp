A model for Un(yet)-typed Lambda calculus

Juan García Garland (Nov. 2016)
License: GPLv3

> {-# LANGUAGE StandaloneDeriving #-}

> module Model where
> import qualified Data.Map.Strict as Map


> type Name = String

Names will be used for Variables.
There are more posibilities such that use De Brujin indexes.

> type Env = Map.Map Name LambExp

An environment is a Mapping from Names to Lambda Expressions.
When evaluation/substitution is occurring at some point of a term,
the environment contains the value of captured Variables in that scope

> data LambExp = Var Name
>              | Abs Name LambExp
>              | App LambExp LambExp
>              deriving Eq

Lambda expressions are defined trivially as an algebraic datatype.

Todo: implement some built-in  stuff

> --deriving instance Show (LambExp)
> 
> instance Show LambExp where
>   show = show2
> show2 (Var n) = n
> show2 (Abs x t) = "(" ++ "λ" ++ x ++ "." ++ show2 t ++ ")"
> show2 (App t v) =  show2 t ++ " " ++ show2 v


> subst :: Env -> LambExp -> LambExp
> subst e t
>   = case t of
>       Var x -> case Map.lookup x e of
>                  Just exp -> exp
>                  Nothing -> Var x
>       App u v -> App (subst e u)(subst e v)
>       Abs f v -> case Map.lookup f e of
>                    Just exp -> Abs f (subst (Map.delete f e) v)
>                    Nothing  -> Abs f (subst e v)


Implementing reduction to \emph{weak head normal form}

> whnf t e
>   = case t of
>       App u v -> case whnf u e of
>                    Abs f w -> whnf (subst (Map.insert f v e) w)
>                               (Map.insert f v e)
>                    u' -> if u == u'
>                          then t
>                          else App u' v
>       _ -> t


identity: (λx.x)

> identity = Abs "x" (Var "x")

true:  (λt.(λf.t))
false: (λt.(λf.f))

> true     = Abs "t" $ Abs "f" $ Var "t"
> false    = Abs "t" $ Abs "f" $ Var "f"

neg: (λb.b F T) → (λb.b (λt.(λf.f)) (λt.(λf.t)))

> neg      = Abs "b" $ (App (App (Var "b") false) true)


> zero = Abs "f" (Abs "x" (Var "x"))
> succ = undefined
