A model for Un(yet)-typed Lambda calculus

Juan García Garland (Nov. 2016)
License: GPLv3

> {-# LANGUAGE StandaloneDeriving #-}

> module Model where
> import qualified Data.Map.Strict as Map
> import Data.Maybe

> type Name = String

Names will be used for Variables.
There are more posibilities such that use De Brujin indexes.

> type Env = Map.Map Name Closure

An environment is a Mapping from Names to Closures.

> data Closure = Closure { getTerm :: LambExp,
>                          getEnv  :: Env}
>              deriving (Eq,Show)

> type Stack = [Closure]

> data LambExp = Var Name
>              | Abs Name LambExp
>              | App LambExp LambExp
>              | Print String
>              deriving Eq

> type Term = LambExp

Lambda expressions are defined trivially as an algebraic datatype.
TODO: This could be much more modular, for instance Names should be
parametric, not Strings..
TODO: implement some built-in stuff



> --deriving instance Show (LambExp)
> 
> instance Show LambExp where
>   show = show2
> show2 (Var n) = n
> show2 (Abs x t) = "(" ++ "λ" ++ x ++ "." ++ show2 t ++ ")"
> show2 (App t v) =  "(" ++ show2 t ++ " " ++ show2 v ++ ")"


identity: (λx.x)

> identity = Abs "x" (Var "x")

true:  (λt.(λf.t))
false: (λt.(λf.f))

> true     = Abs "t" $ Abs "f" $ Var "t"
> false    = Abs "t" $ Abs "f" $ Var "f"
> true2    = Abs "x" $ Abs "y" $ Var "x"

neg: (λb.b F T) → (λb.b (λt.(λf.f)) (λt.(λf.t)))

> neg      = Abs "b" $ (App (App (Var "b") false) true)


> zero = Abs "y" (Abs "t" (Var "t"))
> suc
>   = Abs "n"
>     (Abs "f"
>       (Abs "x" (App (Var "f")(
>                 (App (App (Var "n") (Var "f")) (Var "x"))))))



Some manipulation of terms:
When the machine reduces it searches for the pattern with the
scheme λx1...xn t where t is not an abstraction.
We define a function to collect all the variables and return t

> rmAbs :: Term -> ([Name], Term)
> rmAbs (Abs x t) = ((x:vs), u)
>   where (vs, u) = rmAbs t
> rmAbs (Var x)   = ([],Var x)
> rmAbs (App t u) = ([],App t u)


This will be used to perform the transition when there is an
abstraction.
For each parameter x in the lambda term (that we extracted with
rmAbs, pop a closure (t,e) from the stack, and put x = (t,e) in the
top level environment)

> updateEnv :: [Name] -> Env -> Stack -> (Env,Stack)
> updateEnv []     e s = (e,s)
> updateEnv (x:xs) e (te:s)
>   = updateEnv xs (Map.insert x te e) s
> updateEnv _ e []
>   = (e,[])

> type MachineState = (Term, Env, Stack)

> excStep :: (Term, Env, Stack) -> (Term, Env, Stack)
> excStep (App u v, e, s)
>   = (u, e, (Closure v e):s)
> excStep (Var x,   e, s)
>   = let (Closure t_x e_x) = fromJust (Map.lookup x e)
>     in (t_x, e_x, s)
> excStep (Abs x v, e, s)
>   = let (varlist,t) = rmAbs (Abs x v)
>         (enew,snew) = updateEnv varlist e s
>     in case s of
>          [] -> (Abs x v, e, s)
>          _  -> (t, enew, snew)


> reduce :: (Term, Env, Stack) -> Term
> reduce s@(t,e,st)
>   = let s' = excStep s
>     in if s == s'
>        then t
>        else reduce s'


> reduceN :: Int -> (Term, Env, Stack) -> (Term, Env, Stack)
> reduceN 0 s@(t,e,st)
>   = s

> reduceN n s@(t,e,st)
>   = let s' = excStep s
>     in reduceN (n-1) s'
