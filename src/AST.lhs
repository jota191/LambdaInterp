> {-# LANGUAGE DeriveFunctor #-}

> module AST where

> import Data.Char
> import Data.List
> import Data.Bifunctor

> -- | Datatype for lexing information 
> type Info = String
> type Name = String


Terms are representes eith De Brujin indexes.
The Info field contains information from the parser
to manage the printing of error messages.

The extra Int parameter on TVar is for debugging

> -- | definition of Lambda Terms
> data Term a
>   = TVar Info a Int -- the second for debugging
>   | TApp Info (Term a) (Term a)
>   | TAbs Info Name (Term a)
>   deriving (Eq, Show, Functor)

> data LTerm = Term Int

> -- | Contexts
> type Context = [(String, Binding)]
> data Binding = NameBind deriving Show

> -- | Printing
> showTerm :: Context -> LTerm -> String
> showTerm ctx (TVar inf x n)
>   = if   length ctx == n
>     then fst $ ctx !! (n-(x+1))
>     else error "bad index"

> showTerm ctx (TApp inf t u)
>   = "(" ++ showTerm ctx t
>         ++ showTerm ctx u ++ ")"

> showTerm ctx (TAbs inf name t)
>   = let (ctx', name' ) = freshName ctx name
>     in  "(λ" ++ name' ++ ". " ++ showTerm ctx' t ++ ")"

> isVar :: Term a -> Bool
> isVar (TVar _ _ _) = True
> isVar _            = False

> -- | FreshName, given a ctx and a variable, find a suitable name for that
> -- variable such that it does not clash with the ctx. Returns the new
> -- ctx and the name itself
> freshName :: Context -> Name -> (Context, Name)
> freshName ctx n
>   = if not $ defined n ctx
>     then ((n, NameBind) : ctx, n)
>     else freshName ctx (incNameIndex n)

> -- | Decides if a name is defined in a context
> defined ::  Name -> Context -> Bool
> defined _ []          = False
> defined y ((x,_):ctx) = x == y || defined y ctx

> -- | If a name have a suffix, return a name with an increased value
> incNameIndex :: Name -> Name
> incNameIndex c
>   = let (index, name) = bimap reverse reverse . span isDigit . reverse $ c
>     in case index of
>          [] -> name ++ "1"
>          _  -> name ++ ((show .(+1) . read) index)

> termShift :: Int -> Int -> LTerm -> LTerm
> termShift 




K=λq.λi.q 
    quotes q and ignores i
S=λx.λy.λz.((xz)(yz))

> i = TAbs "" "x" (TVar "" (0 :: Int) 1)
> s = TAbs "" "x" (TAbs "" "y" (TAbs "" "z"
>     (TApp "" (TApp "" (TVar "" 2 3)(TVar "" 0 3 ))
>              (TApp "" (TVar "" (1 :: Int) 3)(TVar "" 0 3 )))))
> k = TAbs "" "x" (TAbs "" "y" (TVar "" (1 :: Int) 2))


