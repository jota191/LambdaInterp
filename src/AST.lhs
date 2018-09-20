
> -- | Datatype for lexing information 
> type Info = String
> type Name = String


Terms are representes eith De Brujin indexes.
The Info field contains information from the parser
to manage the printing of error messages.

The extra Int parameter on TVar is for debugging

> -- | definition of Lambda Terms
> data Term
>   = TVar Info Int Int -- the second for debugging
>   | TApp Info Term Term
>   | TAbs Info Name Term
>   deriving (Eq, Show)


> -- | Contexts
> type Context = [(String, Binding)]
> data Binding = NameBind

> -- | Printing
> showTerm :: Context -> Term -> String

> showTerm ctx (TVar inf x n)
>   = if   length ctx == n
>     then  fst $ ctx !! (n-(x+1))
>     else error "bad index"

> showTerm ctx (TApp inf t u)
>   = "(" ++ showTerm ctx t
>         ++ showTerm ctx u ++ ")"

> showTerm ctx (TAbs inf name t)
>   = error "not yet implemented"



> -- | some tests
