> module Name where



> -- * subindexes

> isSubindex :: Char -> Bool
> isSubindex c
>   = ord c >= 8320 && ord c <= 8329

> getNumSubindex :: Char -> Int
> getNumSubindex
>   = (flip (-) 8320) . ord

> cnum2Subindex :: Char -> Char
> cnum2Subindex
>   = chr . ((+)8272) . ord 

> toUpperSubindex :: Char -> Char
> toUpperSubindex
>   =  chr . (flip (-) 8272) . ord

> getSubindex :: Name -> Int
> getSubindex n
>   = let index =  reverse . takeWhile isSubindex . reverse $ n
>     in  case index of
>           [] -> 0
>           _  -> read . map toUpperSubindex $ index

> chopSubindex :: Name -> Name
> chopSubindex
>   = reverse . dropWhile (not . isSubindex). reverse

> addSubindex :: Int -> Name -> Name
> addSubindex i n
>   = let index =  show . map cnum2Subindex . show $ i
>     in  index
