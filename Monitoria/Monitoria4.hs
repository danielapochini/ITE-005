module Monitoria4 where 

data Produto = Produto {valor :: Double, nome :: String } deriving Show 

class Vendivel a where 
    totalPreco :: a -> a -> a
    
instance Vendivel Produto where 
    totalPreco (Produto v1 _)(Produto v2 _) = (Produto (v1+v2) "")
    
    -- https://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.Base.html#Monoid
    -- https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-Ord.html