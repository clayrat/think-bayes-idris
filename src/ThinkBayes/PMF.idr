module ThinkBayes.PMF

import Data.AVL.Dict

%access export

public export
PMF : Type -> Type
PMF a = Dict a Double

get : (Ord a) => a -> PMF a -> Maybe Double
get = lookup 

domain : PMF a -> List a
domain = keys

pmfFromList : (Ord a) => List a -> PMF a
pmfFromList xs = 
  let s = length xs in
    foldl (\d,v => insert v (1.0/cast s) d) empty xs

mult : (Ord a) => a -> Double -> PMF a -> PMF a
mult a f = update a (*f)

tot : PMF a -> Double
tot = sum . values 

mean : (Num a, Cast a Double) => PMF a -> Double
mean pmf = foldl (\acc,(hypo,prob) => acc + (cast hypo)*prob) 0.0 (toList pmf)

normalize : (Ord a) => PMF a -> PMF a
normalize a = let 
    factor = 1.0 / tot a
  in
    (* factor) <$> a


[pmf] Show a => Show (PMF a) where
  show = show . Dict.toList
