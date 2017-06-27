module ThinkBayes.PMF

import Data.AVL.Dict
import ThinkBayes.Util

%access export

public export
PMF : Type -> Type
PMF a = Dict a Double

[pmf] Show a => Show (PMF a) where
  show = show . Dict.toList

get : (Ord a) => a -> PMF a -> Maybe Double
get = lookup 

domain : PMF a -> List a
domain = keys

mult : (Ord a) => a -> Double -> PMF a -> PMF a
mult a f = update a (*f)

tot : PMF a -> Double
tot = sum . values 

mean : (Num a, Cast a Double) => PMF a -> Double
mean pmf = foldl 
             (\acc,(hypo,prob) => acc + (cast hypo)*prob) 
             0.0 
             (toList pmf)

percentile : PMF a -> Int -> Maybe a
percentile pmf pcnt = let 
    p = (cast pcnt) / 100.0
  in
    case toList pmf of
      hd :: tl => Just $ fst $ foldl 
        (\(h, tot),(hypo,prob) => 
          if tot >= p then (h, tot) 
                      else (hypo, tot + prob)
        ) 
        hd tl
      [] => Nothing

normalize : (Ord a) => PMF a -> PMF a
normalize a = let 
    factor = 1.0 / tot a
  in
    (* factor) <$> a

uniform : (Ord a) => List a -> PMF a
uniform xs = 
  let s = length xs in
    foldl (\d,v => insert v (1.0/cast s) d) empty xs

power : (Ord a, Cast a Double) => List a -> Double -> PMF a
power xs alpha = 
  let init = foldl (\d,v => insert v (pow (cast v) (-alpha)) d) empty xs 
   in
     normalize init