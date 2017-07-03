module ThinkBayes.CDF

import Data.AVL.Dict
import Data.SortedMap
import ThinkBayes.PMF

%access export

public export
CDF : Type -> Type
CDF a = SortedMap Int (a, Double)

Show a => Show (CDF a) where
  show = show . toList

fromPMF : PMF a -> CDF a
fromPMF pmf = let
    summed = scanl1
      (\(_,runSum),(a,prob) => (a,runSum+prob))
      (Dict.toList pmf)
  in
    fromList $ List.zip [0..((cast $ length summed)-1)] summed

len : CDF a -> Int
len cdf = cast $ List.length $ SortedMap.toList cdf

private partial 
lookup : CDF a -> Int -> (a, Double)
lookup cdf k = unwrap $ SortedMap.lookup k cdf
  where
  unwrap : Maybe x -> x
  unwrap (Just x) = x

private
searchBy : Ord v => (cdf : CDF a) -> (target : v) -> (f : (a, Double) -> v) -> Either (a, Double) Int
searchBy cdf target f = bs target 0 clen
  where 
    clen : Int 
    clen = len cdf
    bs : (target : v) -> (start : Int) -> (end : Int) -> Either (a, Double) Int
    bs target start end = 
      if start == end 
        then
          if start == clen || f (cdf `lookup` start) > target
            then Right start
            else Right $ start + 1
        else let
          mid = (end + start) `div` 2
          midVal = f $ cdf `lookup` mid
        in
            if midVal == target
              then Left $ cdf `lookup` mid 
              else 
                if midVal > target
                then bs target start mid
                else bs target (mid + 1) end

prob : (Ord a) => CDF a -> a -> Double
prob cdf key = case searchBy cdf key fst of
  Left (_, p) => p
  Right nextIdx => if nextIdx == 0 
                   then 0.0
                   else snd $ cdf `lookup` (nextIdx - 1) 

value : CDF a -> Double -> a
value cdf prob = let 
    clen = len cdf 
  in case searchBy cdf prob snd of
  Left (key, _) => key
  Right nextIdx => fst $ cdf `lookup` (if nextIdx == clen then clen-1 else nextIdx)

percentile : CDF a -> Int -> a    
percentile cdf p = value cdf (cast p / 100.0)
               
      



