module Main

import Data.AVL.Dict
import Data.SortedMap    -- bug? if removed throws `No such variable Data.SortedMap.SortedMap` @ trainsIntervalCDF

import ThinkBayes.PMF
import ThinkBayes.CDF
import ThinkBayes.Suite
import ThinkBayes.Util

[prop] Suite Int Int where
    likelihood dat hypo = 
      if hypo < dat
        then 0.0
        else 1.0/(cast hypo)

dice : String
dice = let
    dicePMF = uniform [4,6,8,12,20]
    obs = [6,6,8,7,7,5,4]
  in
    foldl (flip $ updatePMF @{prop}) dicePMF obs 
 |> show @{pmf} 

trainsUni : String
trainsUni = uniform [1..1000]
         |> updatePMF @{prop} 60
         |> mean
         |> show 

threeTrains : Int -> PMF Int         
threeTrains x = let
    trainPMF = PMF.power [1..x] 1.0
    obs = [30,60,90]
  in
    foldl (flip $ updatePMF @{prop}) trainPMF obs 

trainsPow : String
trainsPow = show $ (show . mean . threeTrains) <$> [500, 1000, 2000]

trainsInterval : String
trainsInterval = 
    threeTrains 2000     
 |> (\p => (percentile p 5, percentile p 95))
 |> show

trainsIntervalCDF : String
trainsIntervalCDF = 
    threeTrains 2000  
 |> fromPMF 
 |> (\c => (percentile c 5, percentile c 95))
 |> show

-- https://stats.stackexchange.com/questions/70096/locomotive-problem-with-various-size-companies
-- not dependent on hypothesis
[manyComp] Suite Int Int where
  likelihood dat _ = let 
    ns = [10, 100, 1000, 10000]
    tot = sum ns
    withNum = ns |> filter (>=dat) |> length
  in
    cast withNum / cast tot

threeMany : String     
threeMany = let
    trainPMF = PMF.power [1..2000] 1.0
    obs = [30,60,90]
  in
    foldl (flip $ updatePMF @{manyComp}) trainPMF obs  
 |> mean
 |> show 
    
main : IO ()
main = printLn threeMany