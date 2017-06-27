module Main

import Data.AVL.Dict

import ThinkBayes.PMF
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

trainsPow : String
trainsPow = show $ go <$> [500, 1000, 2000]
  where 
    go : Int -> String
    go x = let
        trainPMF = PMF.power [1..x] 1.0
        obs = [30,60,90]
      in
        foldl (flip $ updatePMF @{prop}) trainPMF obs 
     |> mean
     |> show

trainsInterval : String
trainsInterval = let
    trainPMF = PMF.power [1..2000] 1.0
    obs = [30,60,90]
  in
    foldl (flip $ updatePMF @{prop}) trainPMF obs     
 |> (\p => (percentile p 5, percentile p 95))
 |> show

main : IO ()
main = printLn trainsInterval