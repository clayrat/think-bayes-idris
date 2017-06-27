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
dice = uniform [4,6,8,12,20]
    |> updatePMF @{prop} 6
    |> updatePMF @{prop} 6 
    |> updatePMF @{prop} 8 
    |> updatePMF @{prop} 7 
    |> updatePMF @{prop} 7 
    |> updatePMF @{prop} 5 
    |> updatePMF @{prop} 4 
    |> show @{pmf}

trainsUni : String
trainsUni = uniform [1..1000]
         |> updatePMF @{prop} 60
         |> mean
         |> show 

trainsPow : String
trainsPow = show $ f <$> [500, 1000, 2000]
  where 
    f : Int -> String
    f x = PMF.power [1..x] 1.0
       |> updatePMF @{prop} 30
       |> updatePMF @{prop} 60
       |> updatePMF @{prop} 90       
       |> mean
       |> show

trainsInterval : String
trainsInterval = PMF.power [1..2000] 1.0
       |> updatePMF @{prop} 30
       |> updatePMF @{prop} 60
       |> updatePMF @{prop} 90       
       |> (\p => (percentile p 5, percentile p 95))
       |> show

main : IO ()
main = printLn trainsInterval