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
dice = pmfFromList [4,6,8,12,20]
    |> updateS @{prop} 6
    |> updateS @{prop} 6 
    |> updateS @{prop} 8 
    |> updateS @{prop} 7 
    |> updateS @{prop} 7 
    |> updateS @{prop} 5 
    |> updateS @{prop} 4 
    |> show @{pmf}

trainsUni : String
trainsUni = pmfFromList [1..1000]
         |> updateS @{prop} 60
         |> mean
         |> show 

main : IO ()
main = printLn trainsUni