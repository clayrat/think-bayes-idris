module Main

import Data.AVL.Dict

import ThinkBayes.PMF
import ThinkBayes.Suite

[prop] Suite Int Int where
    likelihood dat hypo = 
      if hypo < dat
        then 0.0
        else 1.0/(cast hypo)

dice : String
dice = let 
  p = pmfFromList [4,6,8,12,20]
  u  = updateS @{prop} 6 p
  u0 = updateS @{prop} 6 u
  u1 = updateS @{prop} 8 u0
  u2 = updateS @{prop} 7 u1
  u3 = updateS @{prop} 7 u2
  u4 = updateS @{prop} 5 u3
  u5 = updateS @{prop} 4 u4
 in 
  show @{pmf} u5

trainsUni : String
trainsUni = let
  p = pmfFromList [1..1000]
  u = updateS @{prop} 60 p
 in  
  show $ mean u

main : IO ()
main = printLn trainsUni