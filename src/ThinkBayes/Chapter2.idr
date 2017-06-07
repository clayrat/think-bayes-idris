module Main

import Data.AVL.Dict

import ThinkBayes.PMF
import ThinkBayes.Suite
import ThinkBayes.Util

cookies : String
cookies = pmfFromList ["Bowl 1", "Bowl 2"]
       |> mult "Bowl 1" 0.75
       |> mult "Bowl 2" 0.5
       |> normalize
       |> get "Bowl 1"
       |> maybe "oops" show

[monty] Suite Char Char where
  likelihood dat hypo = 
    if hypo == dat 
      then 0.0
      else 
        if hypo == 'A' 
          then 0.5
          else 1.0

montySuite : String
montySuite = pmfFromList (unpack "ABC")
          |> updateS @{monty} 'B'
          |> show @{pmf}

[mandm] Suite Char (String, String) where
  likelihood dat hypo = let
    (bag, color) = dat
    like = Dict.lookup hypo hypotheses >>= Dict.lookup bag >>= Dict.lookup color
   in 
    cast $ fromMaybe (-1) like
  where 
    Mix : Type
    Mix = Dict String Int
    mix94 : Mix
    mix94 = fromList [("brown",30),("yellow",20),("red",20)
                     ,("green",10),("orange",10),("tan",10)
                     ,("blue", 0)]
    mix96 : Mix
    mix96 = fromList [("blue",  24),("green",20),("orange",16)
                     ,("yellow",14),("red",  13),("brown", 13)
                     ,("tan",   0)]
    Hypo : Type
    Hypo = Dict String Mix
    hypoA : Hypo
    hypoA = fromList [("bag1", mix94), ("bag2", mix96)]
    hypoB : Hypo
    hypoB = fromList [("bag1", mix96), ("bag2", mix94)]
    hypotheses : Dict Char Hypo
    hypotheses = fromList [('A', hypoA), ('B', hypoB)]

mAndM : String
mAndM = pmfFromList (unpack "AB")
     |> updateS @{mandm} ("bag1", "yellow")
     |> updateS @{mandm} ("bag2", "green")
     |> show @{pmf}

cookieEx : String
cookieEx = ?cookie

main : IO ()
main = printLn mAndM