module Main

import Control.Monad.State

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
          |> updatePMF @{monty} 'B'
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
     |> updatePMF @{mandm} ("bag1", "yellow")
     |> updatePMF @{mandm} ("bag2", "green")
     |> show @{pmf}

public export
HypoState : Type -> Type -> Type -> Type
HypoState hypoType dataType = State (Dict hypoType (Dict dataType Double))

public export
interface (Ord hypoType, Ord dataType) => StateSuite hypoType dataType where
  likelihoodS : dataType -> hypoType -> HypoState hypoType dataType Double
  updFreq : Double -> Double
  updatePMFS : dataType -> PMF hypoType -> HypoState hypoType dataType (PMF hypoType)
  updatePMFS dat pmf = let 
    ld = likelihoodS dat
    updated = foldM 
      (\p,hypo => (\lh => PMF.mult hypo lh p) <$> ld hypo) 
      pmf 
      (domain pmf)
   in 
    normalize <$> updated    

-- bugs out if put into the interface, see https://github.com/idris-lang/Idris-dev/issues/3858
updState : StateSuite hypoType dataType => hypoType -> dataType -> HypoState hypoType dataType ()
updState {hypoType} {dataType} h d = modify (Dict.update h (Dict.update d (updFreq {hypoType} {dataType})))

StateSuite String String where
  likelihoodS d h = do
    hs <- get 
    let bh = fromMaybe (fromList []) $ Dict.lookup h hs
    let dat = fromMaybe (-1.0) $ Dict.lookup d bh
    pure $ dat / (sum $ Dict.values bh)
  updFreq x = x - 1.0

cookieEx : String
cookieEx = evalState (do
    hs <- get 
    let strhs = show hs
    let pmf1 = pmfFromList ["Bowl1", "Bowl2"]
    pmf2 <- updatePMFS "vanilla" pmf1
    let str2 = show @{pmf} pmf2
    updState "Bowl1" "vanilla"
    hs2 <- get 
    let strhs2 = show hs2
    pmf3 <- updatePMFS "vanilla" pmf1
    let str3 = show @{pmf} pmf3
    pure (strhs ++ "\n" ++ str2 ++ "\n\n" ++ strhs2 ++ "\n" ++ str3)
    ) (fromList [("Bowl1", fromList [("vanilla", 30.0), ("chocolate", 10.0)])
                ,("Bowl2", fromList [("vanilla", 20.0), ("chocolate", 20.0)])
                ])

main : IO ()
main = putStrLn cookieEx