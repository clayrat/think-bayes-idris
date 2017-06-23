module Main

import Control.Monad.State

import Data.AVL.Dict

import ThinkBayes.PMF
import ThinkBayes.Suite
import ThinkBayes.Util

cookies : String
cookies = uniform ["Bowl 1", "Bowl 2"]
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
montySuite = uniform (unpack "ABC")
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
mAndM = uniform (unpack "AB")
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
    updated = foldlM 
      (\p,hypo => (\lh => PMF.mult hypo lh p) <$> ld hypo) 
      pmf 
      (domain pmf)
   in 
    normalize <$> updated    

-- bugs out if put into the interface, see https://github.com/idris-lang/Idris-dev/issues/3858
updState : StateSuite hypoType dataType => hypoType -> dataType -> HypoState hypoType dataType ()
updState {hypoType} {dataType} h d = modify (Dict.update h (Dict.update d (updFreq {hypoType} {dataType})))

-- bugs out if given a name, maybe some interference from State?
StateSuite String String where
  likelihoodS d h = do
    hs <- get 
    let bh = fromMaybe (fromList []) $ Dict.lookup h hs
    let dat = fromMaybe (-1.0) $ Dict.lookup d bh
    pure $ dat / (sum $ Dict.values bh)
  updFreq x = x - 1.0

cookieEx : String
cookieEx = evalState doCookies $ 
  fromList [("Bowl1", fromList [("vanilla", 30.0), ("chocolate", 10.0)])
           ,("Bowl2", fromList [("vanilla", 20.0), ("chocolate", 20.0)])
           ]
  where 
    doCookies : HypoState String String String
    doCookies = do
      hs <- get 
      let pmf1 = uniform ["Bowl1", "Bowl2"]
      pmf2 <- updatePMFS "vanilla" pmf1
      updState "Bowl1" "vanilla"
      hs2 <- get 
      pmf3 <- updatePMFS "vanilla" pmf1
      pure $ show hs ++ "\n" ++ show @{pmf} pmf2 ++ "\n\n" ++  show hs2 ++ "\n" ++ show @{pmf} pmf3
    
main : IO ()
main = putStrLn cookieEx