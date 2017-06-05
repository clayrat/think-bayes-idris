module Main

import ThinkBayes.PMF

main : IO ()
main = let 
      x = pmfFromList ["Bowl 1", "Bowl 2"]
      y = mult "Bowl 1" 0.75 x
      z = mult "Bowl 2" 0.5 y
      n = normalize z
    in 
      printLn $ maybe "oops" show $ get "Bowl 1" n
      