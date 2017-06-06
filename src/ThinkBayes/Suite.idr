module ThinkBayes.Suite

import ThinkBayes.PMF

%access export

public export
interface Ord hypoType => Suite hypoType dataType where
  likelihood : dataType -> hypoType -> Double
  updateS : dataType -> PMF hypoType -> PMF hypoType
  updateS dat pmf = let 
    ld = likelihood dat
    updated = foldl 
      (\p,hypo => PMF.mult hypo (ld hypo) p) 
      pmf 
      (domain pmf)
   in 
    normalize updated