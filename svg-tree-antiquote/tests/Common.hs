module Common (write, module Rapids, module Rapids.SVG) where

import Language.Haskell.TH
import Rapids
import Rapids.SVG

stepName :: ExpQ
stepName = do
  Loc {loc_filename} <- location
  let sourceName = reverse (takeWhile (/= '/') (reverse loc_filename))
  [| "out/" ++ takeWhile (/= '.') $(litE (stringL sourceName)) ++ ".step" |]

write :: ExpQ -> ExpQ -> Q [Dec]
write solid differences = [d|
  main = do
    writeSTEPColor $stepName (unions $solid)
    print (map checkSolid $solid)
    print (map volume $differences)
  |]
