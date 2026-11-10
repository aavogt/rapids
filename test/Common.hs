{-# LANGUAGE BlockArguments, TemplateHaskell #-}
module Common (module Rapids, module Common) where
import Rapids
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import Control.Monad.IO.Class
import Data.List
import Text.Printf
bowtie = revolution (pi/4) profile

profile = pathFrom 0 [
  r a,
  u b,
  l (2*a),
  d b
  ] & closeLoop2D

a = 1
b = 0.5
r x = lineRelative2D (V2 x 0)
l x = r (-x)
u y = lineRelative2D (V2 0 y)
d y = u (-y)

stripSuffix suff x = reverse <$> stripPrefix (reverse suff) (reverse x)

stepName :: ExpQ
stepName = do
  Loc {loc_filename} <- location
  Just sourceNameBase <- return $ stripSuffix ".hs" loc_filename
  lift $ sourceNameBase ++ ".step"

stepNameI :: ExpQ
stepNameI = do
  Loc {loc_filename} <- location
  Just sourceNameBase <- return $ stripSuffix ".hs" loc_filename
  [| \i -> $(lift sourceNameBase) ++ printf "%02d" (i :: Int)  ++ ".step" |]

writeAction :: Q [Dec]
writeAction = [d|
  writeModel solid = do
    colorAttrsMapReset
    writeSTEPColor $stepName solid
    print (checkSolid solid)
  |]

writesAction :: Q [Dec]
writesAction = [d|
  writeModels solids = do
    colorAttrsMapReset
    writeSTEPColor $stepName $ foldr1 (stacked ex) $ intersperse unitGap solids
    sequence_ $ zipWith (writeSTEPColor . $stepNameI) [0 .. ] solids
    print (map checkSolid solids)
  |]

write :: ExpQ -> Q [Dec]
write solid = [d|
  main = do
    colorAttrsMapReset
    writeSTEPColor $stepName $solid
    print (checkSolid $solid)
  |]

writes :: ExpQ -> Q [Dec]
writes solids = [d|
  main = do
    colorAttrsMapReset
    writeSTEPColor $stepName $ foldr1 (stacked ex) $ intersperse unitGap $solids
    sequence_ $ zipWith (writeSTEPColor . $stepNameI) [0 .. ] $solids
    print (map checkSolid $solids)
  |]
