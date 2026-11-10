{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import Common

$(writeAction)

radius = 10

main = do
  let solid = pad 2 roundedRectangle
  print (volume solid)
  writeModel solid

roundedRectangle :: Path
roundedRectangle = foldMap toPath [svg| h 100 v 100 H 0 Z |] & fillet radius
