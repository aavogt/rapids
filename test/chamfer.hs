{-# LANGUAGE CPP, TemplateHaskell #-}
import Rapids

cutBoth :: (Solid -> Solid) -> (Solid -> Solid) -> E V3 -> Solid
cutBoth a b ax = a (translate ax (-1) centeredCube) + b (translate ax 1 centeredCube)

solved = (scale 5 $ centeredCube - cutBoth $green $yellow ez - cutBoth $blue $purple ey - cutBoth $pink $black ex) - $red (scale 1 5 centeredCylinder)

main =
  writeSTEPColor (takeWhile (/= '.') __FILE__ ++ ".step") $ fillet (7/10) solved
