{-# LANGUAGE CPP, TemplateHaskell #-}
import Rapids

cutBoth :: (Solid -> Solid) -> (Solid -> Solid) -> E V3 -> Solid
cutBoth a b ax = a (translate ax (-1) centeredCube) + b (translate ax 1 centeredCube)

solved = 
    centeredCube
      - cutBoth $gray $yellow ez
      - cutBoth $red $green ex
      - cutBoth $blue $purple ey

main =
  writeSTEPColor (takeWhile (/= '.') __FILE__ ++ ".step") $ fillet 0.2 solved
