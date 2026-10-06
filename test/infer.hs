{-# LANGUAGE FlexibleContexts #-}
import Rapids

main = return ()

sectionPaths = map toPath . shapePaths . section

emptyPaths2D = [] :: [Path2D]

xyToYz = emptySolid & _rotated' ey (pi / 2) ex (-pi / 2) %~ id

rotatedPaths = over (_rotated ez 1) sectionPaths emptySolid

rotateSection = unitCube & _rotated ex 3 ey 1 %~ sectionPaths

rotatedPathsInfix solid = solid & _rotated ez 1 %~ sectionPaths

translatedPaths = over (_translated ez 1) sectionPaths

translatedSection = unitCube & _translated ex 1 ey 2 %~ sectionPaths

mirroredPaths = over (_mirrored ez ex) sectionPaths

scaledPaths = unitSphere & _scaled ez 2 ey 3 %~ sectionPaths

translatedPaths2D = over (_translated2D ex 1 ey 2) id emptyShape

scaledPaths2D = over (_scaled2D ex 2 ey 3) id emptyPaths2D

rotatedPrime = over (_rotated' ez 1) id emptySolid

translatedPrime = over (_translated' ez 1) id emptySolid

mirroredPrime = over (_mirrored' ez) id emptySolid

scaledPrime = over (_scaled' ez 2) id emptySolid

translatedPrime2D = over (_translated2D' ex 1) id emptyPaths2D

scaledPrime2D = over (_scaled2D' ex 2) id emptyShape
