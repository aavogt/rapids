import Rapids

main = return ()

sectionPaths :: Solid -> [Path]
sectionPaths = map toPath . section

emptyPaths2D :: Shape -> [Path2D]
emptyPaths2D _ = []

xyToYz :: Iso' Solid Solid
xyToYz = _rotated' ey (pi / 2) . _rotated' ex (-pi / 2)

rotatedPaths :: Solid -> [Path]
rotatedPaths = over (_rotated ez 1) sectionPaths

rotateSection :: [Path]
rotateSection = unitCube & _rotated ex 3 ey 1 %~ sectionPaths

rotatedPathsInfix :: Solid -> [Path]
rotatedPathsInfix solid = solid & _rotated ez 1 %~ sectionPaths

translatedPaths :: Solid -> [Path]
translatedPaths = over (_translated ez 1) sectionPaths

translatedSection :: [Path]
translatedSection = unitCube & _translated ex 1 ey 2 %~ sectionPaths

mirroredPaths :: Solid -> [Path]
mirroredPaths = over (_mirrored ez ex) sectionPaths

scaledPaths :: [Path]
scaledPaths = unitSphere & _scaled ez 2 ey 3 %~ sectionPaths

translatedPaths2D :: Shape -> [Path2D]
translatedPaths2D = over (_translated2D ex 1 ey 2) emptyPaths2D

scaledPaths2D :: Shape -> [Path2D]
scaledPaths2D = over (_scaled2D ex 2 ey 3) emptyPaths2D

rotatedPrime :: Solid -> Solid
rotatedPrime = over (_rotated' ez 1) id

translatedPrime :: Solid -> Solid
translatedPrime = over (_translated' ez 1) id

mirroredPrime :: Solid -> Solid
mirroredPrime = over (_mirrored' ez) id

scaledPrime :: Solid -> Solid
scaledPrime = over (_scaled' ez 2) id

translatedPrime2D :: Shape -> Shape
translatedPrime2D = over (_translated2D' ex 1) id

scaledPrime2D :: Shape -> Shape
scaledPrime2D = over (_scaled2D' ex 2) id
