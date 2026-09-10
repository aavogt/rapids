import Rapids
main = do
  writeSTEP "sweep.step" $ 
    axisTriad
    + sweepRuled f (rectangle 1 3)
    + translate ey 10 (sweep (f 0) (rectangle 1 3))
    + translate ex 10 (sweep (f 0) (rectangle 3 3))

f (V2 x y) = [V3 (x + th * sin th) (y + th * cos th) th | th <- [0, 0.5 .. 6]]

beziers :: [V3 Double] -> Path
beziers = foldMap (\[a,b,c,d] -> bezier a b c d) . chunksOf 4 

chunksOf _ [] = []
chunksOf n xs = let (a,b) = splitAt n xs
  in a : chunksOf n xs
