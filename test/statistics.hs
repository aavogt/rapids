import Rapids
import Solids

main = do
  print (volume bowtie)
  print (centerOfMass bowtie)
  print (axisAlignedBoundingBox (offset 2 bowtie))

