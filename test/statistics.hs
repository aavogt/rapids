import Rapids
import Solids

main = do
  print (volume bowtie)
  print (centerOfMass bowtie)
  print (momentOfInertiaMatrix bowtie)
  print (principalMomentsOfInertia bowtie)
  print (momentOfInertiaMatrix unitCylinder)
  print (momentOfInertiaMatrix (translate ex 10 unitCylinder))
  print (principalMomentsOfInertia (translate ex 10 unitCylinder))
  print (axisAlignedBoundingBox (offset 2 bowtie))

