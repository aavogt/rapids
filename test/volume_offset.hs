import Rapids
import Solids

main = do
  print [(x, volume (offset x bowtie)) | x <- [0.01, 0.1, 0.5, 1, 2]]
