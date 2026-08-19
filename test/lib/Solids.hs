module Solids where
import Rapids

bowtie = sector (pi/4) profile

profile = execPathState0 $ do r a; u b; l (2*a); d b; closeLoop2D

a = 1
b = 0.5

r x = lineRelative2D (V2 x 0)
l x = r (-x)
u y = lineRelative2D (V2 0 y)
d y = u (-y)
