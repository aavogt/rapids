import Rapids
main = return ()
xyToYz :: Iso' Solid Solid
xyToYz = _rotated ey (pi/2) . _rotated ex (-pi/2)
