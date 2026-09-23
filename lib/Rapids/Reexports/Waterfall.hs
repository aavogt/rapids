module Rapids.Reexports.Waterfall (
    module Waterfall,
    module Waterfall.Internal.NearZero
    ) where


import Waterfall.Internal.NearZero (nearZero)
import Waterfall hiding
  (
    axisAlignedBoundingBox,
    centerOfMass,
    difference,
    intersection,
    intersections,
    mirror,
    momentOfInertia,
    offset,
    offsetWithTolerance,
    revolution,
    rotate,
    scale,
    scale2D,
    splice2D,
    splice3D,
    splitPath2D,
    splitPath3D,
    sweep,
    translate,
    translate2D,
    tryOffset,
    tryOffsetWithTolerance,
    union,
    unions,
    volume,
    _mirrored,
    _rotated,
    _scaled,
    _scaled2D,
    _translated,
    _translated2D,
    fillet,
    chamfer,
  )
