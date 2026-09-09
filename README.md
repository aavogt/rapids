# rapids

Wrapper for [joe-warren/opencascade-hs/waterfall-cad](https://github.com/joe-warren/opencascade-hs), where I add missing operations and other quality-of-life features:

  - [named colors including](https://github.com/aavogt/rapids/blob/main/lib/Rapids/Color.hs#L456) `$red :: Solid -> Solid` also add source locations and propagate through most 3d operations to `mkStepWriterColor :: IO (Solid -> IO FilePath)` for [aavogt/OCCT_XCAF_FacePicker](https://github.com/aavogt/OCCT_XCAF_FacePicker)
  - `pad` generalizes Waterfall.prism turning any `shape` into a `Solid` with optional taperFrac :
    - `pad <x y> z <taperFrac> shape`
    - `pad v3 <taperFrac>`
  - `sweep path shape`
  - `revolution <radians> shape`
  - `unitSpiral turns <taperSlope> shape`
  - meshed convex hull of vertices contained within `Solid`, `[V3 Double]` `[Path]` or `Path`. In other words:
    - `hull :: Solid       -> Solid`
    - `hull :: [V3 Double] -> Solid`
    - `hull :: [Path]      -> Solid`
    - `hull :: Path        -> Solid`
  - Num instances for Path, Solid, and Shape so that + - * are short for [union, difference, intersection](https://hackage-content.haskell.org/package/waterfall-cad-0.6.2.1/docs/Waterfall-Booleans.html)
  - transformations are varargs. Depending on types, 1 to 4 arguments specify a transformation, and multiple transformations can be done with one function. So `rotate ex x . rotate ey y` can be `rotate ex x ey y`.
    - `translate` `rotate` `rotateDeg` `scale` `mirror`  return the changed solid
    - `translated` `rotated` `rotatedDeg` `scaled` `mirrored`  also union the original (ie. `mirrored ... s = s + mirror ... s`)
    - `_translated` `_rotated` `_rotatedDeg` `_scaled` `_mirrored` produce `Iso' Solid Solid`
    - example expressions of type `Solid -> Solid`, where I each group of arguments (transformation) on a single line:
```
        translate
            x y z
            v3
            ex x
            ey y
            ez z
        mirror
            v3
            x y z
            ex
            ey
            ez
        rotate
          x y z radians
          v3    radians
          q
          ex    radians
        scale
          v3
          xyz
          x y z
          xy z
          ex x
          ey y
```
  - `axisAlignedBoundingBox` arrangements covering many of the cases done in [Inkscape's Align and Distribute](https://inkscape-manuals.readthedocs.io/en/latest/align-and-distribute.html)
    - `stack, center, left, right :: E V3 -> Solid -> Solid -> Solid` translates the second Solid. That is:
        - `stack ez a b == translate ez z b` where `z` makes the bottom of `b` coplanar with the top of `a`.
        - `center ez a b == translate (V3 x y 0) b` where `x` `y` make the centers of the z-faces collinear.
        - `left ex a b = translate ex x b` where `x` makes the left (lower x coordinate) faces coplanar
        - `right ex a b = translate ex x b` where `x` makes the right (higher x coordinate) faces coplanar
    - `stacked centered lefted righted` also union the first argument continuing the analogy with `mirrored` above. The `unitCube` could be arranged relative to the `sphere` in different axes with `(flip (stacked ez) =<< flip (left ey) =<< flip (center ex) unitCube) sphere` (using `instance Monad (r ->)`), but flip is noisy, inlining flip will violates the convention that  `y' = f x y` is better than `y' = g y x` (`g = flip f`) for use with `&` `$` or `.`.
    - `[sa,sb,sc] = distribute ez [s1,s2,s3]`, `sa` is the lowest, `sb` is the middle, `sc` is the highest the middle elements translated along z for equal gaps/overlap. TODO or (optionally) restore the old ordering `[s1',s2',s3']`
  - `section :: Solid -> [Path2D]` slice the solid with XY plane
  - Rapids.Path lets you use do notation to construct paths for example [loophv](https://gist.github.com/aavogt/1b59c0d02c5bcc129d743042b99839f9#file-main-hs-L39) or [do notation for paths](http://github.com/aavogt/rapids/blob/main/test/lib/Solids.hs)
  - variables in the above example expressions
    - `ex, ey, ez :: E V3` [reexported from linear](https://hackage-content.haskell.org/package/linear-1.23.3/docs/Linear-V3.html#v:ex)
    - `q :: Quaternion Double` [reexported from linear](https://hackage-content.haskell.org/package/linear-1.23.3/docs/Linear-Quaternion.html#t:Quaternion)
    - `x, y, z, xy, xyz, taperFrac, taperSlope, radians :: Double`, `v3 :: V3 Double`, angle brackets (`<x>`) mean the argument(s) are optional
    - `path :: ToPath p => p` is a `[V2 Double]`, `[V3 Double]`, `Path2D` or `Path`
    - `shape :: ToShape s => s` is a `[V2 Double]`, `Path2D`,  `Shape` or `Path` or lists of them

## viewers
[aavogt/OCCT_XCAF_FacePicker](https://github.com/aavogt/OCCT_XCAF_FacePicker).

Previously I used f3d which only displays colors with the following configuration in `/etc/f3d/config.json` or [elsewhere](https://f3d.app/docs/user/CONFIGURATION_FILE/#locations):
```json
[{
  "match": ".*(step|stp|iges|igs|brep|xbf)",
  "options": {
    "scalar-coloring": true,
    "load-plugins": "occt",
    "coloring-component": "-2",
    "coloring-by-cells": true,
    "watch": true
  }
}]
```
 
## examples

[hose barb union](https://gist.github.com/aavogt/6efaca22c6496ab21e6014f1c63a5a9b#file-main-hs)

### loading step file, vertex convex hull

![hull](http://aavogt.github.io/blog/images/hull.png)

### ini file

![](https://aavogt.github.io/blog/images/workflow_small_color.png)
[square base flange](https://github.com/aavogt/battery-adapter/blob/main/main.hs) or as a [video](https://youtu.be/NTni_7p9clE)
or [blog post](https://aavogt.github.io/blog/posts/2025-12-30-waterfall-cad-gcodeviewer.html)

## testing

cd test && make
cd test && cabal run statistics
