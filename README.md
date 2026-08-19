# rapids

Wrapper for [joe-warren/opencascade-hs/waterfall-cad](https://github.com/joe-warren/opencascade-hs), where I add missing operations and other quality-of-life features:

  - [named colors including](https://github.com/aavogt/rapids/blob/main/lib/Rapids/Color.hs#L456) `$red :: Solid -> Solid` propagate colors and source locations per-face through + - * to `mkStepWriterColor :: IO (Solid -> IO FilePath)` for display by [aavogt/OCCT_XCAF_FacePicker](https://github.com/aavogt/OCCT_XCAF_FacePicker)
  - `section :: Solid -> [Path2D]` slice the solid with XY plane
  - `revolution :: Path2D -> Solid` `sector :: Double -> Path2D -> Solid`
  - meshed convex hull of vertices contained within `Solid`, `[V3 Double]` `[Path]` or `Path`. In other words:
    - `hull :: Solid       -> Solid`
    - `hull :: [V3 Double] -> Solid`
    - `hull :: [Path]      -> Solid`
    - `hull :: Path        -> Solid`
  - `instance Num Solid` for `(+),(-),(*) :: Solid -> Solid -> Solid` for [union, difference, intersection](https://hackage-content.haskell.org/package/waterfall-cad-0.6.2.1/docs/Waterfall-Booleans.html)
  - `translate` `rotate` `rotateDeg` `scale` `mirror` and `mirrored` convert arguments according to:
    - `R.translate ey 1 == W.translate (V3 0 1 0)` here [ex ey ez from linear](https://hackage-content.haskell.org/package/linear-1.23.3/docs/Linear-V3.html#v:ex) decides the direction
    - three doubles are packed `R.translate x y z == W.translate (V3 x y z)`
    - V3 double is unchanged `R.translate xyz = W.translate xyz`
    - `scale xy z` short for `scale xy xy z`
  - R.mirrored unions the original like Freecad's PartDesign::Mirrored
  - Rapids.Path lets you use do notation to construct paths for example [loophv](https://gist.github.com/aavogt/1b59c0d02c5bcc129d743042b99839f9#file-main-hs-L39)

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
