# rapids

simplify waterfall-cad expressions

  - `instance Num Solid` for `(+),(-),(*) :: Solid -> Solid -> Solid` [union, difference, intersection](https://hackage-content.haskell.org/package/waterfall-cad-0.6.2.1/docs/Waterfall-Booleans.html)
  - translate rotate rotateDeg scale mirror and mirrored convert arguments 
  - `R.translate ey 1 == W.translate (V3 0 1 0)` here [ex ey ez from linear](https://hackage-content.haskell.org/package/linear-1.23.3/docs/Linear-V3.html#v:ex) decides the direction
  - three doubles are packed `R.translate x y z == W.translate (V3 x y z)`
  - V3 double is unchanged `R.translate xyz = W.translate xyz`
  - R.mirrored unions the original like Freecad's PartDesign::Mirrored
  - Rapids.Path lets you use do notation to construct paths for example [loophv](https://gist.github.com/aavogt/1b59c0d02c5bcc129d743042b99839f9#file-main-hs-L39)

unfortunately the overloading complicates type errors, and my decision to reuse
  waterfall-cad's names at different types may not always make things easier
 
## examples

[color T-bracket](https://gist.github.com/aavogt/1b59c0d02c5bcc129d743042b99839f9#file-main-hs)

[square base flange](https://github.com/aavogt/battery-adapter/blob/main/main.hs)

[hose barb union](https://gist.github.com/aavogt/6efaca22c6496ab21e6014f1c63a5a9b#file-main-hs)
