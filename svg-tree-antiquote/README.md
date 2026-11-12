# svg-tree-antiquote

used by ../rapids-svg https://github.com/aavogt/rapids/tree/main/rapids-svg

```haskell
-- | Parse svg path commands into svg-tree's 'PathCommand',
-- supporting arithmetic @+ - * / ** ( )@ with haskell-like fixity
--
-- ghci> let x = 7; y = 3 in $(parsePathCommands "v -x**2 h y")
-- [VerticalTo OriginRelative [-49.0],HorizontalTo OriginRelative [3.0]]
parsePathCommands :: String -- ^ https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#path_commands
  -> ExpQ -- ^ ['PathCommand']
```

parse [svg path commands](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#path_commands) into [svg-tree](https://hackage-content.haskell.org/package/svg-tree-0.6.2.4/docs/Graphics-Svg-Types.html#t:PathCommand)

## Updating Expressions

`parsePathCommandsSupplementCmd` works with `renderCmdsWithDiff` to update expressions:

```haskell
ghci> import Text.Show.Pretty; import Control.Lens; import SvgTreeAntiquote
ghci> let cm = let x = 1; y = 3 in $(parsePathCommandsSupplementCmd "M x,y h 2 v 2 H x Z")
ghci> pPrint cm
[ ( CM [ MArg (EVar (Ident "x") []) (EVar (Ident "y") []) ]
  , Right (MoveTo OriginAbsolute [ V2 1.0 3.0 ])
  )
, ( Ch [ HArg (EInt 2) ]
  , Right (HorizontalTo OriginRelative [ 2.0 ])
  )
, ( Cv [ HArg (EInt 2) ]
  , Right (VerticalTo OriginRelative [ 2.0 ])
  )
, ( CH [ HArg (EVar (Ident "x") []) ]
  , Right (HorizontalTo OriginAbsolute [ 1.0 ])
  )
, ( CZ , Right EndPath )
]
ghci> let edited = cm <&> snd <&> _Right . _HorizontalTo . _2 . traversed *~ 3
ghci> renderCmdsWithDiff cm edited & fromJust & pPrint
M x,y
h 6
v 2
H x+2
z
```

## Extensions

As an extension, parsePathCommandsSupplement/pathOutline parse and interpret `wWjJ{}[]`-commands. These represent:

### offset/width

`W a b,c d,e f
L ...` sets the first vertex width to `a` the next to `b` on the left and `c` on the right
### join

TODO. Currently ends are flat (butt) and outside joins are arcs and inside joins are sharp

### branching

Commands in braces make a branch. A `F` shape is `v 1 { h 1 } v 1 h 1`

### temporary W

Square brackets make a scope for `Ww` commands. `W 1 v 1 [ W 2 h 1 ] v 1 = W 1 v 1 W 2 h 1 W 1 v 1 `
