{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Rapids.SVG where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import SvgTreeAntiquote (parsePathCommandsSupplement)
import SvgTreeOffset (pathOutline)
import Waterfall.SVG (convertPathCommands)


svgTol tol =
  QuasiQuoter
    { quoteExp = \str ->
        [| either (error . show) id $ convertPathCommands $ pathOutline tol $(parsePathCommandsSupplement str)|]
    }

svg = svgTol (0.1:: Double)
