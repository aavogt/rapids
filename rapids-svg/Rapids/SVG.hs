{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Rapids.SVG where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import SvgTreeAntiquote (parsePathCommands)
import Waterfall.SVG (convertPathCommands)

svg =
  QuasiQuoter
    { quoteExp = \str ->
        [| either (error . show) id $ convertPathCommands $(parsePathCommands str)|]
    }
