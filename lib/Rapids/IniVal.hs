{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Rapids.IniVal (iniVal) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Read (readMaybe)
import Data.StateVar
import Data.IORef
import System.IO.Unsafe
import Language.Haskell.TH

{-# NOINLINE iniMapCache #-}
iniMapCache :: IORef (Maybe (Map Text Text))
iniMapCache = unsafePerformIO (newIORef mempty)

iniMap :: FilePath -> IO (Map Text Text)
iniMap cfg = do
  c0 <- get iniMapCache
  case c0 of
    Just a -> return a
    Nothing -> do
      m <- T.readFile cfg <&> parseIni . T.lines
      iniMapCache $= Just m
      return m

parseIni :: [Text] -> Map Text Text
parseIni xs = M.fromList [(T.strip a, b) | x <- xs, (a, T.uncons -> Just (_, b)) <- [T.break (== '=') x]]

lookupOneE :: String -> Q Exp
lookupOneE fieldName = do
        m <- runIO $ iniMap "config.ini"
        let str = T.unpack (m M.! T.strip (T.pack fieldName))
        fromMaybe (lift str) $
          listToMaybe $
            catMaybes
              [ lift <$> readMaybe @Int str,
                lift <$> readMaybe @Double str
              ]

tupE1 [x] = x
tupE1 xs = tupE xs

-- | read ./config.ini for example as expressions:
--
-- > [iniVal| first_layer_height |] :: Double
-- > [iniVal| first_layer_height layer_height |] :: (Double,Double)
--
-- or as a top-level declaration:
--
-- > [iniVal| extrusion_width layer_height |]
-- ==>
-- extrusion_width = 0.4
-- layer_height = 0.2
--
-- if it parses as an Int it'll be Int
iniVal :: QuasiQuoter
iniVal =
  QuasiQuoter
    { quoteExp = tupE1 . map lookupOneE . words,
      quoteDec = \fieldNames -> do
        sequence [ valD (varP n) b []
          | f <- words fieldNames,
            let n = mkName f,
            let b = normalB (lookupOneE f) ]
    }
