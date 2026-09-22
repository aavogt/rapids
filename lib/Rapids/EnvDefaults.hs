module Rapids.EnvDefaults (
  -- * interface
  envDefaults,
  whenEnvParseFail,

  -- * customize envDefaultsFailed
  renderParseErrors,
  renderEnvHelp,

  -- * internals
  readMaybeStr,

  -- ** what it generates
  -- | #ddumpsplices#
  --
  -- > envDefaults [d| incline = 20 |]
  -- >  ==>
  -- > incline_orig = 20
  -- > {-# NOINLINE incline #-}
  -- > incline = unsafePerformIO $ fromMaybe incline_orig . readMaybeStr <$> lookupEnv "incline"
  -- > ...
  -- > envWatchList :: [(String, IO (Maybe String), Dynamic, String)]
  -- > envWatchList = [("incline", _parseError, toDyn a_orig, show a_orig)....
  --
  -- @$$whenEnvParseFail :: IO () -> IO ()@ called in main responds to 
  -- --help -h --verbose -v command line arguments,
  -- prints parse errors on stderr,
  -- then calls the continuation when at least one environment
  -- variable fails to parse into the inferred type using 'readMaybeStr'.
  ) where

import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Language.Haskell.TH
import System.Environment
import System.IO.Unsafe
import Text.Read
import Data.Functor
import System.IO
import Control.Monad
import System.Exit

-- | Usage:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import EnvDefaults
-- >
-- > envDefaults [d|
-- >  incline = 20
-- >  dest = "out"
-- >  |]
-- >
-- > main = do
-- >  $$whenEnvParseFail exitFailure -- error
-- >  -- $$whenEnvParseFail mempty      -- warning
-- >  print (incline, dest)
--
-- >>cabal build
-- >>exe=`cabal list-bin all`
-- >>$exe --help
-- > Environment variables:
-- >  incline = 20.0  [default]  (default 20.0)
-- >  dest = "out"  [default]  (default "out")
-- >
-- >>incline=3 dest=xyz $exe 
-- > (3.0, "xyz")
-- >
-- >>incline=3 dest=xyz $exe -v
-- > Environment variables:
-- >  incline = 3  [from env]  (default 20.0)
-- >  dest = xyz  [from env]  (default "out")
-- >
-- > (3.0,"xyz")
--
-- [-ddump-splices]("Rapids.EnvDefaults#ddumpsplices")
envDefaults :: Q [Dec] -> Q [Dec]
envDefaults decs = do
  decs <- decs
  let pairs = pairUp decs
  rewritten <- concat <$> mapM rewriteOne pairs
  watchList <- watchListDecs pairs
  pure (rewritten ++ watchList)

-- | > $$whenEnvParseFail :: IO () -> IO ()
whenEnvParseFail :: Code Q (IO () -> IO ())
whenEnvParseFail = [|| \k -> do
  args <- getArgs
  let envWatchList = $$(unsafeCodeCoerce [| envWatchList |])
  when ("--help" `elem` args || "-h" `elem` args) do
    putStrLn =<< renderEnvHelp envWatchList
    exitSuccess
  when ("--verbose" `elem` args || "-v" `elem` args) do
    putStrLn =<< renderEnvHelp envWatchList
  errors <- traverse (hPutStrLn stderr) =<< renderParseErrors envWatchList
  when (isJust errors) k
  ||]

renderEnvHelp :: [(String, IO (Maybe String), Dynamic, String)] -> IO String
renderEnvHelp vars = do
  ls <- mapM describe vars
  pure (unlines ("Environment variables:" : ls))
  where
    describe (name, _, value, showValue) = do
      mv <- lookupEnv name
      let current = maybe (showValue ++ "  [default]") (++ "  [from env]") mv
      pure ("  " ++ name ++ " = " ++ current ++ "  (default " ++ showValue ++ ")")

renderParseErrors :: [(String, IO (Maybe String), Dynamic, String)] -> IO (Maybe String)
renderParseErrors env = mapM (\(_, err, _, _) -> err) env <&> \ msgs -> case catMaybes msgs of
  [] -> Nothing
  ms -> Just (unlines ms)

-- | Zip each Double type signature with the value binding that follows it.
pairUp :: [Dec] -> [(Name, Maybe Type, Exp)]
pairUp (SigD n ty : ValD (VarP n') (NormalB e) [] : rest) | n == n' = (n, Just ty, e) : pairUp rest
pairUp (ValD (VarP n) (NormalB e) [] : rest) = (n, Nothing, e) : pairUp rest
pairUp (_ : rest) = pairUp rest
pairUp [] = []

rewriteOne :: (Name, Maybe Type, Exp) -> Q [Dec]
rewriteOne (nameBase -> ns, mty, defExpr) = do
  let orig = mkName (ns ++ "_orig") -- plumb newName instead of "_orig" suffix?
  let lookupIO = [|lookupEnv ns <&> fromMaybe $(varE orig) . (readMaybeStr =<<) |]
  let n = mkName ns
  sequence $
    [sigD n (pure ty) | Just ty <- [mty]] ++
      [pure $ PragmaD (InlineP n NoInline FunLike AllPhases), -- confirm FunLike
      valD (varP n) (normalB [|unsafePerformIO $lookupIO|]) [],
      valD (varP orig) (normalB (pure defExpr)) []
         ]

-- | readMaybeStr is readMaybe with a special case for String:
--
-- > readMaybe "abc" == Nothing
-- > readMaybe "\"abc\"" == Just "abc"
-- > readMaybeStr "abc" == Just "abc"
-- > readMaybeStr "\"abc\"" == Just "abc"
-- > readMaybeStr "'abc'" == Just "'abc'" -- may change depending on what shells do?
readMaybeStr :: forall a. (Read a, Typeable a) => String -> Maybe a
readMaybeStr s
  | typeRep (Proxy :: Proxy a) == typeOf "" = readMaybe s `mplus` cast s
  | otherwise = readMaybe s

watchListDecs :: [(Name, Maybe Type, Exp)] -> Q [Dec]
watchListDecs pairs = [d|
  envWatchList :: [(String, IO (Maybe String), Dynamic, String)]
  envWatchList = $(listE
    [ [|(ns, $parseError, toDyn $(varE n0), show $(varE n0))|]
      | (nameBase -> ns, mty, e) <- pairs,
        let n0 = mkName $ ns ++ "_orig",
        let parseError = [| lookupEnv ns <&> \str -> case (readMaybeStr <$> str) `asTypeOf` Just (Just $(varE n0)) of
              Just Nothing ->
                let self = error $ "Environment variable " ++ show ns ++ " = " ++ show str ++
                                            " cannot read as " ++ show (typeOf self)
                in self
              _ -> Nothing
              |]
      ]
    )
  |]
