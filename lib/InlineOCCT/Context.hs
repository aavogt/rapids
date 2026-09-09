module InlineOCCT.Context where

import Control.Exception (bracket)
import Data.Acquire
import qualified Data.Map as Map
import Foreign hiding (with)
import Language.C.Inline.Context
import Language.C.Inline.Cpp
import qualified Language.C.Inline.Cpp as Cpp
import Language.C.Inline.HaskellIdentifier
import Language.C.Types as C
import Language.Haskell.TH as TH
import Linear (V3 (..))
import OpenCascade.GP.Types
import OpenCascade.TopoDS.Types
import Waterfall.Internal.Finalizers (toAcquire)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Solid
import Waterfall.Internal.ToOpenCascade (v3ToDir, v3ToPnt, v3ToVertex)
import qualified Waterfall.TwoD.Internal.Shape as W

getHsVariable :: String -> HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ unHaskellIdentifier s
  case mbHsName of
    Nothing ->
      fail $
        "Cannot capture Haskell variable "
          ++ unHaskellIdentifier s
          ++ ", because it's not in scope. ("
          ++ err
          ++ ")"
    Just hsName -> TH.varE hsName

occtContext :: Context
occtContext = cppCtx {ctxTypesTable = ctxTypesTable cppCtx <> tt, ctxAntiQuoters = aq}

tt :: TypesTable
tt =
  Map.fromList
    [ (f "Vertex", [t|Ptr Vertex|]),
      (f "Pnt", [t|Ptr Pnt|]),
      (f "Dir", [t|Ptr Dir|]),
      (f "TopoDS_Shape", [t|Shape|]),
      (f "TopoDS_Wire", [t|Wire|])
    ]

f :: String -> TypeSpecifier
f str = TypeName $ either (error "tt") id $ cIdentifierFromString True str

p :: String -> C.Type i
p str = Ptr [] (TypeSpecifier mempty (f str))

voidP :: C.Type i
voidP = Ptr [] (TypeSpecifier mempty Void)

aq :: AntiQuoters
aq =
  Map.fromList
    [ ("dir", SomeAntiQuoter dirAntiQuoter),
      ("path", SomeAntiQuoter pathAntiQuoter),
      ("shape", SomeAntiQuoter shapeAntiQuoter),
      ("pnt", SomeAntiQuoter pntAntiQuoter),
      ("pnts", SomeAntiQuoter pntsAntiQuoter),
      ("solid", SomeAntiQuoter solidAntiQuoter)
    ]

pathAntiQuoter :: AntiQuoter HaskellIdentifier
pathAntiQuoter =
  AntiQuoter
    { aqParser = do
        hId <- C.parseIdentifier
        useCpp <- C.parseEnableCpp
        let cId = mangleHaskellIdentifier useCpp hId
        return (cId, p "TopoDS_Wire", hId),
      aqMarshaller = \_purity _cTypes _cTy cId -> do
        hsExp' <- [| with $ toAcquire $ case $(getHsVariable "occtContext" cId) of
          Path (ComplexRawPath wire) -> wire
          _ -> nullPtr |]
        hsTy <- [t|Ptr Wire|]
        return (hsTy, hsExp')
    }

shapeAntiQuoter :: AntiQuoter HaskellIdentifier
shapeAntiQuoter =
  AntiQuoter
    { aqParser = do
        hId <- C.parseIdentifier
        useCpp <- C.parseEnableCpp
        let cId = mangleHaskellIdentifier useCpp hId
        return (cId, p "TopoDS_Shape", hId),
      aqMarshaller = \_purity _cTypes _cTy cId -> do
        hsExp' <- [|with $ toAcquire case $(getHsVariable "occtContext" cId) of W.Shape a -> a |]
        hsTy <- [t|Ptr Shape|]
        return (hsTy, hsExp')
    }

dirAntiQuoter :: AntiQuoter HaskellIdentifier
dirAntiQuoter =
  AntiQuoter
    { aqParser = do
        hId <- C.parseIdentifier
        useCpp <- C.parseEnableCpp
        let cId = mangleHaskellIdentifier useCpp hId
        return (cId, p "gp_Dir", hId),
      aqMarshaller = \_purity _cTypes _cTy cId -> do
        hsExp <- getHsVariable "occtContext" cId
        hsExp' <- [|with (v3ToDir $(return hsExp))|]
        hsTy <- [t|Ptr Dir|]
        return (hsTy, hsExp')
    }

pntAntiQuoter :: AntiQuoter HaskellIdentifier
pntAntiQuoter =
  AntiQuoter
    { aqParser = do
        hId <- C.parseIdentifier
        useCpp <- C.parseEnableCpp
        let cId = mangleHaskellIdentifier useCpp hId
        return (cId, p "gp_Pnt", hId),
      aqMarshaller = \_purity _cTypes _cTy cId -> do
        hsExp <- getHsVariable "occtContext" cId
        hsExp' <- [|with (v3ToPnt $(return hsExp))|]
        hsTy <- [t|Ptr Pnt|]
        return (hsTy, hsExp')
    }

pntsAntiQuoter :: AntiQuoter HaskellIdentifier
pntsAntiQuoter =
  AntiQuoter
    { aqParser = do
        hId <- C.parseIdentifier
        useCpp <- C.parseEnableCpp
        let cId = mangleHaskellIdentifier useCpp hId
        return (cId, voidP, hId),
      aqMarshaller = \_purity _cTypes _cTy cId -> do
        hsExp <- getHsVariable "occtContext" cId
        hsExp' <-
          [|
            \k -> do
              let points = $(return hsExp) :: [V3 Double]
                  n = fromIntegral (length points)
                  coords = concatMap (\(V3 x y z) -> [realToFrac x, realToFrac y, realToFrac z]) points
              withArray coords $ \coordsPtr ->
                bracket (c_newGpPntVector n coordsPtr) c_deleteGpPntVector k
            |]
        hsTy <- [t|Ptr ()|]
        return (hsTy, hsExp')
    }

solidAntiQuoter :: AntiQuoter HaskellIdentifier
solidAntiQuoter =
  AntiQuoter
    { aqParser = do
        hId <- C.parseIdentifier
        useCpp <- C.parseEnableCpp
        let cId = mangleHaskellIdentifier useCpp hId
        return (cId, p "TopoDS_Shape", hId),
      aqMarshaller = \_purity _cTypes _cTy cId -> do
        hsExp <- getHsVariable "occtContext" cId
        hsExp' <- [|with (acquireSolid $(return hsExp))|]
        hsTy <- [t|Ptr Shape|]
        return (hsTy, hsExp')
    }
