{-# LANGUAGE TemplateHaskell #-}

module InlineOCCT where

import Data.Acquire
import qualified Data.Map as Map
import Foreign hiding (with)
import Language.C.Inline.Context
import Language.C.Inline.Cpp
import Language.C.Inline.HaskellIdentifier
import Language.C.Types as C
import Language.Haskell.TH as TH
import OpenCascade.GP.Types
import OpenCascade.TopoDS.Types
import Waterfall.Internal.Solid
import Waterfall.Internal.ToOpenCascade (v3ToDir, v3ToPnt, v3ToVertex)

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
      (f "Solid", [t|Ptr Shape|])
    ]

f :: String -> TypeSpecifier
f str = TypeName $ either (error "tt") id $ cIdentifierFromString True str

p :: String -> C.Type i
p str = Ptr [] (TypeSpecifier mempty (f str))

aq :: AntiQuoters
aq =
  Map.fromList
    [ ("dir", SomeAntiQuoter dirAntiQuoter),
      ("pnt", SomeAntiQuoter pntAntiQuoter),
      ("solid", SomeAntiQuoter solidAntiQuoter)
    ]

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
