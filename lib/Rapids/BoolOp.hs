{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

-- | face-colored + * -
module Rapids.BoolOp where

import Foreign
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import OpenCascade.TopoDS (Shape)
import Waterfall hiding (Shape)
import Waterfall.Internal.Solid (Solid(Solid), rawSolid)
import qualified OpenCascade.BOPAlgo.Operation as BOPAlgo.Operation
import Data.Acquire
import Waterfall.Internal.Finalizers
import qualified OpenCascade.BOPAlgo.BOP as BOPAlgo.BOP
import OpenCascade.Inheritance
import Control.Monad.IO.Class
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo.Builder
import Data.Foldable
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo

C.context occtContext
Cpp.include "<BRepAlgoApi.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"

withBooleans
  :: BOPAlgo.Operation.Operation
  -> [Solid]
  -> ((Solid, Ptr ()) -> IO a)
  -> IO a
withBooleans _ []  k = k (emptySolid, nullPtr)
withBooleans _ [x] k = k (x, nullPtr)
withBooleans op (h:solids) k = withAcquire acquire k
  where
    acquire = do
      firstPtr <- toAcquire . rawSolid $ h
      ptrs     <- traverse (toAcquire . rawSolid) solids
      bop      <- BOPAlgo.BOP.new
      let builder = upcast bop
      liftIO $ do
        BOPAlgo.BOP.setOperation bop op
        BOPAlgo.Builder.addArgument builder firstPtr
        traverse_ (BOPAlgo.BOP.addTool bop) ptrs
        BOPAlgo.setRunParallel builder True
        BOPAlgo.Builder.perform builder
      shapePtr <- BOPAlgo.Builder.shape builder
      histPtr  <- liftIO $ builderHistory (castPtr bop)
      pure (Solid shapePtr, histPtr)

builderHistory :: Ptr () -> IO (Ptr ())
builderHistory builder = [C.exp| void* {
  ((BOPAlgo_Builder*)$(void* builder))->History().get()
}|]
