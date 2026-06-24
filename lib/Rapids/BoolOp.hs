
-- | face-colored + * -
module Rapids.BoolOp where

import Data.Acquire (withAcquire)
import Data.Foldable
import Foreign
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified OpenCascade.BOPAlgo.BOP as BOPAlgo.BOP
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo.Builder
import qualified OpenCascade.BOPAlgo.Operation as BOPAlgo.Operation
import OpenCascade.Inheritance
import Waterfall hiding (Shape)
import Waterfall.Internal.Finalizers (fromAcquire, toAcquire)
import Waterfall.Internal.Solid (Solid (Solid), rawSolid)

C.context occtContext
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Face.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<BRepTools_History.hxx>"
Cpp.include "<TopTools_ListOfShape.hxx>"
Cpp.include "<TopTools_ListIteratorOfListOfShape.hxx>"
Cpp.include "<TDocStd_Document.hxx>"
Cpp.include "<XCAFDoc_ColorTool.hxx>"
Cpp.include "<XCAFDoc_ShapeTool.hxx>"
Cpp.include "<XCAFApp_Application.hxx>"
Cpp.include "<XCAFDoc_DocumentTool.hxx>"
Cpp.include "<STEPCAFControl_Writer.hxx>"
Cpp.include "<Quantity_Color.hxx>"
Cpp.include "<BRepAlgo.hxx>"
Cpp.include "<BOPAlgo_BOP.hxx>"
Cpp.include "<BOPAlgo_Builder.hxx>"
Cpp.include "<BOPAlgo_BuilderShape.hxx>"
Cpp.include "<BRepTools_History.hxx>"
Cpp.include "<BRepAlgoAPI_BooleanOperation.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"

withBooleans ::
  BOPAlgo.Operation.Operation ->
  [Solid] ->
  ((Solid, Ptr ()) -> IO a) ->
  IO a
withBooleans _ [] k = k (emptySolid, nullPtr)
withBooleans _ [x] k = k (x, nullPtr)
withBooleans op (h : solids) k = withAcquire BOPAlgo.BOP.new \bop -> do
  firstPtr <- fromAcquire . toAcquire . rawSolid $ h
  ptrs <- traverse (fromAcquire . toAcquire . rawSolid) solids
  let builder = upcast bop
  BOPAlgo.BOP.setOperation bop op
  BOPAlgo.Builder.addArgument builder firstPtr
  traverse_ (BOPAlgo.BOP.addTool bop) ptrs
  BOPAlgo.setRunParallel builder True
  BOPAlgo.Builder.perform builder
  shapePtr <- fromAcquire (BOPAlgo.Builder.shape builder)
  histPtr <- builderHistory (castPtr bop)
  k (Solid shapePtr, histPtr)

builderHistory :: Ptr () -> IO (Ptr ())
builderHistory builder =
  [C.block| void* {
  Handle(BRepTools_History) hist = ((BOPAlgo_BuilderShape*)$(void* builder))->History();
  if (hist.IsNull()) {
    return nullptr;
  }
  return hist.get();
}|]
