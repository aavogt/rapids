
-- | face-colored + * -
module Rapids.BoolOp where

import Data.Acquire (withAcquire)
import Data.Foldable
import Data.Maybe (mapMaybe)
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
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"
Cpp.include "<TopoDS_Iterator.hxx>"
Cpp.include "<vector>"

withBooleans ::
  BOPAlgo.Operation.Operation ->
  [Solid] ->
  ((Solid, Ptr ()) -> IO a) ->
  IO a
withBooleans op inputs k
  | op == BOPAlgo.Operation.Fuse = do
      classified <- mapM (\solid -> do
        split <- splitPointProxies solid
        pure (solid, split)) inputs
      let arguments = mapMaybe (\(solid, split) -> case split of
            Nothing -> Just solid
            Just (geometry, _) -> geometry) classified
          proxies = [proxy | (_, Just (_, proxy)) <- classified]
      runBoolean op arguments \(result, history) ->
        let resultWithProxies = case (arguments, proxies) of
              ([], proxy : rest) -> foldl addProxy proxy rest
              _ -> foldl addProxy result proxies
        in k (resultWithProxies, history)
  | otherwise = runBoolean op inputs k
  where
    splitPointProxies solid =
      allocaArray 2 $ \output -> do
        let output' = castPtr output
        hasProxies <- [Cpp.block| int {
          TopoDS_Shape** outputs = (TopoDS_Shape**)$(void* output');
          TopoDS_Shape* input = $solid:solid;
          if (input == nullptr || input->IsNull()) return 0;

          auto isVertexProxy = [](const TopoDS_Shape& shape) -> bool {
            if (shape.ShapeType() != TopAbs_COMPOUND) return false;
            TopExp_Explorer vertices(shape, TopAbs_VERTEX);
            if (!vertices.More()) return false;
            TopExp_Explorer compsolids(shape, TopAbs_COMPSOLID);
            if (compsolids.More()) return false;
            TopExp_Explorer solids(shape, TopAbs_SOLID);
            if (solids.More()) return false;
            TopExp_Explorer shells(shape, TopAbs_SHELL);
            if (shells.More()) return false;
            TopExp_Explorer faces(shape, TopAbs_FACE);
            if (faces.More()) return false;
            TopExp_Explorer wires(shape, TopAbs_WIRE);
            if (wires.More()) return false;
            TopExp_Explorer edges(shape, TopAbs_EDGE);
            return !edges.More();
          };

          BRep_Builder builder;
          TopoDS_Compound geometry;
          TopoDS_Compound proxies;
          builder.MakeCompound(geometry);
          builder.MakeCompound(proxies);
          std::vector<TopoDS_Shape> pending;
          pending.push_back(*input);
          bool foundProxy = false;
          bool foundGeometry = false;
          while (!pending.empty()) {
            TopoDS_Shape current = pending.back();
            pending.pop_back();
            if (isVertexProxy(current)) {
              builder.Add(proxies, current);
              foundProxy = true;
            } else if (current.ShapeType() == TopAbs_COMPOUND) {
              TopoDS_Iterator children(current);
              for (; children.More(); children.Next()) {
                pending.push_back(children.Value());
              }
            } else {
              builder.Add(geometry, current);
              foundGeometry = true;
            }
          }
          if (!foundProxy) return 0;
          outputs[0] = foundGeometry ? new TopoDS_Shape(geometry) : nullptr;
          outputs[1] = new TopoDS_Shape(proxies);
          return 1;
        }|]
        if hasProxies == 0
          then pure Nothing
          else do
            [geometryPtr, proxyPtr] <- peekArray 2 output
            let geometry =
                  if geometryPtr == nullPtr
                    then Nothing
                    else Just (ownSolid (pure geometryPtr))
            pure (Just (geometry, ownSolid (pure proxyPtr)))

    addProxy base proxy = ownSolid [Cpp.block| TopoDS_Shape* {
      TopoDS_Shape* baseShape = $solid:base;
      TopoDS_Shape* proxyShape = $solid:proxy;
      BRep_Builder builder;
      TopoDS_Compound compound;
      builder.MakeCompound(compound);
      if (baseShape != nullptr && !baseShape->IsNull()) {
        builder.Add(compound, *baseShape);
      }
      if (proxyShape != nullptr && !proxyShape->IsNull()) {
        TopoDS_Iterator proxies(*proxyShape);
        for (; proxies.More(); proxies.Next()) {
          builder.Add(compound, proxies.Value());
        }
      }
      return new TopoDS_Shape(compound);
    }|]


    runBoolean _ [] k = k (emptySolid, nullPtr)
    runBoolean _ [x] k = k (x, nullPtr)
    runBoolean op (h : solids) k = withAcquire BOPAlgo.BOP.new \bop -> do
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
