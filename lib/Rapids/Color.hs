{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

-- | propagate face colors
module Rapids.Color where

import Foreign
import Foreign.C.Types
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear (V3 (..))
import qualified OpenCascade.GP.Vec as GPVec
import OpenCascade.TopoDS (Shape)
import Waterfall hiding (Shape)
import OpenCascade.TDocStd.Document (Document, fromStorageFormat)
import OpenCascade.Handle (Handle)
import Waterfall.Internal.Solid (Solid(Solid))
import Foreign.C
import Data.IORef
import System.FilePath
import System.Directory
import Control.Applicative
import qualified Data.Map as M

import Rapids.BoolOp
import Data.Map (Map)
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified OpenCascade.BOPAlgo.Operation as BOPAlgo.Operation
import Data.Coerce
import Control.Monad

C.context occtContext
Cpp.include "<TDocStd_Document.hxx>"
Cpp.include "<XCAFDoc_ColorTool.hxx>"
Cpp.include "<XCAFDoc_ShapeTool.hxx>"
Cpp.include "<XCAFApp_Application.hxx>"
Cpp.include "<XCAFDoc_DocumentTool.hxx>"
Cpp.include "<STEPCAFControl_Writer.hxx>"
Cpp.include "<Quantity_Color.hxx>"

{-# NOINLINE faceColorMap #-}
faceColorMap :: IORef (Map (Ptr ()) (V3 CDouble))
faceColorMap = unsafePerformIO $ newIORef Map.empty

setColor :: V3 CDouble -> Solid -> IO Solid
setColor color solid@(Solid ptr) = do
  (facePtrs, n) <- getFaces ptr
  faces <- peekArray (fromIntegral n) facePtrs
  modifyIORef' faceColorMap $ \m -> foldr (`Map.insert` color) m faces
  pure solid

mkStepWriterColor :: IO ([(V3 CDouble, Solid)] -> IO FilePath)
mkStepWriterColor = do
    count <- newIORef Nothing
    prefix <- takeBaseName <$> getCurrentDirectory
    return \ rgbsolids -> do
      doc <- newXCAFDoc
      count <- atomicModifyIORef count (\a -> (succ <$> a <|> Just 0, a))
      let out = prefix ++ maybe "" show count ++ ".step"
      mapM_ (addShapeWithColor doc) rgbsolids
      forM_ solids $ \solid -> do
        colorMap <- readIORef faceColorMap
        (facePtrs, n) <- getFaces (rawSolid solid)
        faces <- peekArray (fromIntegral n) facePtrs
        let faceColors = [(f, c) | f <- faces, Just c <- [Map.lookup f colorMap]]
        addShapeWithFaceColors doc solid faceColors
      writeXCAFToSTEP out doc
      return out

withBooleans2 :: BOPAlgo.Operation.Operation -> [Solid] -> IO Solid
withBooleans2 op inputs = withBooleans op inputs \ (result, history) -> do
  propagateColors history (coerce inputs)
  return result

propagateColors :: Ptr () -> [Ptr ()] -> IO ()
propagateColors history _ | history == nullPtr = return ()
propagateColors history inputs = do
  colorMap <- readIORef faceColorMap
  inputFaces <- concat <$> mapM (fmap (uncurry peekArray . swap) . getFaces) inputs
  forM_ inputFaces $ \face ->
    forM_ (Map.lookup face colorMap) $ \color -> do
      (modFaces, n) <- getModifiedFaces history face
      modFaceList   <- peekArray (fromIntegral n) modFaces
      modifyIORef' faceColorMap $ \m -> foldr (`Map.insert` color) m modFaceList

-- Returns a raw doc pointer you thread through
newXCAFDoc :: IO (Ptr ())
newXCAFDoc = [C.block| void* {
    Handle(XCAFApp_Application) app = XCAFApp_Application::GetApplication();
    Handle(TDocStd_Document) doc;
    app->NewDocument("XmlXCAF", doc);
    doc->IncrementRefCounter();
    return doc.get();
}|]

addShapeWithColor :: Ptr () -> (V3 CDouble, Solid) -> IO ()
addShapeWithColor doc (V3 r g b, solid) =
  [C.block| void {
    Handle(TDocStd_Document) docH((const TDocStd_Document*)$(void* doc));
    auto shapeTool = XCAFDoc_DocumentTool::ShapeTool(docH->Main());
    auto colorTool = XCAFDoc_DocumentTool::ColorTool(docH->Main());

    TDF_Label label = shapeTool->NewShape();
    shapeTool->SetShape(label, *$solid:solid);

    Quantity_Color color($(double r), $(double g), $(double b), Quantity_TOC_RGB);
    colorTool->SetColor(label, color, XCAFDoc_ColorSurf);
  }|]

addShapeWithFaceColors :: Ptr () -> Solid -> [(Ptr (), V3 CDouble)] -> IO ()
addShapeWithFaceColors doc solid@(Solid sptr) faceColors =
  withArray (map fst faceColors) $ \facePtrs ->
  withArray (concatMap (\(_, V3 r g b) -> [r,g,b]) faceColors) $ \colorArr ->
  let n = fromIntegral (length faceColors) in
  [C.block| void {
    Handle(TDocStd_Document) docH((const TDocStd_Document*)$(void* doc));
    auto shapeTool = XCAFDoc_DocumentTool::ShapeTool(docH->Main());
    auto colorTool = XCAFDoc_DocumentTool::ColorTool(docH->Main());

    TDF_Label shapeLabel = shapeTool->NewShape();
    shapeTool->SetShape(shapeLabel, *$solid:solid);

    void**  faces  = $(void** facePtrs);
    double* colors = $(double* colorArr);
    for (int i = 0; i < $(int n); i++) {
      TopoDS_Shape* face = (TopoDS_Shape*)faces[i];
      TDF_Label faceLabel;
      shapeTool->FindSubShape(shapeLabel, *face, faceLabel);
      if (faceLabel.IsNull())
        faceLabel = shapeTool->AddSubShape(shapeLabel, *face);
      Quantity_Color c(colors[i*3], colors[i*3+1], colors[i*3+2], Quantity_TOC_RGB);
      colorTool->SetColor(faceLabel, c, XCAFDoc_ColorSurf);
    }
  }|]

writeXCAFToSTEP :: FilePath -> Ptr () -> IO ()
writeXCAFToSTEP filepath doc =
  withCString filepath $ \fp ->
  [C.block| void {
    Handle(TDocStd_Document) docH((const TDocStd_Document*)$(void* doc));
    STEPCAFControl_Writer writer;
    writer.SetColorMode(true);
    writer.Transfer(docH);
    writer.Write($(const char* fp));
    docH->DecrementRefCounter();
  }|]

