{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

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

C.context occtContext
Cpp.include "<TDocStd_Document.hxx>"
Cpp.include "<XCAFDoc_ColorTool.hxx>"
Cpp.include "<XCAFDoc_ShapeTool.hxx>"
Cpp.include "<XCAFApp_Application.hxx>"
Cpp.include "<XCAFDoc_DocumentTool.hxx>"
Cpp.include "<STEPCAFControl_Writer.hxx>"
Cpp.include "<Quantity_Color.hxx>"

mkStepWriterColor :: IO ([(V3 CDouble, Solid)] -> IO FilePath)
mkStepWriterColor = do
    count <- newIORef Nothing
    prefix <- takeBaseName <$> getCurrentDirectory
    return \ rgbsolids -> do
      doc <- newXCAFDoc
      count <- atomicModifyIORef count (\a -> (succ <$> a <|> Just 0, a))
      let out = prefix ++ maybe "" show count ++ ".step"
      mapM_ (addShapeWithColor doc) rgbsolids
      writeXCAFToSTEP out doc
      return out


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
