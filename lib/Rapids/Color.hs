
-- | propagate face colors
module Rapids.Color
  ( mkStepWriterColor,

    -- * set colors
    lightgray,
    gray,
    darkgray,
    yellow,
    gold,
    orange,
    pink,
    red,
    maroon,
    green,
    lime,
    darkgreen,
    skyblue,
    blue,
    darkblue,
    purple,
    violet,
    darkpurple,
    beige,
    brown,
    darkbrown,
    white,
    black,
    magenta,
    raywhite,
    setColor,

    -- * color propagating
    intersections,
    unions,
    differences,
    intersection,
    union,
    difference,
    PropagateColor (..),
    -- * internals
    faceColorMap,
  )
where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.StateVar
import Foreign
import Foreign.C
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear (V3 (..))
import OpenCascade.BOPAlgo.Operation (Operation (..))
import qualified OpenCascade.BOPAlgo.Operation as BOPAlgo.Operation
import qualified OpenCascade.BRepBuilderAPI.Copy as BRepBuilderAPI.Copy
import Rapids.BoolOp
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Waterfall hiding (Shape, difference, intersection, intersections, union, unions)
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Waterfall.Internal.Solid (Solid (Solid))

C.context (occtContext <> Cpp.funCtx)
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
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<stdio.h>"

type FaceKey = (Ptr (), CSize)

{-# NOINLINE faceColorMap #-}
faceColorMap :: IORef (Map FaceKey (V3 CDouble))
faceColorMap = unsafePerformIO $ newIORef Map.empty

normalizeColor :: V3 CDouble -> V3 CDouble
normalizeColor color@(V3 r g b)
  | all inUnit [r, g, b] = color
  | all inByte [r, g, b] = fmap (/ 255) color
  | otherwise = fmap clamp01 color
  where
    inUnit x = 0 <= x && x <= 1
    inByte x = 0 <= x && x <= 255
    clamp01 x = max 0 (min 1 x)

setColor :: V3 CDouble -> Solid -> Solid
setColor (normalizeColor -> color) (Solid raw) = unsafeFromAcquire do
  solid <- Solid <$> BRepBuilderAPI.Copy.copy raw True True -- deep copy
  liftIO $ withFaces_ solid $ \k -> modifyIORef' faceColorMap $ Map.insert k color
  pure solid

faceKeys :: Solid -> IO [FaceKey]
faceKeys solid = do
  keysRef <- newIORef []
  withFaces_ solid $ \k ->
    modifyIORef' keysRef (k :)
  reverse <$> readIORef keysRef

class (Transformable a) => PropagateColor a where
  propagateColor :: (a -> a) -> (a -> a)

instance {-# OVERLAPS #-} (Transformable a) => PropagateColor a where propagateColor = id

-- | the output 'faceKeys' should get the same color as the input 'faceKeys'
instance PropagateColor Solid where
  propagateColor f solid = unsafePerformIO do
    let !solid' = f solid
    colorMap <- readIORef faceColorMap
    inKeys <- faceKeys solid
    outKeys <- faceKeys solid'
    for_ (zip inKeys outKeys) \(srcKey, dstKey) ->
      for_ (Map.lookup srcKey colorMap) \color ->
        modifyIORef' faceColorMap $ Map.insert dstKey color
    pure solid'

withFaces_ :: Solid -> (FaceKey -> IO ()) -> IO ()
withFaces_ solid (curry -> kFun) =
  [C.block| void{
  TopExp_Explorer explorer(*$solid:solid, TopAbs_FACE);
  for (; explorer.More(); explorer.Next()) {
      const TopoDS_Face& face = TopoDS::Face(explorer.Current());
      void* shapePtr = (void*)face.TShape().get();
      size_t locHash = face.Location().HashCode();
      $fun:(void (*kFun)(void*, size_t))(shapePtr, locHash);
  }
} |]

withModifiedFaces_ :: Ptr () -> Solid -> (Ptr () -> CSize -> Ptr () -> CSize -> IO ()) -> IO ()
withModifiedFaces_ history solid kFun =
  [C.block| void{
    if ($(void* history) == nullptr) {
      return;
    }
    BRepTools_History* hist = (BRepTools_History*)$(void* history);
    TopExp_Explorer explorer(*$solid:solid, TopAbs_FACE);
    for (; explorer.More(); explorer.Next()) {
      const TopoDS_Face& face = TopoDS::Face(explorer.Current());
      void* srcShapePtr = (void*)face.TShape().get();
      size_t srcLocHash = face.Location().HashCode();
      const TopTools_ListOfShape& mods = hist->Modified(face);
      for (TopTools_ListIteratorOfListOfShape it(mods); it.More(); it.Next()) {
        const TopoDS_Shape& modShape = it.Value();
        if (modShape.ShapeType() != TopAbs_FACE) {
          continue;
        }
        void* modShapePtr = (void*)modShape.TShape().get();
        size_t modLocHash = modShape.Location().HashCode();
        $fun:(void (*kFun)(void*, size_t, void*, size_t))(srcShapePtr, srcLocHash, modShapePtr, modLocHash);
      }
      const TopTools_ListOfShape& gens = hist->Generated(face);
      for (TopTools_ListIteratorOfListOfShape it(gens); it.More(); it.Next()) {
        const TopoDS_Shape& genShape = it.Value();
        if (genShape.ShapeType() != TopAbs_FACE) {
          continue;
        }
        void* genShapePtr = (void*)genShape.TShape().get();
        size_t genLocHash = genShape.Location().HashCode();
        $fun:(void (*kFun)(void*, size_t, void*, size_t))(srcShapePtr, srcLocHash, genShapePtr, genLocHash);
      }
    }
  }|]

mkStepWriterColor :: IO (Solid -> IO FilePath)
mkStepWriterColor = do
  count <- newIORef Nothing
  prefix <- takeBaseName <$> getCurrentDirectory
  return \ !solid -> do
    doc <- newXCAFDoc
    count <- atomicModifyIORef count (\a -> (succ <$> a <|> Just 0, a))
    let out = prefix ++ maybe "" show count ++ ".step"
    colorMap <- readIORef faceColorMap
    faceColors <- newIORef []
    withFaces_ solid $ \k ->
      for_ (Map.lookup k colorMap) \c -> faceColors $~ ((k, c) :)
    addShapeWithFaceColors doc solid =<< get faceColors
    writeXCAFToSTEP out doc
    return out

-- data Operation = Common | Fuse | Cut | Cut21 | Section | Unknown
intersections, unions, differences :: [Solid] -> Solid
intersections = unsafePerformIO . withBooleans2 Common
unions = unsafePerformIO . withBooleans2 Fuse
differences = unsafePerformIO . withBooleans2 Cut

intersection, union, difference :: Solid -> Solid -> Solid
intersection a b = intersections [a, b]
union a b = unions [a, b]
difference a b = differences [a, b]

withBooleans2 :: BOPAlgo.Operation.Operation -> [Solid] -> IO Solid
withBooleans2 op inputs = withBooleans op inputs \(result, history) -> do
  propagateColors history inputs
  return result

propagateColors :: Ptr () -> [Solid] -> IO ()
propagateColors history _ | history == nullPtr = return ()
propagateColors history solids = do
  colorMap <- readIORef faceColorMap
  for_ solids \solid ->
    withModifiedFaces_ history solid $ \srcShapePtr srcLocHash dstShapePtr dstLocHash ->
      for_ (Map.lookup (srcShapePtr, srcLocHash) colorMap) \color ->
        modifyIORef' faceColorMap $ \m -> Map.insert (dstShapePtr, dstLocHash) color m

-- Returns a raw doc pointer you thread through
newXCAFDoc :: IO (Ptr ())
newXCAFDoc =
  [C.block| void* {
    Handle(XCAFApp_Application) app = XCAFApp_Application::GetApplication();
    Handle(TDocStd_Document) doc;
    app->NewDocument("XmlXCAF", doc);
    doc->IncrementRefCounter();
    return doc.get();
}|]

addShapeWithFaceColors :: Ptr () -> Solid -> [(FaceKey, V3 CDouble)] -> IO ()
addShapeWithFaceColors doc solid faceColors =
  withArray (map (fst . fst) faceColors) $ \faceShapePtrs ->
    withArray (map (snd . fst) faceColors) $ \faceLocHashes ->
      withArray (concatMap (\(_, V3 r g b) -> [r, g, b]) faceColors) $ \colorArr ->
        let n = fromIntegral (length faceColors)
         in [C.block| void {
    try {
      Handle(TDocStd_Document) docH((const TDocStd_Document*)$(void* doc));
      auto shapeTool = XCAFDoc_DocumentTool::ShapeTool(docH->Main());
      auto colorTool = XCAFDoc_DocumentTool::ColorTool(docH->Main());

      TDF_Label shapeLabel = shapeTool->AddShape(*$solid:solid, Standard_False);
      if (shapeLabel.IsNull()) {
        fprintf(stderr, "addShapeWithFaceColors: AddShape returned null label\n");
        return;
      }

      void**  shapePtrs = $(void** faceShapePtrs);
      size_t* locHashes = $(size_t* faceLocHashes);
      double* colors    = $(double* colorArr);

      TopExp_Explorer explorer(*$solid:solid, TopAbs_FACE);
      for (; explorer.More(); explorer.Next()) {
        const TopoDS_Face& face = TopoDS::Face(explorer.Current());
        void* faceShapePtr = (void*)face.TShape().get();
        size_t faceLocHash = face.Location().HashCode();
        for (int i = 0; i < $(int n); i++) {
          if (shapePtrs[i] != faceShapePtr || locHashes[i] != faceLocHash) {
            continue;
          }

          TDF_Label faceLabel;
          shapeTool->AddSubShape(shapeLabel, face, faceLabel);
          if (faceLabel.IsNull()) {
            fprintf(stderr, "addShapeWithFaceColors: no face label for matched subshape\n");
            continue;
          }

          Quantity_Color c(colors[i*3], colors[i*3+1], colors[i*3+2], Quantity_TOC_RGB);
          colorTool->SetColor(faceLabel, c, XCAFDoc_ColorSurf);
          break;
        }
      }
    } catch (const Standard_Failure& e) {
      fprintf(stderr, "addShapeWithFaceColors: OCCT exception: %s\n", e.GetMessageString());
    } catch (...) {
      fprintf(stderr, "addShapeWithFaceColors: unknown C++ exception\n");
    }
  }|]

writeXCAFToSTEP :: FilePath -> Ptr () -> IO ()
writeXCAFToSTEP filepath doc =
  withCString filepath $ \fp ->
    [C.block| void {
    try {
      Handle(TDocStd_Document) docH((const TDocStd_Document*)$(void* doc));
      STEPCAFControl_Writer writer;
      writer.SetColorMode(true);
      writer.Transfer(docH);
      writer.Write($(const char* fp));
      docH->DecrementRefCounter();
    } catch (const Standard_Failure& e) {
      fprintf(stderr, "writeXCAFToSTEP: OCCT exception: %s\n", e.GetMessageString());
    } catch (...) {
      fprintf(stderr, "writeXCAFToSTEP: unknown C++ exception\n");
    }
  }|]

{- ORMOLU_DISABLE -}
lightgray, gray, darkgray, yellow, gold, orange, pink, red,
  maroon, green, lime, darkgreen, skyblue, blue, darkblue,
  purple, violet, darkpurple, beige, brown, darkbrown, white,
  black, magenta, raywhite :: Solid -> Solid
lightgray = setColor (V3 200 200 200)
gray = setColor (V3 130 130 130)
darkgray = setColor (V3 80 80 80)
yellow = setColor (V3 253 249 0)
gold = setColor (V3 255 203 0)
orange = setColor (V3 255 161 0)
pink = setColor (V3 255 109 194)
red = setColor (V3 230 41 55)
maroon = setColor (V3 190 33 55)
green = setColor (V3 0 228 48)
lime = setColor (V3 0 158 47)
darkgreen = setColor (V3 0 117 44)
skyblue = setColor (V3 102 191 255)
blue = setColor (V3 0 121 241)
darkblue = setColor (V3 0 82 172)
purple = setColor (V3 200 122 255)
violet = setColor (V3 135 60 190)
darkpurple = setColor (V3 112 31 126)
beige = setColor (V3 211 176 131)
brown = setColor (V3 127 106 79)
darkbrown = setColor (V3 76 63 47)
white = setColor (V3 255 255 255)
black = setColor (V3 0 0 0)
magenta = setColor (V3 255 0 255)
raywhite = setColor (V3 245 245 245)
{- ORMOLU ENABLE -}
