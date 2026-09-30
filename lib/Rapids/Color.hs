{-# LANGUAGE TemplateHaskell #-}

-- | propagate face colors
module Rapids.Color
  ( mkStepWriterColor,
    writeSTEPColor,
    lightgray,
    lightgrays,
    gray,
    grays,
    darkgray,
    darkgrays,
    yellow,
    yellows,
    gold,
    golds,
    orange,
    oranges,
    pink,
    pinks,
    red,
    reds,
    maroon,
    maroons,
    green,
    greens,
    lime,
    limes,
    darkgreen,
    darkgreens,
    skyblue,
    skyblues,
    blue,
    blues,
    darkblue,
    darkblues,
    purple,
    purples,
    violet,
    violets,
    darkpurple,
    darkpurples,
    beige,
    beiges,
    brown,
    browns,
    darkbrown,
    darkbrowns,
    white,
    whites,
    black,
    blacks,
    magenta,
    magentas,
    raywhite,
    raywhites,
    setColor,
    setColors,
    tagLoc,

    -- * color propagating
    intersections,
    unions,
    differences,
    intersection,
    union,
    difference,
    PropagateColor (..),
    propagateShapeColors,
    propagateSolidColorsToShape,
    propagatePathColors,
    propagatePathEdgeColors,
    propagatePathColorsToShape,
    propagateShapeColorsToShape,
    propagateSolidColors,
    OpC (..),
    leftColor,
    Note,
    ColorKey,
    colorAttrsMap,
    colorAttrsMapReset,
    colorKeys,
    Transform3D (..),
    composeTransform3D,
    Transform2D (..),
    composeTransform2D,
  )
where

import Control.Applicative
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.StateVar
import Data.These (These (..))
import Data.Word (Word8)
import Foreign hiding (rotate)
import Foreign.C
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Language.Haskell.TH (ExpQ, loc_filename, loc_start, location, stringE)
import Language.Haskell.TH.Syntax
import Rapids.Reexports.Linear
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
import Waterfall.TwoD.Internal.Shape (Shape)
import Data.List (unfoldr, transpose)
import Control.Lens
import qualified Data.Set as S

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
Cpp.include "<XCAFDoc_NotesTool.hxx>"
Cpp.include "<XCAFDoc_Note.hxx>"
Cpp.include "<TCollection_ExtendedString.hxx>"
Cpp.include "<TCollection_HAsciiString.hxx>"
Cpp.include "<STEPCAFControl_Writer.hxx>"
Cpp.include "<XSControl_WorkSession.hxx>"
Cpp.include "<StepData_StepModel.hxx>"
Cpp.include "<HeaderSection_FileDescription.hxx>"
Cpp.include "<Interface_HArray1OfHAsciiString.hxx>"
Cpp.include "<TDF_LabelSequence.hxx>"
Cpp.include "<TDF_Tool.hxx>"
Cpp.include "<TCollection_AsciiString.hxx>"
Cpp.include "<Quantity_Color.hxx>"
Cpp.include "<TDataStd_Name.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<stdio.h>"

type ColorKey = (Ptr (), CSize)

type Note = These String (V3 CDouble)

data OpC
  = OpConstC (Maybe Note)
  | OpFC (Maybe Note -> Maybe Note -> Maybe Note)

leftColor :: OpC
leftColor = OpFC (\left _right -> left)

-- | global variable for source location "main.hs:line:col" and color
{-# NOINLINE colorAttrsMap #-}
colorAttrsMap :: IORef (Map ColorKey Note)
colorAttrsMap = unsafePerformIO $ newIORef Map.empty

colorAttrsMapReset :: IO ()
colorAttrsMapReset = writeIORef colorAttrsMap Map.empty

locationStr :: ExpQ
locationStr = do
  loc <- location
  let (line, col) = loc_start loc
  stringE (loc_filename loc ++ ":" ++ show line ++ ":" ++ show col)

-- | @{-# LANGUAGE TemplateHaskell #-}@ @$tagLoc :: Solid -> Solid@ stores a comment
-- with source location attached to all faces of the solid.
tagLoc :: ExpQ
tagLoc = [|tagFaceNote $(locationStr)|]

combineNote :: String -> Maybe Note -> Note
combineNote note mOld =
  case mOld of
    Nothing -> This note
    Just (This _) -> This note
    Just (That color) -> These note color
    Just (These _ color) -> These note color

combineColor :: V3 CDouble -> Maybe Note -> Note
combineColor color mOld =
  case mOld of
    Nothing -> That color
    Just (This note) -> These note color
    Just (That _) -> That color
    Just (These note _) -> These note color

normalizeColor :: V3 CDouble -> V3 CDouble
normalizeColor color@(V3 r g b)
  | all inUnit [r, g, b] = color
  | all inByte [r, g, b] = fmap (/ 255) color
  | otherwise = fmap clamp01 color
  where
    inUnit x = 0 <= x && x <= 1
    inByte x = 0 <= x && x <= 255
    clamp01 x = max 0 (min 1 x)

type Off = V3 Int
type P = V3 Word8

kids :: Off -> [Off]
kids (V3 a b c) =
  [ V3 a' b c | a' <- push a ] ++
  [ V3 a b' c | b' <- push b ] ++
  [ V3 a b c' | c' <- push c ]
  where
    push 0 = [1, -1]
    push d = [d + signum d]

-- All triples in [0,255]^3, nearest to the centre first.
near :: P -> [P]
near c0 = go (S.singleton (0, V3 0 0 0))
  where
    ctr = fromIntegral <$> c0 :: V3 Int
    lo = fromIntegral (minBound :: Word8)
    hi = fromIntegral (maxBound :: Word8)
    inside = all (\x -> x >= lo && x <= hi)

    go frontier = case S.minView frontier of
      Nothing -> []
      Just ((_, d), rest) ->
        let p = ctr + d
        in if inside p
             then fmap fromIntegral p
                  : go (foldr (\o -> S.insert (quadrance o, o)) rest (kids d))
             else go rest

setColor :: V3 CDouble -> Solid -> Solid
setColor color = setColors (repeat color)

setColors :: [V3 CDouble] -> Solid -> Solid
setColors colors (Solid raw) = unsafeFromAcquire do
  solid <- Solid <$> BRepBuilderAPI.Copy.copy raw True True -- deep copy
  colorsRef <- liftIO $ newIORef colors
  liftIO $ withFaces_ solid $ \k -> do
    color <- atomicModifyIORef' colorsRef \remaining ->
      case remaining of
        [] -> ([], Nothing)
        color : rest -> (rest, Just color)
    for_ color $ \color' ->
      modifyIORef' colorAttrsMap $ Map.alter (Just . combineColor (normalizeColor color')) k
  pure solid

tagFaceNote :: String -> Solid -> Solid
tagFaceNote note solid = unsafeFromAcquire do
  liftIO $ withFaces_ solid $ \k -> modifyIORef' colorAttrsMap $ Map.alter (Just . combineNote note) k
  pure solid

-- | may become `Solid -> Set ColorKey`
colorKeys :: Solid -> IO [ColorKey]
colorKeys solid = do
  keysRef <- newIORef []
  withFaces_ solid $ \k ->
    modifyIORef' keysRef (k :)
  reverse <$> readIORef keysRef

newtype Transform3D = Transform3D {runTransform3D :: forall a. (Transformable a) => a -> a}

composeTransform3D :: Transform3D -> Transform3D -> Transform3D
composeTransform3D (Transform3D f) (Transform3D g) = Transform3D (f . g)

newtype Transform2D = Transform2D {runTransform2D :: forall a. (Transformable2D a) => a -> a}

composeTransform2D :: Transform2D -> Transform2D -> Transform2D
composeTransform2D (Transform2D f) (Transform2D g) = Transform2D (f . g)

instance (Transformable a) => Transformable [a] where
  matTransform matrix = fmap (matTransform matrix)
  scale factors = fmap (scale factors)
  uScale factor = fmap (uScale factor)
  rotate axis angle = fmap (rotate axis angle)
  translate vector = fmap (translate vector)
  mirror normal = fmap (mirror normal)

instance (Transformable2D a) => Transformable2D [a] where
  matTransform2D matrix = fmap (matTransform2D matrix)
  rotate2D angle = fmap (rotate2D angle)
  scale2D factors = fmap (scale2D factors)
  uScale2D factor = fmap (uScale2D factor)
  translate2D vector = fmap (translate2D vector)
  mirror2D normal = fmap (mirror2D normal)

class (Transformable a) => PropagateColor a where
  propagateColor :: (a -> a) -> (a -> a)

instance {-# OVERLAPS #-} (Transformable a) => PropagateColor a where propagateColor = id

-- | the output 'faceKeys' should get the same color as the input 'faceKeys'
instance PropagateColor Solid where
  propagateColor f solid = unsafePerformIO do
    let !solid' = f solid
    colorMap <- readIORef colorAttrsMap
    inKeys <- colorKeys solid
    outKeys <- colorKeys solid'
    for_ (zip inKeys outKeys) \(srcKey, dstKey) ->
      for_ (Map.lookup srcKey colorMap) \color ->
        modifyIORef' colorAttrsMap $ Map.insert dstKey color
    pure solid'

withFaces_ :: Solid -> (ColorKey -> IO ()) -> IO ()
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

withShapeFaces_ :: Shape -> (ColorKey -> IO ()) -> IO ()
withShapeFaces_ shape (curry -> kFun) =
  [C.block| void{
  TopExp_Explorer explorer(*$shape:shape, TopAbs_FACE);
  for (; explorer.More(); explorer.Next()) {
      const TopoDS_Face& face = TopoDS::Face(explorer.Current());
      void* shapePtr = (void*)face.TShape().get();
      size_t locHash = face.Location().HashCode();
      $fun:(void (*kFun)(void*, size_t))(shapePtr, locHash);
  }
} |]

withShapeEdges_ :: Shape -> (ColorKey -> IO ()) -> IO ()
withShapeEdges_ shape (curry -> kFun) =
  [C.block| void{
  TopExp_Explorer explorer(*$shape:shape, TopAbs_EDGE);
  for (; explorer.More(); explorer.Next()) {
      const TopoDS_Shape& edge = explorer.Current();
      void* shapePtr = (void*)edge.TShape().get();
      size_t locHash = edge.Location().HashCode();
      $fun:(void (*kFun)(void*, size_t))(shapePtr, locHash);
  }
} |]

withPathEdges_ :: Path -> (ColorKey -> IO ()) -> IO ()
withPathEdges_ path (curry -> kFun) =
  [C.block| void{
  TopoDS_Shape wireShape = *(TopoDS_Shape*)$path:path;
  TopExp_Explorer explorer(wireShape, TopAbs_EDGE);
  for (; explorer.More(); explorer.Next()) {
      const TopoDS_Shape& edge = explorer.Current();
      void* shapePtr = (void*)edge.TShape().get();
      size_t locHash = edge.Location().HashCode();
      $fun:(void (*kFun)(void*, size_t))(shapePtr, locHash);
  }
} |]

-- | Copy shape attributes onto all faces made by a sweep.
propagateShapeColors :: Shape -> Solid -> Solid
propagateShapeColors shape solid = unsafePerformIO do
  sourceAttrs <- newIORef []
  withShapeFaces_ shape $ \key -> do
    colorMap <- readIORef colorAttrsMap
    for_ (Map.lookup key colorMap) $ \attribute ->
      modifyIORef' sourceAttrs (attribute :)
  attrs <- reverse <$> readIORef sourceAttrs
  unless (null attrs) do
    index <- newIORef 0
    withFaces_ solid $ \key -> do
      i <- atomicModifyIORef' index (\n -> (n + 1, n))
      modifyIORef' colorAttrsMap $ Map.insert key (attrs !! (i `mod` length attrs))
  pure solid

propagateSolidColorsToShape :: Solid -> Shape -> Shape
propagateSolidColorsToShape source shape = unsafePerformIO do
  sourceAttrs <- newIORef []
  withFaces_ source $ \key -> do
    colorMap <- readIORef colorAttrsMap
    for_ (Map.lookup key colorMap) $ \attribute ->
      modifyIORef' sourceAttrs (attribute :)
  attrs <- reverse <$> readIORef sourceAttrs
  unless (null attrs) do
    index <- newIORef 0
    let copyAttribute key = do
          i <- atomicModifyIORef' index (\n -> (n + 1, n))
          modifyIORef' colorAttrsMap $ Map.insert key (attrs !! (i `mod` length attrs))
    withShapeFaces_ shape copyAttribute
    withShapeEdges_ shape copyAttribute
  pure shape

-- | Copy path attributes onto the faces made by a revolution.
propagatePathColors :: Path -> Solid -> Solid
propagatePathColors path solid = unsafePerformIO do
  pathAttrs <- newIORef []
  withPathEdges_ path $ \key -> do
    colorMap <- readIORef colorAttrsMap
    for_ (Map.lookup key colorMap) $ \attribute ->
      modifyIORef' pathAttrs (attribute :)
  attrs <- reverse <$> readIORef pathAttrs
  unless (null attrs) do
    index <- newIORef 0
    withFaces_ solid $ \key -> do
      i <- atomicModifyIORef' index (\n -> (n + 1, n))
      modifyIORef' colorAttrsMap $ Map.insert key (attrs !! (i `mod` length attrs))
  pure solid

propagateShapeColorsToShape :: Shape -> Shape -> Shape
propagateShapeColorsToShape source shape = unsafePerformIO do
  sourceAttrs <- newIORef []
  withShapeFaces_ source $ \key -> do
    colorMap <- readIORef colorAttrsMap
    for_ (Map.lookup key colorMap) $ \attribute ->
      modifyIORef' sourceAttrs (attribute :)
  attrs <- reverse <$> readIORef sourceAttrs
  unless (null attrs) do
    index <- newIORef 0
    withShapeFaces_ shape $ \key -> do
      i <- atomicModifyIORef' index (\n -> (n + 1, n))
      modifyIORef' colorAttrsMap $ Map.insert key (attrs !! (i `mod` length attrs))
  pure shape

propagateSolidColors :: Solid -> Solid -> Solid
propagateSolidColors source solid = unsafePerformIO do
  sourceAttrs <- newIORef []
  withFaces_ source $ \key -> do
    colorMap <- readIORef colorAttrsMap
    for_ (Map.lookup key colorMap) $ \attribute ->
      modifyIORef' sourceAttrs (attribute :)
  attrs <- reverse <$> readIORef sourceAttrs
  unless (null attrs) do
    index <- newIORef 0
    withFaces_ solid $ \key -> do
      i <- atomicModifyIORef' index (\n -> (n + 1, n))
      modifyIORef' colorAttrsMap $ Map.insert key (attrs !! (i `mod` length attrs))
  pure solid

propagatePathEdgeColors :: OpC -> Path -> Path -> Path
propagatePathEdgeColors policy source path = unsafePerformIO do
  sourceAttrs <- newIORef []
  withPathEdges_ source $ \key -> do
    colorMap <- readIORef colorAttrsMap
    modifyIORef' sourceAttrs (Map.lookup key colorMap :)
  attrs <- reverse <$> readIORef sourceAttrs
  unless (null attrs) do
    index <- newIORef 0
    withPathEdges_ path $ \key -> do
      i <- atomicModifyIORef' index (\n -> (n + 1, n))
      let left = attrs !! (min i (length attrs - 1))
          right = attrs !! (min (i + 1) (length attrs - 1))
          result = case policy of
            OpConstC value -> value
            OpFC f -> f left right
      for_ result $ \value -> modifyIORef' colorAttrsMap (Map.insert key value)
  pure path

propagatePathColorsToShape :: Path -> Shape -> Shape
propagatePathColorsToShape path shape = unsafePerformIO do
  pathAttrs <- newIORef []
  withPathEdges_ path $ \key -> do
    colorMap <- readIORef colorAttrsMap
    for_ (Map.lookup key colorMap) $ \attribute ->
      modifyIORef' pathAttrs (attribute :)
  attrs <- reverse <$> readIORef pathAttrs
  unless (null attrs) do
    index <- newIORef 0
    let copyAttribute key = do
          i <- atomicModifyIORef' index (\n -> (n + 1, n))
          modifyIORef' colorAttrsMap $ Map.insert key (attrs !! (i `mod` length attrs))
    withShapeFaces_ shape copyAttribute
    withShapeEdges_ shape copyAttribute
  pure shape

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
    count <- atomicModifyIORef count (\a -> (succ <$> a <|> Just 0, a))
    let out = prefix ++ maybe "" show count ++ ".step"
    writeSTEPColor out solid
    return out

writeSTEPColor :: FilePath -> Solid -> IO ()
writeSTEPColor out solid = do
  doc <- newXCAFDoc
  colorMap <- readIORef colorAttrsMap
  facePayloads <- newIORef []
  withFaces_ solid $ \k ->
    for_ (Map.lookup k colorMap) \payload -> facePayloads $~ ((k, payload) :)
  addShapeWithFaceData doc solid =<< get facePayloads
  writeXCAFToSTEP out doc

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
  colorMap <- readIORef colorAttrsMap
  for_ solids \solid ->
    withModifiedFaces_ history solid $ \srcShapePtr srcLocHash dstShapePtr dstLocHash ->
      for_ (Map.lookup (srcShapePtr, srcLocHash) colorMap) \color ->
        modifyIORef' colorAttrsMap $ \m -> Map.insert (dstShapePtr, dstLocHash) color m

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

addShapeWithFaceData :: Ptr () -> Solid -> [(ColorKey, Note)] -> IO ()
addShapeWithFaceData doc solid faceData = do
  let toParts ((shapePtr, locHash), payload) =
        case payload of
          This note -> (shapePtr, locHash, 0 :: CInt, V3 0 0 0, note)
          That rgb -> (shapePtr, locHash, 1 :: CInt, rgb, "")
          These note rgb -> (shapePtr, locHash, 1 :: CInt, rgb, note)
      parts = map toParts faceData
      shapePtrs = map (\(p, _, _, _, _) -> p) parts
      locHashes = map (\(_, h, _, _, _) -> h) parts
      hasColors = map (\(_, _, hc, _, _) -> hc) parts
      colors = concatMap (\(_, _, _, V3 r g b, _) -> [r, g, b]) parts
      notes = map (\(_, _, _, _, n) -> n) parts
      n = fromIntegral (length faceData)

  withArray shapePtrs $ \faceShapePtrs ->
    withArray locHashes $ \faceLocHashes ->
      withArray hasColors $ \faceHasColors ->
        withArray colors $ \colorArr ->
          withMany withCString notes $ \noteCStrs ->
            withArray noteCStrs $ \noteArr ->
              [C.block| void {
    try {
      Handle(TDocStd_Document) docH((const TDocStd_Document*)$(void* doc));
      auto shapeTool = XCAFDoc_DocumentTool::ShapeTool(docH->Main());
      auto colorTool = XCAFDoc_DocumentTool::ColorTool(docH->Main());
      auto notesTool = XCAFDoc_DocumentTool::NotesTool(docH->Main());

      TDF_Label shapeLabel = shapeTool->AddShape(*$solid:solid, Standard_False);
      if (shapeLabel.IsNull()) {
        fprintf(stderr, "addShapeWithFaceData: AddShape returned null label\n");
        return;
      }

      void**       shapePtrs  = $(void** faceShapePtrs);
      size_t*      locHashes  = $(size_t* faceLocHashes);
      int*         hasColors  = $(int* faceHasColors);
      double*      colors     = $(double* colorArr);
      const char** notes      = $(const char** noteArr);

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
            fprintf(stderr, "addShapeWithFaceData: no face label for matched subshape\n");
            continue;
          }

          if (hasColors[i]) {
            Quantity_Color c(colors[i*3], colors[i*3+1], colors[i*3+2], Quantity_TOC_RGB);
            colorTool->SetColor(faceLabel, c, XCAFDoc_ColorSurf);
          }

          if (notes[i] != nullptr && notes[i][0] != '\0') {
            // Fallback for STEP export: persist note text as item name (NameMode),
            // in addition to XCAF notes metadata.
            TDataStd_Name::Set(faceLabel, TCollection_ExtendedString(notes[i]));

            auto aNote = notesTool->CreateComment(
              TCollection_ExtendedString(""),
              TCollection_ExtendedString(""),
              TCollection_ExtendedString(notes[i])
            );
            if (!aNote.IsNull()) {
              auto aRef = notesTool->AddNote(aNote->Label(), faceLabel);
              if (aRef.IsNull()) {
                fprintf(stderr, "addShapeWithFaceData: failed to attach note to face label\n");
              }
            } else {
              fprintf(stderr, "addShapeWithFaceData: failed to create note comment\n");
            }
          }


          break;
        }
      }
    } catch (const Standard_Failure& e) {
      fprintf(stderr, "addShapeWithFaceData: OCCT exception: %s\n", e.GetMessageString());
    } catch (...) {
      fprintf(stderr, "addShapeWithFaceData: unknown C++ exception\n");
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
      writer.Transfer(docH, STEPControl_AsIs);

      Handle(StepData_StepModel) model =
        Handle(StepData_StepModel)::DownCast(writer.Writer().WS()->Model());

      if (!model.IsNull()) {
        auto shapeTool = XCAFDoc_DocumentTool::ShapeTool(docH->Main());

        std::vector<TCollection_AsciiString> descLines;

        TDF_LabelSequence rootShapes;
        shapeTool->GetShapes(rootShapes);
        for (Standard_Integer r = 1; r <= rootShapes.Length(); ++r) {
          const TDF_Label root = rootShapes.Value(r);

          TDF_LabelSequence subLabels;
          XCAFDoc_ShapeTool::GetSubShapes(root, subLabels);
          for (Standard_Integer i = 1; i <= subLabels.Length(); ++i) {
            const TDF_Label subL = subLabels.Value(i);
            Handle(TDataStd_Name) nameAttr;
            if (!subL.FindAttribute(TDataStd_Name::GetID(), nameAttr) || nameAttr.IsNull()) {
              continue;
            }

            TCollection_AsciiString entry;
            TDF_Tool::Entry(subL, entry);
            TCollection_AsciiString note(nameAttr->Get());

            TCollection_AsciiString line(entry);
            line += ":";
            line += note;
            descLines.push_back(line);
          }
        }

        if (!descLines.empty()) {
          Handle(Interface_HArray1OfHAsciiString) descList =
            new Interface_HArray1OfHAsciiString(1, (Standard_Integer)descLines.size());
          for (Standard_Integer i = 1; i <= (Standard_Integer)descLines.size(); ++i) {
            descList->SetValue(i, new TCollection_HAsciiString(descLines[(size_t)i - 1]));
          }

          Handle(HeaderSection_FileDescription) fd =
            Handle(HeaderSection_FileDescription)::DownCast(
              model->HeaderEntity(STANDARD_TYPE(HeaderSection_FileDescription)));

          if (!fd.IsNull()) {
            fd->SetDescription(descList);
          }
        }
      }

      writer.Write($(const char* fp));
      docH->DecrementRefCounter();
    } catch (const Standard_Failure& e) {
      fprintf(stderr, "writeXCAFToSTEP: OCCT exception: %s\n", e.GetMessageString());
    } catch (...) {
      fprintf(stderr, "writeXCAFToSTEP: unknown C++ exception\n");
    }
  }|]

mkTaggedColor :: V3 CDouble -> ExpQ
mkTaggedColor color = [|$tagLoc . setColor color|]

mkTaggedColors :: V3 Word8 -> ExpQ
mkTaggedColors color = [|$tagLoc . setColors (fmap ((/ 255) . fromIntegral) <$> near color)|]

deriving instance Lift CDouble

{- ORMOLU_DISABLE -}
lightgray, gray, darkgray, yellow, gold, orange, pink, red,
  maroon, green, lime, darkgreen, skyblue, blue, darkblue,
  purple, violet, darkpurple, beige, brown, darkbrown, white,
  black, magenta, raywhite,
  lightgrays, grays, darkgrays, yellows, golds, oranges, pinks, reds,
  maroons, greens, limes, darkgreens, skyblues, blues, darkblues,
  purples, violets, darkpurples, beiges, browns, darkbrowns, whites,
  blacks, magentas, raywhites :: ExpQ
lightgray = mkTaggedColor (V3 200 200 200)
gray = mkTaggedColor (V3 130 130 130)
darkgray = mkTaggedColor (V3 80 80 80)
yellow = mkTaggedColor (V3 253 249 0)
gold = mkTaggedColor (V3 255 203 0)
orange = mkTaggedColor (V3 255 161 0)
pink = mkTaggedColor (V3 255 109 194)
red = mkTaggedColor (V3 230 41 55)
maroon = mkTaggedColor (V3 190 33 55)
green = mkTaggedColor (V3 0 228 48)
lime = mkTaggedColor (V3 0 158 47)
darkgreen = mkTaggedColor (V3 0 117 44)
skyblue = mkTaggedColor (V3 102 191 255)
blue = mkTaggedColor (V3 0 121 241)
darkblue = mkTaggedColor (V3 0 82 172)
purple = mkTaggedColor (V3 200 122 255)
violet = mkTaggedColor (V3 135 60 190)
darkpurple = mkTaggedColor (V3 112 31 126)
beige = mkTaggedColor (V3 211 176 131)
brown = mkTaggedColor (V3 127 106 79)
darkbrown = mkTaggedColor (V3 76 63 47)
white = mkTaggedColor (V3 255 255 255)
black = mkTaggedColor (V3 0 0 0)
magenta = mkTaggedColor (V3 255 0 255)
raywhite = mkTaggedColor (V3 245 245 245)
lightgrays = mkTaggedColors (V3 200 200 200)
grays = mkTaggedColors (V3 130 130 130)
darkgrays = mkTaggedColors (V3 80 80 80)
yellows = mkTaggedColors (V3 253 249 0)
golds = mkTaggedColors (V3 255 203 0)
oranges = mkTaggedColors (V3 255 161 0)
pinks = mkTaggedColors (V3 255 109 194)
reds = mkTaggedColors (V3 230 41 55)
maroons = mkTaggedColors (V3 190 33 55)
greens = mkTaggedColors (V3 0 228 48)
limes = mkTaggedColors (V3 0 158 47)
darkgreens = mkTaggedColors (V3 0 117 44)
skyblues = mkTaggedColors (V3 102 191 255)
blues = mkTaggedColors (V3 0 121 241)
darkblues = mkTaggedColors (V3 0 82 172)
purples = mkTaggedColors (V3 200 122 255)
violets = mkTaggedColors (V3 135 60 190)
darkpurples = mkTaggedColors (V3 112 31 126)
beiges = mkTaggedColors (V3 211 176 131)
browns = mkTaggedColors (V3 127 106 79)
darkbrowns = mkTaggedColors (V3 76 63 47)
whites = mkTaggedColors (V3 255 255 255)
blacks = mkTaggedColors (V3 0 0 0)
magentas = mkTaggedColors (V3 255 0 255)
raywhites = mkTaggedColors (V3 245 245 245)
{- ORMOLU_ENABLE -}
