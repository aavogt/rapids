{-# LANGUAGE OverloadedLabels #-}

-- | Find solid topology by face colors.
module Rapids.Color.Query
  ( ColorQueryResult,
    colorQuery,
    colorQueryFromString,
    findPathByColors,
    findVertexByColors,
  )
where

import Data.Char (isHexDigit)
import Data.Coerce (coerce)
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.These (These (..))
import Data.Word (Word8)
import Foreign.C (CDouble, CInt, CSize)
import Foreign.Marshal.Array (withArray)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import InlineOCCT (occtContext, ownPath)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as Cpp
import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Linear (V3 (..))
import Numeric (readHex)
import Rapids.Color (ColorKey, Note (..), colorAttrsMap)
import qualified System.IO.Unsafe
import Waterfall (Path)
import Waterfall.Internal.Solid (Solid)

Cpp.context (occtContext <> C.funCtx)
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopTools_IndexedDataMapOfShapeListOfShape.hxx>"
Cpp.include "<TopTools_IndexedMapOfShape.hxx>"
Cpp.include "<TopTools_ListIteratorOfListOfShape.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Face.hxx>"
Cpp.include "<TopoDS_Vertex.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<TopoDS_Edge.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"

-- | Select the path made from an edge whose incident face colors match the
-- query. One color selects every edge of that face; two colors select their
-- common edge.
findPathByColors :: String -> Solid -> Path
findPathByColors colors solid = findPathByParsed (parseColors colors) solid

findPathByParsed :: Maybe [RGB] -> Solid -> Path
findPathByParsed Nothing _ = mempty
findPathByParsed (Just wanted) solid = System.IO.Unsafe.unsafePerformIO $ do
  incidents <- newIORef Map.empty
  let emit edgePtr edgeHash facePtr faceHash =
        modifyIORef' incidents $
          Map.insertWith
            (++)
            (edgePtr, edgeHash)
            [(facePtr, faceHash)]
  [Cpp.block| void {
    TopTools_IndexedMapOfShape edges;
    TopTools_IndexedDataMapOfShapeListOfShape ancestors;
    TopExp::MapShapes(*$solid:solid, TopAbs_EDGE, edges);
    TopExp::MapShapesAndAncestors(*$solid:solid, TopAbs_EDGE, TopAbs_FACE, ancestors);
    for (int i = 1; i <= edges.Extent(); ++i) {
      const TopoDS_Shape& edge = edges(i);
      const TopTools_ListOfShape& faces = ancestors.FindFromKey(edge);
      void* edgePtr = (void*)edge.TShape().get();
      size_t edgeHash = edge.Location().HashCode();
      for (TopTools_ListIteratorOfListOfShape it(faces); it.More(); it.Next()) {
        const TopoDS_Face face = TopoDS::Face(it.Value());
        $fun:(void (*emit)(void*, size_t, void*, size_t))
          (edgePtr, edgeHash, (void*)face.TShape().get(),
           face.Location().HashCode());
      }
    }
  } |]
  attrs <- readIORef colorAttrsMap
  entries <- readIORef incidents
  case Map.keys (Map.filter (matchesEdges wanted attrs) entries) of
    [] -> pure mempty
    edgeKeys -> do
      let edgePtrs = map fst edgeKeys
          edgeCount = fromIntegral (length edgePtrs) :: CInt
      pure $ ownPath $ withArray edgePtrs $ \edgeArray ->
        [Cpp.block| TopoDS_Wire* {
        void** edges = $(void** edgeArray);
        const int edgeCount = $(int edgeCount);
        BRepBuilderAPI_MakeWire builder;
        for (int i = 0; i < edgeCount; ++i) {
          TopoDS_Edge* edge = (TopoDS_Edge*)edges[i];
          if (edge != nullptr && !edge->IsNull()) builder.Add(*edge);
        }
        if (!builder.IsDone()) return nullptr;
        return new TopoDS_Wire(builder.Wire());
      } |]
  where
    matchesEdges wanted attrs keys = case traverse (lookupColor attrs) keys of
      Nothing -> False
      Just actual -> case wanted of
        [one] -> one `elem` actual
        _ -> List.sort wanted == List.sort actual

-- | Result type selected by the expected type of a color query.
class ColorQueryResult result where
  runColorQuery :: [RGB] -> result

instance ColorQueryResult (Solid -> V3 Double) where
  runColorQuery wanted = coerce . unsafeFind (Just wanted)

instance ColorQueryResult (Solid -> Path) where
  runColorQuery wanted = findPathByParsed (Just wanted)

parseColorsOrError :: String -> [RGB]
parseColorsOrError input = case parseColors input of
  Just colors -> colors
  Nothing -> error "colorQuery expects one or more six-digit hexadecimal RGB colors"

colorQueryFromString :: (ColorQueryResult result) => String -> result
colorQueryFromString input = runColorQuery (parseColorsOrError input)

-- | The color string is a concatenation of six-digit RGB values, in the
-- order in which OpenCascade reports the incident faces. A missing match
-- returns the origin, matching the useful total-function shape of an
-- overloaded label.
findVertexByColors :: String -> Solid -> V3 Double
findVertexByColors colors solid = coerce $ unsafeFind parsed solid
  where
    parsed = parseColors colors

unsafeFind :: Maybe [RGB] -> Solid -> V3 CDouble
unsafeFind Nothing _ = V3 0 0 0
unsafeFind (Just wanted) solid = System.IO.Unsafe.unsafePerformIO $ do
  incidents <- newIORef Map.empty
  let emit x y z ptr hash =
        modifyIORef' incidents $
          Map.insertWith
            (++)
            (V3 x y z)
            [(ptr, hash)]
  [Cpp.block| void {
    TopTools_IndexedMapOfShape vertices;
    TopTools_IndexedDataMapOfShapeListOfShape ancestors;
    TopExp::MapShapes(*$solid:solid, TopAbs_VERTEX, vertices);
    TopExp::MapShapesAndAncestors(*$solid:solid, TopAbs_VERTEX, TopAbs_FACE, ancestors);
    for (int i = 1; i <= vertices.Extent(); ++i) {
      const TopoDS_Vertex vertex = TopoDS::Vertex(vertices(i));
      const gp_Pnt point = BRep_Tool::Pnt(vertex);
      const TopTools_ListOfShape& faces = ancestors.FindFromKey(vertices(i));
      for (TopTools_ListIteratorOfListOfShape it(faces); it.More(); it.Next()) {
        const TopoDS_Face face = TopoDS::Face(it.Value());
        void* shapePtr = (void*)face.TShape().get();
        size_t locHash = face.Location().HashCode();
        $fun:(void (*emit)(double, double, double, void*, size_t))
          (point.X(), point.Y(), point.Z(), shapePtr, locHash);
      }
    }
  } |]
  entries <- readIORef incidents
  attrs <- readIORef colorAttrsMap
  pure $
    maybe (V3 0 0 0) (fst . fst) $
      Map.minViewWithKey $
        Map.filter (matches wanted attrs) entries
  where
    matches wanted attrs keys = traverse (lookupColor attrs) keys == Just wanted

lookupColor :: Map.Map ColorKey Note -> ColorKey -> Maybe RGB
lookupColor attrs key = do
  note <- Map.lookup key attrs
  color <- case note of
    That c -> Just c
    These _ c -> Just c
    This _ -> Nothing
  let V3 r g b = color
  pure (toByte r, toByte g, toByte b)
  where
    toByte x = round (255 * x)

type RGB = (Word8, Word8, Word8)

parseColors :: String -> Maybe [RGB]
parseColors input
  | null input || length input `mod` 6 /= 0 = Nothing
  | otherwise = traverse parseOne (chunksOf 6 input)
  where
    parseOne [r1, r2, g1, g2, b1, b2]
      | all isHexDigit [r1, r2, g1, g2, b1, b2] = do
          r <- byte [r1, r2]
          g <- byte [g1, g2]
          b <- byte [b1, b2]
          pure (r, g, b)
    parseOne _ = Nothing

    byte digits = case readHex digits of
      [(n, "")] -> Just n
      _ -> Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

-- | Overloaded-label spelling of 'findVertexByColors'.
instance (KnownSymbol value) => IsLabel value (Solid -> V3 Double) where
  fromLabel = findVertexByColors (symbolVal (Proxy @value))

-- | The number of colors determines the result (V3 Double for >=3 colors otherwise return a Path)
-- This is usually correct. The vertex on a cylinder has 2 faces, but the producer of the string
-- (OOCT_XCAF_FacePicker) either has to list the same color twice, or there
-- could be something else to signal the result type
--
-- > [colorQuery|027bf20279f00177ef007af1|] :: Solid -> V3 Double
-- > [colorQuery|027bf2|] :: Solid -> Path
colorQuery :: QuasiQuoter
colorQuery =
  QuasiQuoter
    { quoteExp = \input ->
        case (validColorString input, length input `div` 6) of
          (True, n) | n < 3 -> [|colorQueryFromString input :: Solid -> Path |]
          (True, _) -> [|colorQueryFromString input :: Solid -> V3 Double |]
          _ -> fail "colorQuery expects one or more six-digit hexadecimal RGB colors",
      quotePat = const $ fail "colorQuery is expression-only",
      quoteType = const $ fail "colorQuery is expression-only",
      quoteDec = const $ fail "colorQuery is expression-only"
    }
  where
    validColorString input = case parseColors input of
      Just _ -> True
      Nothing -> False
