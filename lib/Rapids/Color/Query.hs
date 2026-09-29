{-# LANGUAGE OverloadedLabels #-}

-- | Find a solid vertex by the colors of its incident faces.
module Rapids.Color.Query
  ( findVertexByColors,
    colorQuery,
  )
where

import Data.Char (isHexDigit)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import InlineOCCT (occtContext)
import qualified Language.C.Inline.Cpp as Cpp
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Linear (V3 (..))
import Numeric (readHex)
import Rapids.Color (ColorKey, Note (..), colorAttrsMap)
import qualified System.IO.Unsafe
import Waterfall.Internal.Solid (Solid)
import qualified Language.C.Inline.Context as C
import Data.These
import Foreign.C (CDouble)
import Data.Coerce

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
Cpp.include "<gp_Pnt.hxx>"

-- | The color string is a concatenation of six-digit RGB values, in the
-- order in which OpenCascade reports the incident faces.  A missing match
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
  let
    emit x y z ptr hash =
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

-- | Quasiquoted spelling of 'findVertexByColors'.
--
-- > [colorQuery|027bf20279f00177ef007af1|] :: Solid -> V3 Double
colorQuery :: QuasiQuoter
colorQuery =
  QuasiQuoter
    { quoteExp = \input ->
        if validColorString input
          then [|findVertexByColors input|]
          else fail "colorQuery expects one or more six-digit hexadecimal RGB colors",
      quotePat = const $ fail "colorQuery is expression-only",
      quoteType = const $ fail "colorQuery is expression-only",
      quoteDec = const $ fail "colorQuery is expression-only"
    }
  where
    validColorString input = case parseColors input of
      Just _ -> True
      Nothing -> False
