-- | Query solid topology by face index.
module Rapids.Query
  ( selectFacesByIndex,
  )
where

import Foreign.C.Types (CLLong)
import Foreign.Marshal.Array (withArray)
import InlineOCCT (occtContext, ownSolid)
import qualified Language.C.Inline.Cpp as Cpp
import Waterfall.Internal.Solid (Solid)

Cpp.context occtContext
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp.hxx>"
Cpp.include "<TopTools_IndexedMapOfShape.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"

-- | Select 1-based face indices into a compound. Positive indices select only
-- those faces; negative indices exclude their absolute values. With only
-- negative indices, all faces except those indices are selected. Out-of-range
-- indices are ignored.
selectFacesByIndex :: [Int] -> Solid -> Solid
selectFacesByIndex indices solid =
  let included = [fromIntegral index :: CLLong | index <- indices, index > 0]
      excluded =
        [ fromInteger magnitude :: CLLong
          | index <- indices,
            index < 0,
            let magnitude = negate (toInteger index),
            magnitude <= toInteger (maxBound :: CLLong)
        ]
      includeAll = if null included then 1 else 0
      lengthIncluded = fromIntegral $ length included
      lengthExcluded = fromIntegral $ length excluded
   in ownSolid $
        withArray included $ \includedPtr ->
          withArray excluded $ \excludedPtr ->
            [Cpp.block| TopoDS_Shape* {
              TopoDS_Compound result;
              BRep_Builder builder;
              builder.MakeCompound(result);

              TopoDS_Shape* input = $solid:solid;
              if (input == nullptr || input->IsNull()) {
                return new TopoDS_Shape(result);
              }

              TopTools_IndexedMapOfShape faces;
              TopExp::MapShapes(*input, TopAbs_FACE, faces);
              const long long* included = (const long long*)$(long long* includedPtr);
              const long long* excluded = (const long long*)$(long long* excludedPtr);
              const int includedCount = $(int lengthIncluded);
              const int excludedCount = $(int lengthExcluded);
              const bool includeAll = $(int includeAll) != 0;

              for (int faceIndex = 1; faceIndex <= faces.Extent(); ++faceIndex) {
                bool selected = includeAll;
                for (int i = 0; i < includedCount; ++i) {
                  if (included[i] == faceIndex) {
                    selected = true;
                    break;
                  }
                }
                if (!includeAll && !selected) {
                  continue;
                }
                bool isExcluded = false;
                for (int i = 0; i < excludedCount; ++i) {
                  if (excluded[i] == faceIndex) {
                    isExcluded = true;
                    break;
                  }
                }
                if (!isExcluded) {
                  builder.Add(result, faces(faceIndex));
                }
              }
              return new TopoDS_Shape(result);
            } |]
