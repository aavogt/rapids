module InlineOCCT (module InlineOCCT.Context, module InlineOCCT) where

import InlineOCCT.Context
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified OpenCascade.TopoDS as TopoDS
import Foreign
import Waterfall
import Waterfall.Internal.Path
import Waterfall.TwoD.Internal.Shape
import Waterfall.Internal.Path.Common (RawPath(ComplexRawPath))
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Data.Acquire (mkAcquire)

C.context occtContext
Cpp.include "<gp_Pnt.hxx>"

-- these don't really belong here
-- they are referenced in InlineOCCT.Context
-- which defines occtContext which is needed to compile these
c_newGpPntVector count p = [Cpp.block| void* {
  std::vector<gp_Pnt>* points = new std::vector<gp_Pnt>();
  int count = $(int count);
  if (count > 0) {
    points->reserve((size_t)count);
  }

  double * coords = $(double *p);
  for (int i = 0; i < count; ++i) {
    double x = coords[3 * i + 0];
    double y = coords[3 * i + 1];
    double z = coords[3 * i + 2];
    points->emplace_back(x, y, z);
  }

  return points;
} |]

c_deleteGpPntVector ptr = [Cpp.block| void {
  std::vector<gp_Pnt>* points = (std::vector<gp_Pnt>*)$(void *ptr);
  delete points;
} |]

c_deleteTopoDSWire :: Ptr TopoDS.Wire -> IO ()
c_deleteTopoDSWire (castPtr -> ptr) = [Cpp.block| void { delete (TopoDS_Wire*)$(void* ptr); } |]

c_deleteTopoDSShape :: Ptr TopoDS.Shape -> IO ()
c_deleteTopoDSShape (castPtr -> ptr) = [Cpp.block| void { delete (TopoDS_Shape*)$(void* ptr); } |]


ownPath :: IO (Ptr TopoDS.Wire) -> Path
ownPath io = Path $ ComplexRawPath $ unsafeFromAcquire $ mkAcquire io c_deleteTopoDSWire

ownShape :: IO (Ptr TopoDS.Shape) -> Shape
ownShape io = Shape $ unsafeFromAcquire $ mkAcquire io c_deleteTopoDSShape

