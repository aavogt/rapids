module InlineOCCT (module InlineOCCT.Context, module InlineOCCT) where

import InlineOCCT.Context
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

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

