module Rapids.ConvexHull where

import Data.Acquire
import Data.Maybe (mapMaybe)
import Foreign
import Foreign.C.Types
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear (V3 (..))
import OpenCascade.TopoDS (Shape)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Waterfall (Path)
import Waterfall.Internal.Finalizers
import qualified Waterfall.Internal.Path as InternalPath
import Waterfall.Internal.Path.Common (RawPath (..), rawPathWire)
import Waterfall.Internal.Solid

C.context occtContext
Cpp.include "libqhull_r/libqhull_r.h"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepBuilderAPI_MakePolygon.hxx>"
Cpp.include "<BRepBuilderAPI_MakeSolid.hxx>"
Cpp.include "<BRepBuilderAPI_Sewing.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Shell.hxx>"
Cpp.include "<TopoDS_Vertex.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<vector>"

[Cpp.emitBlock|
static TopoDS_Shape* rapids_convex_hull_from_points(const std::vector<gp_Pnt>& occtPoints, const TopoDS_Shape* fallbackShape) {
  int numPoints = (int)occtPoints.size();
  if (numPoints < 4) {
    if (fallbackShape != nullptr) {
      return new TopoDS_Shape(*fallbackShape);
    }
    return new TopoDS_Shape();
  }

  std::vector<coordT> qhCoords;
  qhCoords.reserve((size_t)numPoints * 3);
  for (const gp_Pnt& p : occtPoints) {
    qhCoords.push_back((coordT)p.X());
    qhCoords.push_back((coordT)p.Y());
    qhCoords.push_back((coordT)p.Z());
  }

  gp_Pnt hullCenter(0.0, 0.0, 0.0);
  for (const gp_Pnt& p : occtPoints) {
    hullCenter.SetX(hullCenter.X() + p.X());
    hullCenter.SetY(hullCenter.Y() + p.Y());
    hullCenter.SetZ(hullCenter.Z() + p.Z());
  }
  hullCenter.SetX(hullCenter.X() / numPoints);
  hullCenter.SetY(hullCenter.Y() / numPoints);
  hullCenter.SetZ(hullCenter.Z() / numPoints);

  qhT qh_qh;
  qhT *qh = &qh_qh;
  qh_zero(qh, stderr);

  int exitcode = qh_new_qhull(
    qh,
    3,
    numPoints,
    qhCoords.data(),
    False,
    (char*)"qhull Qt",
    NULL,
    stderr
  );

  if (exitcode != qh_ERRnone) {
    int curlong = 0;
    int totlong = 0;
    qh_freeqhull(qh, !qh_ALL);
    qh_memfreeshort(qh, &curlong, &totlong);
    if (fallbackShape != nullptr) {
      return new TopoDS_Shape(*fallbackShape);
    }
    return new TopoDS_Shape();
  }

  BRepBuilderAPI_Sewing sewing(1.0e-7);

  facetT *facet;
  vertexT *vertex, **vertexp;
  FORALLfacets {
    if (facet->upperdelaunay) {
      continue;
    }

    std::vector<int> ids;
    FOREACHvertex_(facet->vertices) {
      int id = qh_pointid(qh, vertex->point);
      if (id >= 0 && id < numPoints) {
        ids.push_back(id);
      }
    }

    if (ids.size() != 3) {
      continue;
    }

    const gp_Pnt& p0 = occtPoints[ids[0]];
    const gp_Pnt& p1 = occtPoints[ids[1]];
    const gp_Pnt& p2 = occtPoints[ids[2]];

    double ux = p1.X() - p0.X();
    double uy = p1.Y() - p0.Y();
    double uz = p1.Z() - p0.Z();
    double vx = p2.X() - p0.X();
    double vy = p2.Y() - p0.Y();
    double vz = p2.Z() - p0.Z();

    double nx = uy * vz - uz * vy;
    double ny = uz * vx - ux * vz;
    double nz = ux * vy - uy * vx;

    double cx = (p0.X() + p1.X() + p2.X()) / 3.0;
    double cy = (p0.Y() + p1.Y() + p2.Y()) / 3.0;
    double cz = (p0.Z() + p1.Z() + p2.Z()) / 3.0;

    double toCenterX = hullCenter.X() - cx;
    double toCenterY = hullCenter.Y() - cy;
    double toCenterZ = hullCenter.Z() - cz;

    int i0 = ids[0];
    int i1 = ids[1];
    int i2 = ids[2];
    double inwardDot = nx * toCenterX + ny * toCenterY + nz * toCenterZ;
    if (inwardDot > 0.0) {
      i1 = ids[2];
      i2 = ids[1];
    }

    BRepBuilderAPI_MakePolygon poly;
    poly.Add(occtPoints[i0]);
    poly.Add(occtPoints[i1]);
    poly.Add(occtPoints[i2]);
    poly.Close();

    if (!poly.IsDone()) {
      continue;
    }

    TopoDS_Wire wire = poly.Wire();
    BRepBuilderAPI_MakeFace mf(wire);
    if (!mf.IsDone()) {
      continue;
    }

    sewing.Add(mf);
  }

  sewing.Perform();
  TopoDS_Shape sewed = sewing.SewedShape();

  BRepBuilderAPI_MakeSolid ms;
  TopExp_Explorer shellEx(sewed, TopAbs_SHELL);
  for (; shellEx.More(); shellEx.Next()) {
    ms.Add(TopoDS::Shell(shellEx.Current()));
  }

  int curlong = 0;
  int totlong = 0;
  qh_freeqhull(qh, !qh_ALL);
  qh_memfreeshort(qh, &curlong, &totlong);

  if (!ms.IsDone()) {
    return new TopoDS_Shape(sewed);
  }

  return new TopoDS_Shape(ms);
}
|]

solidFromShape :: IO (Ptr Shape) -> Solid
solidFromShape newShape = Solid $ unsafeFromAcquire (mkAcquire newShape deleteShape)

-- | produce convex hull of the vertices within the argument
class Hull a where
  hull :: a -> Solid

instance Hull [V3 Double] where
  hull = pointsHull

instance Hull [Path] where
  hull = pathVerticesHull

instance Hull Path where
  hull = pathVerticesHull . (: [])

instance Hull Solid where
  hull = solidVerticesHull

-- | convex hull of a set of points
pointsHull :: [V3 Double] -> Solid
pointsHull points = solidFromShape
  [Cpp.block| TopoDS_Shape* {
    std::vector<gp_Pnt>* occtPoints = (std::vector<gp_Pnt>*)$pnts:points;
    return rapids_convex_hull_from_points(*occtPoints, nullptr);
  } |]

pathVerticesHull :: [Path] -> Solid
pathVerticesHull paths = solidFromShape $ withArray wirePtrs $ \wiresPtr ->
  [Cpp.block| TopoDS_Shape* {
    int numWires = $(int numWires);
    void** wires = $(void** wiresPtr);

    std::vector<gp_Pnt>* singlePointVec = (std::vector<gp_Pnt>*)$pnts:singlePoints;

    std::vector<gp_Pnt> occtPoints;
    occtPoints.insert(occtPoints.end(), singlePointVec->begin(), singlePointVec->end());

    for (int i = 0; i < numWires; ++i) {
      TopoDS_Wire* wire = (TopoDS_Wire*)wires[i];
      if (wire == nullptr) {
        continue;
      }

      TopExp_Explorer ex(*wire, TopAbs_VERTEX);
      for (; ex.More(); ex.Next()) {
        TopoDS_Vertex v = TopoDS::Vertex(ex.Current());
        gp_Pnt p = BRep_Tool::Pnt(v);
        occtPoints.push_back(p);
      }
    }

    return rapids_convex_hull_from_points(occtPoints, nullptr);
  } |]
  where
    wirePtrs :: [Ptr ()]
    wirePtrs = mapMaybe pathWirePtr paths

    numWires :: CInt
    numWires = fromIntegral (length wirePtrs)

    singlePoints :: [V3 Double]
    singlePoints = mapMaybe pathSinglePoint paths

pathWirePtr :: Path -> Maybe (Ptr ())
pathWirePtr (InternalPath.Path raw) = castPtr <$> rawPathWire raw

pathSinglePoint :: Path -> Maybe (V3 Double)
pathSinglePoint (InternalPath.Path (SinglePointRawPath p)) = Just p
pathSinglePoint _ = Nothing

-- | convex hull of a solid's vertices
solidVerticesHull :: Solid -> Solid
solidVerticesHull solid = solidFromShape
  [Cpp.block| TopoDS_Shape* {
    std::vector<gp_Pnt> occtPoints;

    TopExp_Explorer ex(* $solid:solid, TopAbs_VERTEX);
    for (; ex.More(); ex.Next()) {
      TopoDS_Vertex v = TopoDS::Vertex(ex.Current());
      gp_Pnt p = BRep_Tool::Pnt(v);
      occtPoints.push_back(p);
    }

    const TopoDS_Shape* fallbackShape = $solid:solid;
    return rapids_convex_hull_from_points(occtPoints, fallbackShape);
  } |]

