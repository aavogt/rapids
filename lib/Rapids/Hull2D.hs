{-# LANGUAGE QuasiQuotes #-}

-- | Convex hulls of planar paths.
module Rapids.Hull2D
  ( hull2D,
  )
where

import InlineOCCT (occtContext, ownPath)
import qualified Language.C.Inline.Cpp as Cpp
import Waterfall (Path)

Cpp.context occtContext
Cpp.include "<BRepAdaptor_Curve.hxx>"
Cpp.include "<BRepBuilderAPI_MakeEdge.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Edge.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<Geom_Curve.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<algorithm>"
Cpp.include "<cmath>"
Cpp.include "<vector>"

-- | Build a convex hull wire for a finite path in the XY plane. Each edge is
-- sampled at 2,000 points; hull arcs retain their source curves and other
-- boundary spans become straight edges.
hull2D :: Path -> Path
hull2D input = ownPath [Cpp.block| TopoDS_Wire* {
  TopoDS_Wire* wire = $path:input;
  if (wire == nullptr || wire->IsNull()) {
    return nullptr;
  }

  struct EdgeData {
    Handle(Geom_Curve) curve;
    Standard_Real last;
    bool closed;
  };
  struct Sample {
    gp_Pnt point;
    Standard_Real parameter;
    int edgeIndex;
    int sampleIndex;
  };

  const int sampleCount = 2000;
  std::vector<EdgeData> edges;
  std::vector<Sample> samples;
  TopExp_Explorer edgeExplorer(*wire, TopAbs_EDGE);
  for (; edgeExplorer.More(); edgeExplorer.Next()) {
    TopoDS_Edge edge = TopoDS::Edge(edgeExplorer.Current());
    BRepAdaptor_Curve curveAdaptor(edge);
    Standard_Real first = curveAdaptor.FirstParameter();
    Standard_Real last = curveAdaptor.LastParameter();
    const bool closed = curveAdaptor.IsClosed();
    const int edgeIndex = static_cast<int>(edges.size());
    edges.push_back({BRep_Tool::Curve(edge, first, last), last, closed});

    for (int i = 0; i < sampleCount; ++i) {
      const Standard_Real fraction = closed
        ? static_cast<Standard_Real>(i) / sampleCount
        : static_cast<Standard_Real>(i) / (sampleCount - 1);
      const Standard_Real parameter = first + (last - first) * fraction;
      samples.push_back({curveAdaptor.Value(parameter), parameter, edgeIndex, i});
    }
  }
  if (samples.empty()) {
    return nullptr;
  }

  std::vector<size_t> sorted(samples.size());
  for (size_t i = 0; i < sorted.size(); ++i) {
    sorted[i] = i;
  }
  std::sort(sorted.begin(), sorted.end(), [&samples](size_t a, size_t b) {
    const gp_Pnt& p = samples[a].point;
    const gp_Pnt& q = samples[b].point;
    if (p.X() != q.X()) return p.X() < q.X();
    if (p.Y() != q.Y()) return p.Y() < q.Y();
    if (samples[a].edgeIndex != samples[b].edgeIndex) return samples[a].edgeIndex < samples[b].edgeIndex;
    return samples[a].sampleIndex < samples[b].sampleIndex;
  });

  std::vector<size_t> unique;
  unique.reserve(sorted.size());
  for (size_t index : sorted) {
    if (unique.empty() ||
        samples[unique.back()].point.X() != samples[index].point.X() ||
        samples[unique.back()].point.Y() != samples[index].point.Y()) {
      unique.push_back(index);
    }
  }
  if (unique.size() < 2) {
    return nullptr;
  }

  auto turn = [&samples](size_t a, size_t b, size_t c) {
    const gp_Pnt& p = samples[a].point;
    const gp_Pnt& q = samples[b].point;
    const gp_Pnt& r = samples[c].point;
    return (q.X() - p.X()) * (r.Y() - q.Y()) - (q.Y() - p.Y()) * (r.X() - q.X());
  };
  std::vector<size_t> lower;
  for (size_t index : unique) {
    while (lower.size() >= 2 && turn(lower[lower.size() - 2], lower.back(), index) <= 0.0) {
      lower.pop_back();
    }
    lower.push_back(index);
  }
  std::vector<size_t> upper;
  for (auto it = unique.rbegin(); it != unique.rend(); ++it) {
    while (upper.size() >= 2 && turn(upper[upper.size() - 2], upper.back(), *it) <= 0.0) {
      upper.pop_back();
    }
    upper.push_back(*it);
  }
  lower.pop_back();
  upper.pop_back();
  lower.insert(lower.end(), upper.begin(), upper.end());
  const std::vector<size_t>& hull = lower;

  auto adjacentOnEdge = [&edges, sampleCount](const Sample& a, const Sample& b) {
    if (a.edgeIndex != b.edgeIndex) return false;
    const int difference = std::abs(a.sampleIndex - b.sampleIndex);
    return difference == 1 ||
      (edges[a.edgeIndex].closed && difference == sampleCount - 1);
  };

  BRepBuilderAPI_MakeWire result;
  const size_t segmentCount = hull.size() == 2 ? 1 : hull.size();
  for (size_t i = 0; i < segmentCount;) {
    const Sample& start = samples[hull[i]];
    const Sample& end = samples[hull[(i + 1) % hull.size()]];
    size_t runEnd = i + 1;
    const bool trimCurve = adjacentOnEdge(start, end) && !edges[start.edgeIndex].curve.IsNull();

    if (trimCurve && i + 1 < hull.size() &&
        std::abs(start.sampleIndex - end.sampleIndex) == 1) {
      const int direction = end.sampleIndex - start.sampleIndex;
      while (runEnd + 1 < hull.size()) {
        const Sample& a = samples[hull[runEnd]];
        const Sample& b = samples[hull[runEnd + 1]];
        if (a.edgeIndex != start.edgeIndex || b.edgeIndex != start.edgeIndex ||
            b.sampleIndex - a.sampleIndex != direction) {
          break;
        }
        ++runEnd;
      }
    }

    const Sample& runEndSample = samples[hull[runEnd % hull.size()]];
    TopoDS_Edge outputEdge;
    if (trimCurve) {
      const EdgeData& edge = edges[start.edgeIndex];
      Standard_Real first = start.parameter;
      Standard_Real last = runEndSample.parameter;
      if (edge.closed && std::abs(start.sampleIndex - runEndSample.sampleIndex) == sampleCount - 1) {
        if (start.sampleIndex == sampleCount - 1) {
          last = edge.last;
        } else {
          first = edge.last;
        }
      }
      if (first <= last) {
        BRepBuilderAPI_MakeEdge edgeBuilder(edge.curve, first, last);
        if (!edgeBuilder.IsDone()) return nullptr;
        outputEdge = edgeBuilder.Edge();
      } else {
        BRepBuilderAPI_MakeEdge edgeBuilder(edge.curve, last, first);
        if (!edgeBuilder.IsDone()) return nullptr;
        outputEdge = edgeBuilder.Edge();
        outputEdge.Reverse();
      }
    } else {
      BRepBuilderAPI_MakeEdge edgeBuilder(start.point, end.point);
      if (!edgeBuilder.IsDone()) return nullptr;
      outputEdge = edgeBuilder.Edge();
    }
    result.Add(outputEdge);
    i = trimCurve ? runEnd : i + 1;
  }

  if (!result.IsDone()) {
    return nullptr;
  }
  return new TopoDS_Wire(result.Wire());
} |]
