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
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<Geom_Curve.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<algorithm>"
Cpp.include "<cmath>"
Cpp.include "<vector>"
Cpp.include "<Precision.hxx>"

-- | Build a convex hull wire for a finite path in the XY plane. Each edge
-- uses 64 seed samples; Newton refines bitangent contacts where it converges.
-- Retained curve arcs remain exact, and other spans are straight edges.
hull2D :: Path -> Path
hull2D input = ownPath [Cpp.block| TopoDS_Wire* {
  TopoDS_Wire* wire = $path:input;
  if (wire == nullptr || wire->IsNull()) {
    return nullptr;
  }

  struct EdgeData {
    TopoDS_Edge edge;
    Handle(Geom_Curve) curve;
    Standard_Real first;
    Standard_Real last;
    bool closed;
  };
  struct Sample {
    gp_Pnt point;
    Standard_Real parameter;
    int edgeIndex;
    int sampleIndex;
  };

  const int sampleCount = 64;
  std::vector<EdgeData> edges;
  std::vector<Sample> samples;
  TopExp_Explorer edgeExplorer(*wire, TopAbs_EDGE);
  for (; edgeExplorer.More(); edgeExplorer.Next()) {
    TopoDS_Edge edge = TopoDS::Edge(edgeExplorer.Current());
    BRepAdaptor_Curve curveAdaptor(edge);
    Standard_Real first = curveAdaptor.FirstParameter();
    Standard_Real last = curveAdaptor.LastParameter();
    const bool closed = curveAdaptor.IsClosed();
    Handle(Geom_Curve) curve = BRep_Tool::Curve(edge, first, last);
    const int edgeIndex = static_cast<int>(edges.size());
    edges.push_back({edge, curve, first, last, closed});

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
    if (!unique.empty() &&
        samples[unique.back()].point.Distance(samples[index].point) <= Precision::Confusion()) {
      continue;
    }
    unique.push_back(index);
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
  const size_t segmentCount = hull.size() == 2 ? 1 : hull.size();

  auto cross2 = [](const gp_Vec& a, const gp_Vec& b) {
    return a.X() * b.Y() - a.Y() * b.X();
  };
  auto adjacentDirection = [&edges, sampleCount](const Sample& a, const Sample& b) {
    if (a.edgeIndex != b.edgeIndex) return 0;
    const int difference = std::abs(a.sampleIndex - b.sampleIndex);
    if (difference == 1) return b.sampleIndex > a.sampleIndex ? 1 : -1;
    if (edges[a.edgeIndex].closed && difference == sampleCount - 1) {
      return a.sampleIndex == sampleCount - 1 ? 1 : -1;
    }
    return 0;
  };
  auto curveArc = [&edges, &cross2, &adjacentDirection](
                    const Sample& start, const Sample& end,
                    Standard_Real first, Standard_Real last) {
    const int direction = adjacentDirection(start, end);
    if (direction == 0 || edges[start.edgeIndex].curve.IsNull()) return false;
    try {
      BRepAdaptor_Curve adaptor(edges[start.edgeIndex].edge);
      gp_Pnt p0, p1;
      gp_Vec tangent0, tangent1;
      adaptor.D1(first, p0, tangent0);
      adaptor.D1(last, p1, tangent1);
      const gp_Vec chord(p0, p1);
      return direction * cross2(tangent0, chord) > 0.0 &&
             direction * cross2(chord, tangent1) > 0.0;
    } catch (Standard_Failure const&) {
      return false;
    }
  };
  auto solveBitangent = [&edges, sampleCount, &cross2](
                          const Sample& start, const Sample& end,
                          Standard_Real& solvedStart, Standard_Real& solvedEnd) {
    const EdgeData& edge0 = edges[start.edgeIndex];
    const EdgeData& edge1 = edges[end.edgeIndex];
    if (edge0.curve.IsNull() || edge1.curve.IsNull()) return false;
    if ((edge0.closed && (start.sampleIndex == 0 || start.sampleIndex == sampleCount - 1)) ||
        (edge1.closed && (end.sampleIndex == 0 || end.sampleIndex == sampleCount - 1))) {
      return false;
    }

    Standard_Real s = start.parameter;
    Standard_Real t = end.parameter;
    const Standard_Real lowS = std::min(edge0.first, edge0.last);
    const Standard_Real highS = std::max(edge0.first, edge0.last);
    const Standard_Real lowT = std::min(edge1.first, edge1.last);
    const Standard_Real highT = std::max(edge1.first, edge1.last);
    const Standard_Real stepS = std::abs(edge0.last - edge0.first) /
      (edge0.closed ? sampleCount : sampleCount - 1);
    const Standard_Real stepT = std::abs(edge1.last - edge1.first) /
      (edge1.closed ? sampleCount : sampleCount - 1);
    const Standard_Real maxShiftS = 2.0 * stepS;
    const Standard_Real maxShiftT = 2.0 * stepT;
    BRepAdaptor_Curve curve0(edge0.edge);
    BRepAdaptor_Curve curve1(edge1.edge);
    bool converged = false;

    try {
      for (int iteration = 0; iteration < 30; ++iteration) {
        gp_Pnt p0, p1;
        gp_Vec d0, dd0, d1, dd1;
        curve0.D2(s, p0, d0, dd0);
        curve1.D2(t, p1, d1, dd1);
        const gp_Vec chord(p0, p1);
        if (chord.Magnitude() <= Precision::Confusion()) return false;
        const Standard_Real f0 = cross2(d0, chord);
        const Standard_Real f1 = cross2(d1, chord);
        const Standard_Real j00 = cross2(dd0, chord);
        const Standard_Real j01 = cross2(d0, d1);
        const Standard_Real j11 = cross2(dd1, chord);
        const Standard_Real determinant = j00 * j11 - j01 * j01;
        const Standard_Real scale = std::max(1.0, std::max(std::abs(j00 * j11), j01 * j01));
        if (std::abs(determinant) <= 1e-14 * scale) return false;

        const Standard_Real deltaS = (j11 * f0 - j01 * f1) / determinant;
        const Standard_Real deltaT = (j00 * f1 - j01 * f0) / determinant;
        if (!std::isfinite(deltaS) || !std::isfinite(deltaT)) return false;
        s -= deltaS;
        t -= deltaT;
        if (!std::isfinite(s) || !std::isfinite(t) ||
            s < lowS - maxShiftS || s > highS + maxShiftS ||
            t < lowT - maxShiftT || t > highT + maxShiftT) {
          return false;
        }
        if (std::max(std::abs(deltaS), std::abs(deltaT)) < 1e-14) {
          converged = true;
          break;
        }
      }

      if (!converged || s < lowS || s > highS || t < lowT || t > highT ||
          std::abs(s - start.parameter) > maxShiftS ||
          std::abs(t - end.parameter) > maxShiftT) {
        return false;
      }

      gp_Pnt p0, p1;
      gp_Vec d0, d1;
      curve0.D1(s, p0, d0);
      curve1.D1(t, p1, d1);
      const gp_Vec chord(p0, p1);
      const Standard_Real residualScale = std::max(1.0, chord.Magnitude() *
        std::max(d0.Magnitude(), d1.Magnitude()));
      if (std::abs(cross2(d0, chord)) > 1e-9 * residualScale ||
          std::abs(cross2(d1, chord)) > 1e-9 * residualScale) {
        return false;
      }
    } catch (Standard_Failure const&) {
      return false;
    }

    solvedStart = s;
    solvedEnd = t;
    return true;
  };

  std::vector<bool> sampledArcs(segmentCount, false);
  std::vector<Standard_Real> parameterSums(hull.size(), 0.0);
  std::vector<int> parameterCounts(hull.size(), 0);
  std::vector<bool> parameterConflicts(hull.size(), false);
  if (hull.size() > 2) {
    for (size_t i = 0; i < segmentCount; ++i) {
      const size_t next = (i + 1) % hull.size();
      const Sample& start = samples[hull[i]];
      const Sample& end = samples[hull[next]];
      sampledArcs[i] = curveArc(start, end, start.parameter, end.parameter);
      if (sampledArcs[i] || adjacentDirection(start, end) != 0) continue;

      Standard_Real s, t;
      if (!solveBitangent(start, end, s, t)) continue;
      auto propose = [&](size_t vertex, Standard_Real parameter) {
        const Sample& sample = samples[hull[vertex]];
        const EdgeData& edge = edges[sample.edgeIndex];
        const Standard_Real tolerance = 1e-6 * std::max(1.0, std::abs(edge.last - edge.first));
        if (parameterCounts[vertex] > 0 &&
            std::abs(parameterSums[vertex] / parameterCounts[vertex] - parameter) > tolerance) {
          parameterConflicts[vertex] = true;
        }
        parameterSums[vertex] += parameter;
        ++parameterCounts[vertex];
      };
      propose(i, s);
      propose(next, t);
    }
  }

  std::vector<Standard_Real> contactParameters(hull.size());
  for (size_t i = 0; i < hull.size(); ++i) {
    contactParameters[i] = samples[hull[i]].parameter;
    if (parameterCounts[i] > 0 && !parameterConflicts[i]) {
      contactParameters[i] = parameterSums[i] / parameterCounts[i];
    }
  }

  auto pointAt = [&edges](const Sample& sample, Standard_Real parameter) {
    if (edges[sample.edgeIndex].curve.IsNull()) return sample.point;
    BRepAdaptor_Curve adaptor(edges[sample.edgeIndex].edge);
    return adaptor.Value(parameter);
  };
  BRepBuilderAPI_MakeWire result;
  for (size_t i = 0; i < segmentCount;) {
    const size_t next = (i + 1) % hull.size();
    const Sample& start = samples[hull[i]];
    const Sample& end = samples[hull[next]];
    const bool isArc = sampledArcs[i] &&
      curveArc(start, end, contactParameters[i], contactParameters[next]);

    if (isArc) {
      size_t runEnd = i + 1;
      const int direction = adjacentDirection(start, end);
      const bool crossesSeam = start.edgeIndex == end.edgeIndex &&
        edges[start.edgeIndex].closed &&
        std::abs(start.sampleIndex - end.sampleIndex) == sampleCount - 1;
      if (!crossesSeam) {
        while (runEnd + 1 < hull.size() && sampledArcs[runEnd]) {
          const Sample& a = samples[hull[runEnd]];
          const Sample& b = samples[hull[runEnd + 1]];
          if (a.edgeIndex != start.edgeIndex || b.edgeIndex != start.edgeIndex ||
              b.sampleIndex - a.sampleIndex != direction) {
            break;
          }
          ++runEnd;
        }
      }

      const size_t endVertex = runEnd % hull.size();
      const Sample& arcEnd = samples[hull[endVertex]];
      const EdgeData& edge = edges[start.edgeIndex];
      Standard_Real first = contactParameters[i];
      Standard_Real last = contactParameters[endVertex];
      if (edge.closed && std::abs(start.sampleIndex - arcEnd.sampleIndex) == sampleCount - 1) {
        if (start.sampleIndex == sampleCount - 1) {
          last = edge.last;
        } else {
          first = edge.last;
        }
      }
      TopoDS_Edge outputEdge;
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
      result.Add(outputEdge);
      i = runEnd;
    } else {
      const gp_Pnt p0 = pointAt(start, contactParameters[i]);
      const gp_Pnt p1 = pointAt(end, contactParameters[next]);
      BRepBuilderAPI_MakeEdge edgeBuilder(p0, p1);
      if (!edgeBuilder.IsDone()) {
        return nullptr;
      }
      result.Add(edgeBuilder.Edge());
      ++i;
    }
  }

  if (!result.IsDone()) {
    return nullptr;
  }
  return new TopoDS_Wire(result.Wire());
} |]
