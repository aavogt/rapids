{-# LANGUAGE QuasiQuotes #-}

-- | Directional padding up to an existing solid.
module Rapids.Pad.To (padToV, paddedToV) where

import InlineOCCT (occtContext, ownSolid)
import qualified Language.C.Inline.Cpp as Cpp
import Linear (V3)
import Rapids.ToShape (ToShape, toShape)
import Waterfall (Solid)
import Rapids.Num ()

Cpp.context occtContext
Cpp.include "<BRepAlgoAPI_Cut.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepExtrema_DistShapeShape.hxx>"
Cpp.include "<BRepPrimAPI_MakePrism.hxx>"
Cpp.include "<BRepTools.hxx>"
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<Geom_Surface.hxx>"
Cpp.include "<ShapeAnalysis_FreeBounds.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"
Cpp.include "<TopoDS_Face.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<limits>"
Cpp.include "<Standard_Failure.hxx>"

-- | Extrude a profile along @direction@ until it reaches @target@.
-- The returned solid is the portion of the extrusion touching the profile;
-- inner wires on target faces are also swept back through the pad and removed.
padToV :: ToShape s => V3 Double -> s -> Solid -> Solid

-- | Add the target solid to the pad produced by 'padToV'.
paddedToV :: ToShape s => V3 Double -> s -> Solid -> Solid
paddedToV direction profile target = padToV direction profile target + target
padToV direction profile target = ownSolid [Cpp.block| TopoDS_Shape* {
  auto emptyShape = []() -> TopoDS_Shape* { return new TopoDS_Shape(); };
  TopoDS_Shape* profileShape = $shape:profileShape;
  TopoDS_Shape* targetShape = $solid:target;
  if (profileShape == nullptr || profileShape->IsNull() ||
      targetShape == nullptr || targetShape->IsNull()) {
    return emptyShape();
  }

  try {
    const gp_Dir* directionPtr = $dir:direction;
    gp_Vec extrusion(*directionPtr);
    extrusion *= 1.0e6;

    BRepPrimAPI_MakePrism prism(*profileShape, extrusion);
    if (!prism.IsDone()) {
      return emptyShape();
    }
    BRepAlgoAPI_Cut targetCut(prism.Shape(), *targetShape);
    if (!targetCut.IsDone()) {
      return emptyShape();
    }
    const TopoDS_Shape cut = targetCut.Shape();

    // Keep only the component whose distance to the source profile is zero.
    auto solidTouching = [&](const TopoDS_Shape& shape) -> TopoDS_Shape {
      TopoDS_Shape nearest;
      Standard_Real nearestDistance = std::numeric_limits<Standard_Real>::max();
      for (TopExp_Explorer solids(shape, TopAbs_SOLID);
           solids.More(); solids.Next()) {
        BRepExtrema_DistShapeShape distance(solids.Current(), *profileShape);
        distance.Perform();
        if (distance.IsDone() && distance.Value() < nearestDistance) {
          nearestDistance = distance.Value();
          nearest = solids.Current();
        }
      }
      return nearestDistance <= 1.0e-6 ? nearest : TopoDS_Shape();
    };
    TopoDS_Shape pad = solidTouching(cut);
    if (pad.IsNull()) {
      return emptyShape();
    }

    // Analyze each target face independently so its closed inner bounds are
    // retained even when the supporting surface is curved.
    TopoDS_Compound holeCutters;
    BRep_Builder cutterBuilder;
    cutterBuilder.MakeCompound(holeCutters);
    bool hasHoleCutter = false;
    gp_Vec reverseExtrusion = extrusion;
    reverseExtrusion.Reverse();

    auto sameEdges = [](const TopoDS_Wire& a, const TopoDS_Wire& b) {
      Standard_Integer countA = 0;
      Standard_Integer countB = 0;
      for (TopExp_Explorer edges(a, TopAbs_EDGE); edges.More(); edges.Next()) {
        ++countA;
      }
      for (TopExp_Explorer edges(b, TopAbs_EDGE); edges.More(); edges.Next()) {
        ++countB;
      }
      if (countA == 0 || countA != countB) {
        return false;
      }
      for (TopExp_Explorer edgesA(a, TopAbs_EDGE); edgesA.More(); edgesA.Next()) {
        bool found = false;
        for (TopExp_Explorer edgesB(b, TopAbs_EDGE); edgesB.More(); edgesB.Next()) {
          if (edgesA.Current().IsSame(edgesB.Current())) {
            found = true;
            break;
          }
        }
        if (!found) {
          return false;
        }
      }
      return true;
    };

    for (TopExp_Explorer faces(*targetShape, TopAbs_FACE);
         faces.More(); faces.Next()) {
      const TopoDS_Face face = TopoDS::Face(faces.Current());
      TopoDS_Compound oneFace;
      BRep_Builder faceBuilder;
      faceBuilder.MakeCompound(oneFace);
      faceBuilder.Add(oneFace, face);
      ShapeAnalysis_FreeBounds bounds(
          oneFace, 1.0e-7, Standard_False, Standard_False);
      const TopoDS_Wire outerWire = BRepTools::OuterWire(face);
      const Handle(Geom_Surface) surface = BRep_Tool::Surface(face);
      if (surface.IsNull()) {
        continue;
      }

      for (TopExp_Explorer closedWires(bounds.GetClosedWires(), TopAbs_WIRE);
           closedWires.More(); closedWires.Next()) {
        const TopoDS_Wire bound = TopoDS::Wire(closedWires.Current());
        TopoDS_Wire innerWire;
        bool isOuter = false;
        for (TopExp_Explorer faceWires(face, TopAbs_WIRE);
             faceWires.More(); faceWires.Next()) {
          const TopoDS_Wire candidate = TopoDS::Wire(faceWires.Current());
          if (sameEdges(bound, candidate)) {
            if (sameEdges(candidate, outerWire)) {
              isOuter = true;
            } else {
              innerWire = candidate;
            }
            break;
          }
        }
        if (isOuter || innerWire.IsNull()) {
          continue;
        }

        BRepBuilderAPI_MakeFace holeFace(surface, innerWire, Standard_True);
        if (!holeFace.IsDone()) {
          return emptyShape();
        }
        BRepPrimAPI_MakePrism holePrism(holeFace.Face(), reverseExtrusion);
        if (!holePrism.IsDone()) {
          return emptyShape();
        }
        hasHoleCutter = true;
        cutterBuilder.Add(holeCutters, holePrism.Shape());
      }
    }

    if (hasHoleCutter) {
      BRepAlgoAPI_Cut removeHoles(pad, holeCutters);
      if (!removeHoles.IsDone()) {
        return emptyShape();
      }
      pad = solidTouching(removeHoles.Shape());
      if (pad.IsNull()) {
        return emptyShape();
      }
    }
    return new TopoDS_Shape(pad);
  } catch (const Standard_Failure&) {
    return emptyShape();
  }
} |] where
  profileShape = toShape profile
