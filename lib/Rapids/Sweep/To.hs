{-# LANGUAGE QuasiQuotes #-}

-- | Trim a sweep at its first contact with a target solid.
module Rapids.Sweep.To (sweepTo, sweptTo) where

import Data.Acquire (withAcquire)
import InlineOCCT (occtContext, ownSolid)
import qualified Language.C.Inline.Cpp as Cpp
import Linear (V3 (..))
import qualified Rapids.Pad as Pad
import Rapids.ToPath (ToPath, toPath)
import Rapids.ToShape (ToShape, toShape)
import Rapids.Color (propagateShapeColors)
import System.IO.Unsafe (unsafePerformIO)
import Waterfall (Solid)
import Rapids.Num ()
import Waterfall.Internal.Edges (wireEndpoints, wireTangentStart)
import Waterfall.Internal.Finalizers (toAcquire)
import Waterfall.Internal.Path (Path (Path))
import Waterfall.Internal.Path.Common (RawPath (ComplexRawPath))

Cpp.context occtContext
Cpp.include "<BRepAlgoAPI_Cut.hxx>"
Cpp.include "<BRepBuilderAPI_Transform.hxx>"
Cpp.include "<gp_Ax1.hxx>"
Cpp.include "<gp_Dir.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<gp_Trsf.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<algorithm>"
Cpp.include "<cmath>"
Cpp.include "<BRepExtrema_DistShapeShape.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<limits>"

-- | Sweep @profile@ along @path@, subtract @target@, and keep the portion
-- touching the sweep's starting cross-section. The supplied path must reach
-- or pass through the target.
sweepTo :: (ToPath path, ToShape profile) => path -> profile -> Solid -> Solid
sweepTo rawPath profile target =
  case sweepStartFrame path of
    Nothing -> mempty
    Just (start, tangent) ->
      let swept = Pad.sweep path profile
          result = ownSolid [Cpp.block| TopoDS_Shape* {
            auto emptyShape = []() -> TopoDS_Shape* { return new TopoDS_Shape(); };
            TopoDS_Shape* sweptShape = $solid:swept;
            TopoDS_Shape* targetShape = $solid:target;
            TopoDS_Shape* profileShape = $shape:profileShape;
            const gp_Pnt* startPoint = $pnt:start;
            const gp_Dir* tangentDirection = $dir:tangent;
            if (sweptShape == nullptr || sweptShape->IsNull() ||
                targetShape == nullptr || targetShape->IsNull() ||
                profileShape == nullptr || profileShape->IsNull()) {
              return emptyShape();
            }
            try {
              const gp_Dir zAxis(0.0, 0.0, 1.0);
              const Standard_Real cosine = std::max(
                  -1.0, std::min(1.0, zAxis.Dot(*tangentDirection)));
              TopoDS_Shape startSection = *profileShape;
              if (cosine < 1.0 - 1.0e-14) {
                const Standard_Real angle = std::acos(cosine);
                const gp_Dir axis = cosine < -1.0 + 1.0e-14
                    ? gp_Dir(1.0, 0.0, 0.0)
                    : zAxis.Crossed(*tangentDirection);
                gp_Trsf rotation;
                rotation.SetRotation(
                    gp_Ax1(gp_Pnt(0.0, 0.0, 0.0), axis), angle);
                BRepBuilderAPI_Transform rotated(
                    startSection, rotation, Standard_True);
                if (!rotated.IsDone()) {
                  return emptyShape();
                }
                startSection = rotated.Shape();
              }
              gp_Trsf translation;
              translation.SetTranslation(gp_Vec(startPoint->XYZ()));
              BRepBuilderAPI_Transform placed(
                  startSection, translation, Standard_True);
              if (!placed.IsDone()) {
                return emptyShape();
              }
              startSection = placed.Shape();

              BRepAlgoAPI_Cut targetCut(*sweptShape, *targetShape);
              if (!targetCut.IsDone()) {
                return emptyShape();
              }

              TopoDS_Shape nearest;
              Standard_Real nearestDistance = std::numeric_limits<Standard_Real>::max();
              for (TopExp_Explorer solids(targetCut.Shape(), TopAbs_SOLID);
                   solids.More(); solids.Next()) {
                BRepExtrema_DistShapeShape distance(solids.Current(), startSection);
                distance.Perform();
                if (distance.IsDone() && distance.Value() < nearestDistance) {
                  nearestDistance = distance.Value();
                  nearest = solids.Current();
                }
              }
              if (nearest.IsNull() || nearestDistance > 1.0e-6) {
                return emptyShape();
              }
              return new TopoDS_Shape(nearest);
            } catch (const Standard_Failure&) {
              return emptyShape();
            }
          } |]
       in propagateShapeColors profileShape result
  where
    path = toPath rawPath
    profileShape = toShape profile

-- Use the same tangent and start-point placement as Waterfall.sweep.
sweepStartFrame :: Path -> Maybe (V3 Double, V3 Double)
sweepStartFrame (Path (ComplexRawPath wire)) = Just $ unsafePerformIO $
  withAcquire (toAcquire wire) $ \wirePtr -> do
    tangent <- wireTangentStart wirePtr
    (start, _) <- wireEndpoints wirePtr
    pure (start, tangent)
sweepStartFrame _ = Nothing
{-# NOINLINE sweepStartFrame #-}

-- | Add the target solid to the sweep produced by 'sweepTo'.
sweptTo :: (ToPath path, ToShape profile) => path -> profile -> Solid -> Solid
sweptTo path profile target = sweepTo path profile target + target
