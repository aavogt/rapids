-- TODO https://github.com/fpco/inline-c/tree/master/inline-c#vectors instead?
-- | volume, center of mass and moment of inertia of a 'Solid' that may be a compound of many solids
module Rapids.Statistics (
  -- * merge compounds
  volume,
  centerOfMass,
  momentOfInertia,
  momentOfInertiaMatrix,
  principalMomentsOfInertia,
  -- * split compounds
  volumes,
  centersOfMass,
  momentsOfInertia,
  componentCount,
) where

import Control.Monad.IO.Class (liftIO)
import Foreign.C.Types (CDouble)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import InlineOCCT
import qualified Language.C.Inline.Cpp as Cpp
import qualified Language.C.Inline as C
import Waterfall (Solid)
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Linear (M33, V3 (..))

C.context occtContext
Cpp.include "<BRep_Builder.hxx>"
Cpp.include "<BRepBuilderAPI_MakeSolid.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<BRepOffset.hxx>"
Cpp.include "<BRepOffsetAPI_MakeOffsetShape.hxx>"
Cpp.include "<GProp_GProps.hxx>"
Cpp.include "<gp_Ax1.hxx>"
Cpp.include "<gp_Dir.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<GeomAbs_JoinType.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Compound.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"

Cpp.include "<gp_Mat.hxx>"
Cpp.include "<math_Jacobi.hxx>"
Cpp.include "<math_Matrix.hxx>"
Cpp.include "<math_Vector.hxx>"
-- | Compute volume by summing the volumes of all solid components.
volume :: Solid -> Double
volume solid = unsafeFromAcquire $ do
  realToFrac <$> liftIO [Cpp.block| double {
        TopoDS_Shape* input = $solid:solid;
        if (input == nullptr || input->IsNull()) {
          return 0.0;
        }

        Standard_Real total = 0.0;
        for (TopExp_Explorer solids(*input, TopAbs_SOLID);
             solids.More(); solids.Next()) {
          GProp_GProps props;
          BRepGProp::VolumeProperties(
            solids.Current(), props, Standard_False, Standard_False, Standard_False);
          total += props.Mass();
        }
        return total;
      }|]

-- | Compute the volume-weighted center of mass of all solid components.
centerOfMass :: Solid -> V3 Double
centerOfMass solid = unsafeFromAcquire $ liftIO $ do
  allocaArray 3 $ \output -> do
    [Cpp.block| void {
      double* output = $(double *output);
      TopoDS_Shape* input = $solid:solid;
      if (input == nullptr || input->IsNull()) {
        output[0] = 0.0;
        output[1] = 0.0;
        output[2] = 0.0;
        return;
      }

      GProp_GProps props;
      int count = 0;
      for (TopExp_Explorer solids(*input, TopAbs_SOLID);
           solids.More(); solids.Next()) {
        GProp_GProps component;
        BRepGProp::VolumeProperties(
          solids.Current(), component, Standard_False, Standard_False, Standard_False);
        props.Add(component);
        ++count;
      }
      if (count == 0) {
        output[0] = 0.0;
        output[1] = 0.0;
        output[2] = 0.0;
        return;
      }

      gp_Pnt center = props.CentreOfMass();
      output[0] = center.X();
      output[1] = center.Y();
      output[2] = center.Z();
    }|]
    values <- peekArray 3 output
    pure (asV3 (map realToFrac values))

-- | Compute the volume of each solid component.
volumes :: Solid -> [Double]
volumes solid = unsafeFromAcquire $ liftIO $ do
  count <- componentCount solid
  allocaArray count $ \output -> do
    writeVolumes solid output
    map realToFrac <$> peekArray count output

-- | Compute the center of mass of each solid component.
centersOfMass :: Solid -> [V3 Double]
centersOfMass solid = unsafeFromAcquire $ liftIO $ do
  count <- componentCount solid
  allocaArray (3 * count) $ \output -> do
    writeCentersOfMass solid output
    triples . map realToFrac <$> peekArray (3 * count) output


-- | Inertia tensor about the world-coordinate origin, in world XYZ axes.
-- Translating the solid therefore changes this tensor by the parallel axis theorem.
momentOfInertiaMatrix :: Solid -> M33 Double
momentOfInertiaMatrix solid = unsafeFromAcquire $ liftIO $ allocaArray 9 $ \output -> do
  writeMomentOfInertiaMatrix solid output
  asM33 . map realToFrac <$> peekArray 9 output

-- | Principal moments and their corresponding unit principal axes about the origin.
-- The matrix columns are the eigenvectors, so @transpose axes !*! inertia !*! axes@
-- is diagonal with the returned moments on its diagonal.
principalMomentsOfInertia :: Solid -> (V3 Double, M33 Double)
principalMomentsOfInertia solid = unsafeFromAcquire $ liftIO $ allocaArray 9 $ \tensorOutput -> do
  writeMomentOfInertiaMatrix solid tensorOutput
  allocaArray 12 $ \output -> do
    [Cpp.block| void {
      double* tensor = $(double *tensorOutput);
      math_Matrix matrix(1, 3, 1, 3);
      for (int row = 1; row <= 3; ++row) {
        for (int col = 1; col <= 3; ++col) {
          matrix(row, col) = tensor[(row - 1) * 3 + col - 1];
        }
      }
      math_Jacobi eigen(matrix);
      const math_Vector& moments = eigen.Values();
      const math_Matrix& axes = eigen.Vectors();
      double* output = $(double *output);
      for (int i = 1; i <= 3; ++i) output[i - 1] = moments(i);
      for (int row = 1; row <= 3; ++row) {
        for (int col = 1; col <= 3; ++col) {
          output[3 + (row - 1) * 3 + col - 1] = axes.Value(row, col);
        }
      }
    }|]
    values <- map realToFrac <$> peekArray 12 output
    pure $ asPrincipalDecomposition values

-- | @momentOfInertia center axis solid@
--
-- Computes the moment of inertia of all solid components around an axis.
momentOfInertia :: V3 Double -> V3 Double -> Solid -> Double
momentOfInertia center axis solid = unsafeFromAcquire $ do
  let V3 cx cy cz = center
      V3 ax ay az = axis
      cx' = realToFrac cx :: CDouble
      cy' = realToFrac cy :: CDouble
      cz' = realToFrac cz :: CDouble
      ax' = realToFrac ax :: CDouble
      ay' = realToFrac ay :: CDouble
      az' = realToFrac az :: CDouble
  realToFrac <$> liftIO [Cpp.block| double {
    TopoDS_Shape* input = $solid:solid;
    if (input == nullptr || input->IsNull()) {
      return 0.0;
    }

    gp_Pnt point($(double cx'), $(double cy'), $(double cz'));
    gp_Dir direction($(double ax'), $(double ay'), $(double az'));
    gp_Ax1 inertiaAxis(point, direction);
    GProp_GProps props;
    for (TopExp_Explorer solids(*input, TopAbs_SOLID);
         solids.More(); solids.Next()) {
      GProp_GProps component;
      BRepGProp::VolumeProperties(
        solids.Current(), component, Standard_False, Standard_False, Standard_False);
      props.Add(component);
    }
    return props.MomentOfInertia(inertiaAxis);
  }|]

-- | @momentsOfInertia center axis solid@
--
-- compute the moment of inertia of each solid component around an axis.
momentsOfInertia :: V3 Double -> V3 Double -> Solid -> [Double]
momentsOfInertia center axis solid = unsafeFromAcquire $ liftIO $ do
  count <- componentCount solid
  let V3 cx cy cz = center
      V3 ax ay az = axis
      cx' = realToFrac cx :: CDouble
      cy' = realToFrac cy :: CDouble
      cz' = realToFrac cz :: CDouble
      ax' = realToFrac ax :: CDouble
      ay' = realToFrac ay :: CDouble
      az' = realToFrac az :: CDouble
  allocaArray count $ \output -> do
    writeMomentsOfInertia solid output cx' cy' cz' ax' ay' az'
    map realToFrac <$> peekArray count output

componentCount :: Solid -> IO Int
componentCount solid = fromIntegral <$> [Cpp.block| int {
  TopoDS_Shape* input = $solid:solid;
  if (input == nullptr || input->IsNull()) {
    return 0;
  }

  int count = 0;
  for (TopExp_Explorer solids(*input, TopAbs_SOLID);
       solids.More(); solids.Next()) {
    ++count;
  }
  return count;
}|]

writeVolumes :: Solid -> Ptr CDouble -> IO ()
writeVolumes solid output = [Cpp.block| void {
  double* output = $(double *output);
  TopoDS_Shape* input = $solid:solid;
  if (input == nullptr || input->IsNull()) {
    return;
  }

  int index = 0;
  for (TopExp_Explorer solids(*input, TopAbs_SOLID);
       solids.More(); solids.Next()) {
    GProp_GProps props;
    BRepGProp::VolumeProperties(
      solids.Current(), props, Standard_False, Standard_False, Standard_False);
    output[index++] = props.Mass();
  }
}|]

writeCentersOfMass :: Solid -> Ptr CDouble -> IO ()
writeCentersOfMass solid output = [Cpp.block| void {
  double* output = $(double *output);
  TopoDS_Shape* input = $solid:solid;
  if (input == nullptr || input->IsNull()) {
    return;
  }

  int index = 0;
  for (TopExp_Explorer solids(*input, TopAbs_SOLID);
       solids.More(); solids.Next()) {
    GProp_GProps props;
    BRepGProp::VolumeProperties(
      solids.Current(), props, Standard_False, Standard_False, Standard_False);
    gp_Pnt center = props.CentreOfMass();
    output[index++] = center.X();
    output[index++] = center.Y();
    output[index++] = center.Z();
  }
}|]

writeMomentsOfInertia :: Solid -> Ptr CDouble -> CDouble -> CDouble -> CDouble
  -> CDouble -> CDouble -> CDouble -> IO ()
writeMomentsOfInertia solid output cx cy cz ax ay az = [Cpp.block| void {
  double* output = $(double *output);
  TopoDS_Shape* input = $solid:solid;
  if (input == nullptr || input->IsNull()) {
    return;
  }

  gp_Pnt point($(double cx), $(double cy), $(double cz));
  gp_Dir direction($(double ax), $(double ay), $(double az));
  gp_Ax1 inertiaAxis(point, direction);
  int index = 0;
  for (TopExp_Explorer solids(*input, TopAbs_SOLID);
       solids.More(); solids.Next()) {
    GProp_GProps props;
    BRepGProp::VolumeProperties(
      solids.Current(), props, Standard_False, Standard_False, Standard_False);
    output[index++] = props.MomentOfInertia(inertiaAxis);
  }
}|]

writeMomentOfInertiaMatrix :: Solid -> Ptr CDouble -> IO ()
writeMomentOfInertiaMatrix solid output = [Cpp.block| void {
  TopoDS_Shape* input = $solid:solid;
  GProp_GProps props;
  bool hasSolids = false;
  if (input != nullptr && !input->IsNull()) {
    for (TopExp_Explorer solids(*input, TopAbs_SOLID);
         solids.More(); solids.Next()) {
      GProp_GProps component;
      BRepGProp::VolumeProperties(
        solids.Current(), component, Standard_False, Standard_False, Standard_False);
      props.Add(component);
      hasSolids = true;
    }
  }

  double* output = $(double *output);
  if (!hasSolids) {
    for (int i = 0; i < 9; ++i) output[i] = 0.0;
    return;
  }

  gp_Mat matrix = props.MatrixOfInertia();
  gp_Pnt center = props.CentreOfMass();
  const double x = center.X(), y = center.Y(), z = center.Z();
  const double mass = props.Mass();
  const double coordinates[3] = {x, y, z};
  const double distanceSquared = x * x + y * y + z * z;
  for (int row = 1; row <= 3; ++row) {
    for (int col = 1; col <= 3; ++col) {
      const double parallelAxis = mass * (
        (row == col ? distanceSquared : 0.0) - coordinates[row - 1] * coordinates[col - 1]);
      output[(row - 1) * 3 + col - 1] = matrix.Value(row, col) + parallelAxis;
    }
  }
}|]

asV3 :: [Double] -> V3 Double
asV3 [x, y, z] = V3 x y z
asV3 _ = error "OCCT returned an invalid center of mass"

triples :: [Double] -> [V3 Double]
triples [] = []
triples (x : y : z : rest) = V3 x y z : triples rest
triples _ = error "OCCT returned an invalid list of centers of mass"
asM33 :: [Double] -> M33 Double
asM33 [xx, xy, xz, yx, yy, yz, zx, zy, zz] =
  V3 (V3 xx xy xz) (V3 yx yy yz) (V3 zx zy zz)
asM33 _ = error "OCCT returned an invalid inertia matrix"

asPrincipalDecomposition :: [Double] -> (V3 Double, M33 Double)
asPrincipalDecomposition [ix, iy, iz, ax, bx, cx, ay, by, cy, az, bz, cz] =
  (V3 ix iy iz, V3 (V3 ax bx cx) (V3 ay by cy) (V3 az bz cz))
asPrincipalDecomposition _ = error "OCCT returned an invalid inertia eigendecomposition"
