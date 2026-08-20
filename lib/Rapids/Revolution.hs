{-# LANGUAGE QuasiQuotes #-}

module Rapids.Revolution where

import Data.Acquire (mkAcquire)
import Foreign hiding (rotate)
import Foreign.C.Types (CDouble)
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear (unit, _x)
import OpenCascade.TopoDS (Shape)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Waterfall hiding (unions, Shape, revolution)
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Solid (emptySolid, solidFromAcquire)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Rapids.ToPath
import Rapids.Color
import Rapids.ToShape

C.context occtContext
Cpp.include "<Bnd_Box.hxx>"
Cpp.include "<BRepBndLib.hxx>"
Cpp.include "<BRepAlgoAPI_Fuse.hxx>"
Cpp.include "<BRepAlgoAPI_Splitter.hxx>"
Cpp.include "<TopTools_ListOfShape.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepPrimAPI_MakeRevol.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<gp_Ax1.hxx>"
Cpp.include "<gp_Dir.hxx>"
Cpp.include "<gp_Pnt.hxx>"

class Revolution a where
  -- | rotate around the profile y axis (which becomes the solid z axis) with
  -- an optional angle for how far around the axis to go (clockwise with the
  -- camera pointed down (towards z= -infinity))
  --
  -- > revolution profile
  -- > revolution radians profile
  revolution :: a

instance (ToShape profile, Solid ~ solid) => Revolution (profile -> solid) where
  revolution = unions . map (sector (2*pi)) . shapePaths . toShape

instance {-# INCOHERENT #-} (ToShape profile, radians ~ Double, solid ~ Solid) => Revolution (radians -> profile -> solid) where
  revolution radians = unions . map (sector radians) . shapePaths . toShape

-- | Construct a sector of a 'Solid' of revolution from a 'Path2D'.
--
-- The angle is in radians. The path is revolved about the y axis and the
-- resulting solid is rotated around the x axis so that its axis of revolution is the z axis.
sector :: Double -> Path2D -> Solid
sector angle (Path2D (ComplexRawPath rawPath)) =
  rotate (unit _x) (pi / 2) . solidFromShape $
    [Cpp.block| TopoDS_Shape* {
      TopoDS_Wire* path = (TopoDS_Wire*)$(void* rawPath');
      if (path == nullptr) {
        return new TopoDS_Shape();
      }

      try {
        BRepBuilderAPI_MakeFace faceBuilder(*path);
        if (!faceBuilder.IsDone()) {
          return new TopoDS_Shape();
        }

        TopoDS_Face face = faceBuilder.Face();
        TopoDS_Shape profile = face;
        gp_Ax1 axis(gp_Pnt(0.0, 0.0, 0.0), gp_Dir(0.0, 1.0, 0.0));
        Bnd_Box bounds;
        BRepBndLib::Add(face, bounds);
        Standard_Real xMin, yMin, zMin, xMax, yMax, zMax;
        bounds.Get(xMin, yMin, zMin, xMax, yMax, zMax);
        if (xMin < -1.0e-9 && xMax > 1.0e-9) {
          // MakeRevol cannot process a face which crosses its axis. Split the
          // planar face on the y axis and revolve the resulting faces one by
          // one, fusing them back together afterwards.
          BRepAlgoAPI_Splitter splitter;
          TopTools_ListOfShape arguments;
          arguments.Append(face);
          splitter.SetArguments(arguments);

          BRepBuilderAPI_MakeFace axisFaceBuilder(
            gp_Pln(gp_Pnt(0.0, 0.0, 0.0), gp_Dir(1.0, 0.0, 0.0)));
          if (!axisFaceBuilder.IsDone()) {
            return new TopoDS_Shape();
          }
          TopTools_ListOfShape tools;
          tools.Append(axisFaceBuilder.Face());
          splitter.SetTools(tools);
          splitter.Build();
          if (splitter.HasErrors()) {
            return new TopoDS_Shape();
          }
          profile = splitter.Shape();
        }

        TopoDS_Shape result;
        for (TopExp_Explorer explorer(profile, TopAbs_FACE);
             explorer.More(); explorer.Next()) {
          BRepPrimAPI_MakeRevol revol(explorer.Current(), axis, $(double angle'), true);
          if (!revol.IsDone()) {
            return new TopoDS_Shape();
          }
          TopoDS_Shape piece = revol.Shape();
          if (result.IsNull()) {
            result = piece;
          } else {
            BRepAlgoAPI_Fuse fuse(result, piece);
            fuse.Build();
            if (fuse.HasErrors()) {
              return new TopoDS_Shape();
            }
            result = fuse.Shape();
          }
        }

        if (result.IsNull()) {
          BRepPrimAPI_MakeRevol revol(profile, axis, $(double angle'), true);
          if (!revol.IsDone()) {
            return new TopoDS_Shape();
          }
          result = revol.Shape();
        }

        return new TopoDS_Shape(result);
      } catch (Standard_Failure const&) {
        return new TopoDS_Shape();
      }
    }|]
  where
    rawPath' :: Ptr ()
    rawPath' = castPtr rawPath
    angle' :: CDouble
    angle' = realToFrac angle
    solidFromShape :: IO (Ptr Shape) -> Solid
    solidFromShape newShape =
      solidFromAcquire (mkAcquire newShape deleteShape)
sector _ _ = emptySolid
