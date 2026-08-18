{-# LANGUAGE QuasiQuotes #-}

module Rapids.Revolution
  ( revolution,
    sector,
  )
where

import Data.Acquire (mkAcquire)
import Foreign hiding (rotate)
import Foreign.C.Types (CDouble)
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Linear (unit, _x)
import OpenCascade.TopoDS (Shape)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Waterfall hiding (Shape, revolution)
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Solid (emptySolid, solidFromAcquire)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))

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

-- | Construct a full 'Solid' of revolution from a 'Path2D'.
--
-- The path is revolved about the y axis and the resulting solid is rotated
-- so that its axis of revolution is the z axis.
revolution :: Path2D -> Solid
revolution = sector (2 * pi)

-- | Construct a sector of a 'Solid' of revolution from a 'Path2D'.
--
-- The angle is in radians. The path is revolved about the y axis and the
-- resulting solid is rotated so that its axis of revolution is the z axis.
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
