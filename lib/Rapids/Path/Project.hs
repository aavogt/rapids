{-# LANGUAGE QuasiQuotes #-}

module Rapids.Path.Project where

import Control.Lens
import Control.Monad.IO.Class
import Data.Acquire (Acquire, mkAcquire)
import Foreign
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Language.Haskell.TH (unsafe)
import Linear
import qualified OpenCascade.TopoDS.Wire as TopoDS
import Waterfall
import Waterfall.Internal.Finalizers
import Waterfall.Internal.Path
import Waterfall.Internal.Path.Common
import Waterfall.TwoD.Internal.Path2D

C.context occtContext
Cpp.include "<BRepExtrema_DistShapeShape.hxx>"
Cpp.include "<gp.hxx>"
Cpp.include "<gp_Pnt.hxx>"
Cpp.include "<gp_Pln.hxx>"
Cpp.include "<gp_Vec.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<BRepBuilderAPI_MakeFace.hxx>"
Cpp.include "<BRepBuilderAPI_MakeEdge.hxx>"
Cpp.include "<BRepBuilderAPI_MakeWire.hxx>"
Cpp.include "<BRepGProp.hxx>"
Cpp.include "<BRepAlgoAPI_Section.hxx>"
Cpp.include "<BRep_Tool.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<BRepGProp_Cinert.hxx>"
Cpp.include "<GeomProjLib.hxx>"
Cpp.include "<Geom_Plane.hxx>"
Cpp.include "<Standard_Failure.hxx>"

-- | i = projectPath j
--
-- make a 3D path 2D by removing z components
projectPath :: Path -> Path2D
projectPath (Path (ComplexRawPath w)) = Path2D (ComplexRawPath (unsafeFromAcquire (projectPathRaw (castPtr w))))
projectPath (Path (SinglePointRawPath v)) = Path2D (SinglePointRawPath (v & _z .~ 0))
projectPath (Path EmptyRawPath) = Path2D EmptyRawPath

projectPathRaw :: Ptr () -> Acquire (Ptr TopoDS.Wire)
projectPathRaw p =
  mkAcquire
    ( liftIO $ do
        castPtr
          <$> [Cpp.block| void * {
            try {
              TopoDS_Wire* inputWire = (TopoDS_Wire*)$(void* p);
              if (inputWire == NULL) {
                return NULL;
              }

              Handle(Geom_Plane) plane = new Geom_Plane(gp::XOY());
              gp_Dir projectionDir(0.0, 0.0, 1.0);

              BRepBuilderAPI_MakeWire wireBuilder;
              TopExp_Explorer edgeExplorer(*inputWire, TopAbs_EDGE);
              for (; edgeExplorer.More(); edgeExplorer.Next()) {
                try {
                  TopoDS_Edge edge = TopoDS::Edge(edgeExplorer.Current());

                  Standard_Real first = 0.0;
                  Standard_Real last = 0.0;
                  Handle(Geom_Curve) curve = BRep_Tool::Curve(edge, first, last);
                  if (curve.IsNull()) {
                    continue;
                  }

                  Handle(Geom_Curve) projected =
                    GeomProjLib::ProjectOnPlane(curve, plane, projectionDir, Standard_True);
                  if (projected.IsNull()) {
                    continue;
                  }

                  BRepBuilderAPI_MakeEdge edgeBuilder(projected, first, last);
                  if (!edgeBuilder.IsDone()) {
                    continue;
                  }

                  wireBuilder.Add(edgeBuilder.Edge());
                } catch (Standard_Failure const&) {
                  continue;
                }
              }

              if (!wireBuilder.IsDone()) {
                return NULL;
              }

              return new TopoDS_Wire(wireBuilder.Wire());
            } catch (Standard_Failure const&) {
              return NULL;
            }
          } |]
    )
    (\(castPtr -> q) -> [Cpp.block| void { delete  (TopoDS_Wire*)$(void* q); } |])
