-- | Validate OCCT shapes and report detected topology/geometry problems.
module Rapids.Check
  ( checkSolid,
    checkShape,
    checkPath,
    checkBoolean,
  )
where

import Control.Monad.IO.Class (liftIO)
import Foreign
import Foreign.C
import InlineOCCT (occtContext)
import qualified Language.C.Inline.Cpp as Cpp
import Waterfall (Path, Shape, Solid)
import Waterfall.Internal.Finalizers (unsafeFromAcquire)

Cpp.context occtContext
Cpp.include "<BRepAlgoAPI_Check.hxx>"
Cpp.include "<BRepCheck_Analyzer.hxx>"
Cpp.include "<ShapeAnalysis_Edge.hxx>"
Cpp.include "<ShapeAnalysis_Shell.hxx>"
Cpp.include "<Standard_Failure.hxx>"
Cpp.include "<TopAbs_ShapeEnum.hxx>"
Cpp.include "<TopExp_Explorer.hxx>"
Cpp.include "<TopoDS.hxx>"
Cpp.include "<TopoDS_Shape.hxx>"
Cpp.include "<TopoDS_Wire.hxx>"
Cpp.include "<cstdio>"
Cpp.include "<sstream>"
Cpp.include "<string>"

[Cpp.emitBlock|
static int rapids_check_shape(const TopoDS_Shape* shape, char* output, size_t capacity) {
  if (shape == nullptr || shape->IsNull()) {
    std::snprintf(output, capacity, "null shape");
    return 1;
  }

  try {
    std::ostringstream report;
    BRepCheck_Analyzer brepCheck(*shape);
    if (!brepCheck.IsValid()) {
      report << "BRepCheck_Analyzer: invalid topology or geometry";
    }

    ShapeAnalysis_Edge edgeAnalysis;
    TopExp_Explorer edges(*shape, TopAbs_EDGE);
    for (; edges.More(); edges.Next()) {
      if (!edgeAnalysis.HasCurve3d(TopoDS::Edge(edges.Current()))) {
        if (report.tellp() > 0) report << "; ";
        report << "ShapeAnalysis_Edge: edge has no 3D curve";
        break;
      }
    }

    ShapeAnalysis_Shell shellAnalysis;
    shellAnalysis.LoadShells(*shape);
    if (!shellAnalysis.CheckOrientedShells(*shape) && shellAnalysis.HasBadEdges()) {
      if (report.tellp() > 0) report << "; ";
      report << "ShapeAnalysis_Shell: shell has incorrectly oriented edges";
    }

    BRepAlgoAPI_Check algoCheck(*shape);
    algoCheck.Perform();
    if (!algoCheck.IsValid()) {
      if (report.tellp() > 0) report << "; ";
      report << "BRepAlgoAPI_Check: small edges or self-interference detected";
    }

    const std::string message = report.str();
    if (message.empty()) return 0;
    std::snprintf(output, capacity, "%s", message.c_str());
    return 1;
  } catch (const Standard_Failure& failure) {
    std::snprintf(output, capacity, "Open CASCADE check failed: %s", failure.GetMessageString());
    return 1;
  } catch (...) {
    std::snprintf(output, capacity, "Open CASCADE check failed with an unknown exception");
    return 1;
  }
}
|]

runCheck :: (Ptr CChar -> IO CInt) -> Maybe String
runCheck check = unsafeFromAcquire $ liftIO $ allocaBytes 2048 $ \output -> do
  fillBytes output 0 2048
  failed <- check output
  if failed == 0 then pure Nothing else Just <$> peekCString output

-- | Return 'Nothing' when the solid passes BRepCheck and BRepAlgoAPI checks.
checkSolid :: Solid -> Maybe String
checkSolid solid = runCheck $ \output ->
  [Cpp.block| int {
    return rapids_check_shape($solid:solid, $(char *output), 2048);
  } |]

-- | Return 'Nothing' when the shape passes BRepCheck and BRepAlgoAPI checks.
checkShape :: Shape -> Maybe String
checkShape shape = runCheck $ \output ->
  [Cpp.block| int {
    return rapids_check_shape($shape:shape, $(char *output), 2048);
  } |]

-- | Return 'Nothing' when the path passes BRepCheck and BRepAlgoAPI checks.
checkPath :: Path -> Maybe String
checkPath path = runCheck $ \output ->
  [Cpp.block| int {
    return rapids_check_shape(static_cast<const TopoDS_Shape*>($path:path), $(char *output), 2048);
  } |]

-- | Check two solids for individual validity and compatibility in a boolean operation.
checkBoolean :: Solid -> Solid -> Maybe String
checkBoolean first second = runCheck $ \output ->
  [Cpp.block| int {
    TopoDS_Shape* shape1 = $solid:first;
    TopoDS_Shape* shape2 = $solid:second;
    if (shape1 == nullptr || shape1->IsNull() || shape2 == nullptr || shape2->IsNull()) {
      std::snprintf($(char *output), 2048, "null shape in boolean check");
      return 1;
    }
    try {
      BRepAlgoAPI_Check algoCheck(*shape1, *shape2);
      algoCheck.Perform();
      if (algoCheck.IsValid()) return 0;
      std::snprintf($(char *output), 2048,
                    "BRepAlgoAPI_Check: boolean operation inputs are incompatible or invalid");
      return 1;
    } catch (const Standard_Failure& failure) {
      std::snprintf($(char *output), 2048, "Open CASCADE boolean check failed: %s",
                    failure.GetMessageString());
      return 1;
    } catch (...) {
      std::snprintf($(char *output), 2048,
                    "Open CASCADE boolean check failed with an unknown exception");
      return 1;
    }
  } |]
