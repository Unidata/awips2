/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.drawing;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.jts.JTS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.TopologyException;

/**
 * Drawing layer that can draw lines and handle undo/redo/clear/erase
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 23, 2012           mschenke    Initial creation
 * May 23, 2012  2646     bsteffen    Fix NPE in project.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawingToolLayer implements IRenderable {

    public static enum DrawMode {
        NONE, DRAW, ERASE;
    }

    public static class StackFrame {
        /** The collection of geometries displayed at a given frame */
        public Collection<Geometry> geometries;

        public StackFrame(Collection<Geometry> geometries) {
            this.geometries = geometries;
        }
    }

    /** The factory used for geometry construction */
    private static final GeometryFactory factory = new GeometryFactory();

    /**
     * Stack for undo operations. Currently size is unlimited, may want to limit
     * to specific size at some point
     */
    protected Stack<StackFrame> undoStack;

    /** Stack for redo operations */
    protected Stack<StackFrame> redoStack;

    /** Wireframe shape for display of currentData frame */
    private IWireframeShape wireframeShape;

    /** Currently displayed frame */
    protected StackFrame currentData;

    /** Color of the data */
    private RGB color = new RGB(155, 155, 155);

    /** Line width of the data */
    private int lineWidth = 2;

    /** Eraser width to use when erasing */
    private int eraserWidth = 4;

    /** Line style of the data drawn */
    private LineStyle lineStyle = LineStyle.DEFAULT;

    /**
     * Draw mode of the data (NONE,DRAW,ERASE). Used when
     * {@link #addCoordinate(Coordinate)} is called
     */
    private DrawMode drawMode = DrawMode.NONE;

    /**
     * The line currently being drawn through {@link #addCoordinate(Coordinate)}
     * calls before {@link #doneDrawing()} is called
     */
    private Geometry currentDrawingLine;

    /**
     * The line currently being drawn through {@link #addCoordinate(Coordinate)}
     * calls before {@link #doneErasing()} is called
     */
    private Geometry currentErasingLine;

    /**
     * The collection of geometries the {@link #currentErasingLine} is operating
     * on
     */
    private Collection<Geometry> currentErasingGeometries;

    /**
     * Flag that erasing is finished and a new stack frame should be created
     * next time {@link #processErase(IExtent, Rectangle)} is called
     */
    private boolean addErasingEventToStack = false;

    /** The {@link GeneralGridGeometry} we are drawing to */
    private GeneralGridGeometry targetGeometry;

    /**
     * Cached "world" to grid {@link MathTransform} ({@link StackFrame}
     * geometries are stored in grid space)
     */
    private MathTransform worldToGrid;

    /**
     * Construct a DrawingToolLayer that will draw to the
     * {@link GeneralGridGeometry} passed in
     * 
     * @param targetGeometry
     */
    public DrawingToolLayer(GeneralGridGeometry targetGeometry) {
        setTargetGeometry(targetGeometry);
        undoStack = new Stack<StackFrame>();
        redoStack = new Stack<StackFrame>();
        currentData = new StackFrame(new ArrayList<Geometry>(0));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        synchronized (currentData) {
            // Process erase coordinates before drawing data for current frame
            processErase(paintProps.getView().getExtent(),
                    paintProps.getCanvasBounds());

            if (wireframeShape == null) {
                Collection<Geometry> geoms = currentErasingGeometries != null ? currentErasingGeometries
                        : currentData.geometries;
                if (geoms != null && geoms.size() > 0) {
                    // No wireframe shape and we have data, create for drawing
                    wireframeShape = target.createWireframeShape(false,
                            targetGeometry);
                    int totalPoints = 0;
                    for (Geometry geom : geoms) {
                        totalPoints += geom.getNumPoints();
                    }
                    wireframeShape.allocate(totalPoints * 3 * 8);
                    for (Geometry geom : geoms) {
                        handle(wireframeShape, geom);
                    }
                    wireframeShape.compile();
                }
            }
            if (wireframeShape != null) {
                // We have data to draw, draw it
                target.drawWireframeShape(wireframeShape, color, lineWidth,
                        lineStyle);
            }

            // Render any line currently being drawn through addCoordinate(...)
            if (currentDrawingLine != null
                    && currentDrawingLine.getNumPoints() > 1) {
                IWireframeShape tmpShape = target.createWireframeShape(true,
                        targetGeometry);
                tmpShape.allocate(currentDrawingLine.getNumPoints() * 3 * 8);
                handle(tmpShape, currentDrawingLine);
                target.drawWireframeShape(tmpShape, color, lineWidth, lineStyle);
                tmpShape.dispose();
            }
        }
    }

    /**
     * Processes the erase line currently constructed from
     * {@link #addCoordinate(Coordinate)} while in "ERASE" {@link DrawMode}
     * 
     * @param extent
     * @param canvasSize
     */
    public void processErase(IExtent extent, Rectangle canvasSize) {
        synchronized (currentData) {
            if (currentErasingLine != null
                    && currentErasingLine.getNumPoints() > 0) {
                if (currentErasingGeometries == null) {
                    currentErasingGeometries = new ArrayList<Geometry>(
                            currentData.geometries);
                }

                // Calculate world grid to canvas grid ratio
                double ratio = extent.getWidth() / canvasSize.width;
                // Get the size to buffer the eraser line for differencing
                double bufferSize = (ratio * eraserWidth) / 2;

                // Flatten all eraser line geometries into a single list
                List<Geometry> eraserLines = new ArrayList<Geometry>(
                        currentErasingLine.getNumGeometries());
                flattenGeometry(currentErasingLine, eraserLines);

                boolean change = false;
                List<Geometry> newGeoms = new ArrayList<Geometry>(
                        currentErasingGeometries.size());

                // For each eraser line, run against currentData
                for (Geometry eraserLine : eraserLines) {
                    eraserLine = eraserLine.buffer(bufferSize);
                    newGeoms = new ArrayList<Geometry>(
                            currentErasingGeometries.size());
                    for (Geometry geom : currentErasingGeometries) {
                        if (geom.intersects(eraserLine)) {
                            // Eraser line intersects, create difference
                            Geometry diff = geom.difference(eraserLine);
                            // Mark change flag
                            change = true;
                            if (diff instanceof GeometryCollection == false) {
                                // To avoid self intersecting lines, this
                                // will split the difference geometry
                                Coordinate[] coords = diff.getCoordinates();
                                diff = diff.union(factory
                                        .createPoint(coords[0]));
                            }
                            // Add diff to newGeoms
                            flattenGeometry(diff, newGeoms);
                        } else {
                            // Add old geometry, no changes
                            newGeoms.add(geom);
                        }
                    }
                    // These are the new "currentGeoms" for the next eraser line
                    currentErasingGeometries = newGeoms;
                }

                if (change && wireframeShape != null) {
                    // In else if since addCurrentDataToStack will destroy
                    // wireframeShape for us
                    wireframeShape.dispose();
                    wireframeShape = null;
                }
                currentErasingLine = null;
            }

            if (addErasingEventToStack) {
                // If data changed and we should add a new frame, do it
                addErasingEventToStack = false;
                if (currentErasingGeometries != null) {
                    addCurrentDataToStack(undoStack);
                    redoStack.clear();
                    currentData.geometries = currentErasingGeometries;
                    currentErasingGeometries = null;
                }
            }
        }
    }

    /**
     * Recursively adds LineString objects in the geom to wireframeShape
     * 
     * @param wireframeShape
     * @param geom
     */
    private void handle(IWireframeShape wireframeShape, Geometry geom) {
        if (geom instanceof GeometryCollection) {
            for (int n = 0; n < geom.getNumGeometries(); ++n) {
                handle(wireframeShape, geom.getGeometryN(n));
            }
        } else if (geom instanceof LineString) {
            Coordinate[] coords = geom.getCoordinates();
            double[][] points = new double[coords.length][];
            for (int i = 0; i < coords.length; ++i) {
                points[i] = new double[] { coords[i].x, coords[i].y,
                        coords[i].z };
            }
            wireframeShape.addLineSegment(points);
        }
    }

    /**
     * Disposes the data in the layer
     */
    public void dispose() {
        synchronized (currentData) {
            if (wireframeShape != null) {
                wireframeShape.dispose();
            }
            currentData.geometries.clear();
            currentDrawingLine = null;
            undoStack.clear();
            redoStack.clear();
        }
    }

    /**
     * Adds a coordinate to the layer, coordinate is processed based on
     * {@link #drawMode}. Coordinate should be in {@link #targetGeometry}
     * "world" spacing
     * 
     * @param coord
     */
    public void addCoordinate(Coordinate coord) {
        synchronized (currentData) {
            // Convert coord to targetGeometry grid space
            double[] point = new double[] { coord.x, coord.y, coord.z };
            if (worldToGrid != null) {
                double[] out = new double[point.length];
                try {
                    worldToGrid.transform(point, 0, out, 0, 1);
                    point = out;
                } catch (TransformException e) {
                    UFStatus.getHandler().handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            Coordinate newCoord = new Coordinate(point[0], point[1], point[2]);
            Geometry toAddto = null;
            if (drawMode == DrawMode.DRAW) {
                if (currentDrawingLine == null) {
                    currentDrawingLine = factory.createPoint(newCoord);
                } else {
                    toAddto = currentDrawingLine;
                }
            } else if (drawMode == DrawMode.ERASE) {
                if (currentErasingLine == null) {
                    currentErasingLine = factory.createPoint(newCoord);
                } else {
                    toAddto = currentErasingLine;
                }
            }
            if (toAddto != null) {
                // This will flatten the new line into a geometry collection so
                // it is not self intersecting and errors will not occur
                int numGeoms = toAddto.getNumGeometries();
                // The last geometry in the collection is the one to append the
                // coordinate to
                Geometry last = toAddto.getGeometryN(numGeoms - 1);
                Coordinate[] coords = last.getCoordinates();
                Coordinate[] newCoords = Arrays.copyOf(coords,
                        coords.length + 1);
                newCoords[newCoords.length - 1] = newCoord;
                Geometry newGeom = null;
                try {
                    // Create new LineString with newCoords
                    newGeom = factory.createLineString(newCoords).union(
                            factory.createPoint(coords[0]));
                } catch (TopologyException e) {
                    // Can't keep adding to this line, create new one from last
                    // coordinate and newGeom will be collection with both
                    newGeom = factory.createGeometryCollection(new Geometry[] {
                            last,
                            factory.createLineString(new Coordinate[] {
                                    coords[coords.length - 1], newCoord }) });
                }

                List<Geometry> newGeoms = new ArrayList<Geometry>(numGeoms);
                for (int n = 0; n < numGeoms - 1; ++n) {
                    // Don't grab the last one (newGeoms - 1) since it will be
                    // included in newGeom
                    newGeoms.add(toAddto.getGeometryN(n));
                }
                if (newGeoms.size() > 0) {
                    // geoms still in toAddto, flatten our newGeom object into
                    // newGeoms list and create collection
                    flattenGeometry(newGeom, newGeoms);
                    newGeom = factory.createGeometryCollection(newGeoms
                            .toArray(new Geometry[newGeoms.size()]));
                }

                // Set newGeom to proper line
                if (toAddto == currentDrawingLine) {
                    currentDrawingLine = newGeom;
                } else {
                    currentErasingLine = newGeom;
                }
            }
        }
    }

    /**
     * Should be called when no more coordinates will be added to
     * {@link #addCoordinate(Coordinate)} and the line should be processed as
     * is. A new stack frame will be created with the new line in it
     */
    public void doneDrawing() {
        synchronized (currentData) {
            if (currentDrawingLine != null
                    && currentDrawingLine.getNumPoints() > 1) {
                // Have data to process
                try {
                    addCurrentDataToStack(undoStack);
                    List<Geometry> newGeometries = new ArrayList<Geometry>(
                            currentData.geometries);
                    flattenGeometry(currentDrawingLine, newGeometries);
                    currentData.geometries = newGeometries;
                    redoStack.clear();
                } catch (Exception e) {
                    UFStatus.getHandler().handle(Priority.PROBLEM,
                            "Could not add line, bad geometry", e);
                }
            }
            currentDrawingLine = null;
        }
    }

    /**
     * Recursively adds all non GeometryCollection geometries to geoms
     * 
     * @param geom
     * @param geoms
     */
    public void flattenGeometry(Geometry geom, List<Geometry> geoms) {
        if (geom instanceof GeometryCollection) {
            for (int n = 0; n < geom.getNumGeometries(); ++n) {
                flattenGeometry(geom.getGeometryN(n), geoms);
            }
        } else {
            geoms.add(geom);
        }
    }

    /**
     * Should be called when no more coordinates will be added to
     * {@link #addCoordinate(Coordinate)} and the line should be processed as
     * is. A new stack frame will be created with the new line in it
     */
    public void doneErasing() {
        synchronized (currentData) {
            addErasingEventToStack = true;
        }
    }

    /**
     * Returns true if an undo operation is capable of being processed
     * 
     * @return
     */
    public boolean canUndo() {
        return undoStack.size() > 0;
    }

    /**
     * Returns true if a redo operation is capable of being processed
     * 
     * @return
     */
    public boolean canRedo() {
        return redoStack.size() > 0;
    }

    /**
     * Returns true if a clear operation is capable of being processed
     * 
     * @return
     */
    public boolean canClear() {
        return currentData.geometries.size() > 0 || redoStack.size() > 0;
    }

    /**
     * Undo the last drawing action
     */
    public void undo() {
        pushPop(undoStack, redoStack);
    }

    /**
     * Redo the last undone drawing action
     */
    public void redo() {
        pushPop(redoStack, undoStack);
    }

    /**
     * Clears the current display, a new stack frame is created. This operation
     * is "undoable" by calling {@link #undo()}
     */
    public void clear() {
        synchronized (currentData) {
            if (currentData.geometries.size() > 0) {
                addCurrentDataToStack(undoStack);
                currentData.geometries.clear();
            }
            redoStack.clear();
        }
    }

    /**
     * Pushes currentData on pushStack and pops next frame from popStack and
     * puts in currentData
     * 
     * @param user
     * @param popFrom
     * @param pushTo
     */
    private void pushPop(Stack<StackFrame> popFrom, Stack<StackFrame> pushTo) {
        synchronized (currentData) {
            if (popFrom.size() > 0) {
                // There is something to undo, add current data to redoStack
                addCurrentDataToStack(pushTo);
                StackFrame prevFrame = popFrom.pop();
                currentData.geometries = new ArrayList<Geometry>(
                        prevFrame.geometries);
            }
        }
    }

    /**
     * Method to add the current data for the user to the user's stack. This
     * method is not thread safe and needs to be wrapped in a synchronize block
     * on currentData. Returns the current data for the user which will no
     * longer be in currentData
     * 
     * @param user
     * @return
     */
    private void addCurrentDataToStack(Stack<StackFrame> stack) {
        StackFrame oldData = new StackFrame(new ArrayList<Geometry>(
                currentData.geometries));
        stack.push(oldData);
        if (wireframeShape != null) {
            wireframeShape.dispose();
            wireframeShape = null;
        }
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        this.color = color;
    }

    /**
     * @param lineWidth
     *            the lineWidth to set
     */
    public void setLineWidth(int lineWidth) {
        this.lineWidth = lineWidth;
    }

    /**
     * @param lineStyle
     *            the lineStyle to set
     */
    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    /**
     * @param eraserWidth
     *            the eraserWidth to set
     */
    public void setEraserWidth(int eraserWidth) {
        this.eraserWidth = eraserWidth;
    }

    /**
     * @return the drawMode
     */
    public DrawMode getDrawMode() {
        return drawMode;
    }

    /**
     * @param drawMode
     *            the drawMode to set
     */
    public void setDrawMode(DrawMode drawMode) {
        if (this.drawMode != drawMode) {
            this.drawMode = drawMode;
            currentDrawingLine = null;
        }
    }

    private void setTargetGeometry(GeneralGridGeometry targetGeometry) {
        try {
            this.targetGeometry = targetGeometry;
            this.worldToGrid = TransformFactory.worldToGrid(targetGeometry,
                    PixelInCell.CELL_CENTER);
        } catch (FactoryException e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    /**
     * Reprojects the layer data for the new targetGeometry
     * 
     * @param targetGeometry
     */
    public void reproject(GeneralGridGeometry targetGeometry) {
        synchronized (currentData) {
            try {
                MathTransform oldGridToNewGrid = TransformFactory
                        .gridCellToGridCell(this.targetGeometry,
                                PixelInCell.CELL_CENTER, targetGeometry,
                                PixelInCell.CELL_CENTER);
                if (oldGridToNewGrid != null) {
                    Map<Geometry, Geometry> projectionMap = new HashMap<Geometry, Geometry>();
                    for (StackFrame sf : undoStack) {
                        sf.geometries = reprojectCollection(sf.geometries,
                                projectionMap, oldGridToNewGrid);
                    }
                    for (StackFrame sf : redoStack) {
                        sf.geometries = reprojectCollection(sf.geometries,
                                projectionMap, oldGridToNewGrid);
                    }
                    currentData.geometries = reprojectCollection(
                            currentData.geometries, projectionMap,
                            oldGridToNewGrid);
                    if (currentDrawingLine != null) {
                        currentDrawingLine = JTS.transform(currentDrawingLine,
                                oldGridToNewGrid);
                    }
                }
            } catch (Exception e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
            setTargetGeometry(targetGeometry);
            if (wireframeShape != null) {
                wireframeShape.dispose();
                wireframeShape = null;
            }
        }
    }

    private Collection<Geometry> reprojectCollection(
            Collection<Geometry> geometries, Map<Geometry, Geometry> cache,
            MathTransform transform) {
        List<Geometry> newGeoms = new ArrayList<Geometry>(geometries.size());
        for (Geometry geom : geometries) {
            Geometry projected = cache.get(geom);
            if (projected == null) {
                try {
                    projected = JTS.transform(geom, transform);
                } catch (Exception e) {
                    UFStatus.getHandler().handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                cache.put(geom, projected);
            }

            if (projected != null) {
                newGeoms.add(projected);
            }
        }
        return newGeoms;
    }

    public void rebuildLayer(Collection<Geometry> currentData,
            Stack<Collection<Geometry>> undoStack,
            Stack<Collection<Geometry>> redoStack) {
        // pre-build StackFrames so we limit our time in the synchronized block
        Collection<StackFrame> undoFrames = new ArrayList<DrawingToolLayer.StackFrame>(
                undoStack.capacity());
        for (Collection<Geometry> frame : undoStack) {
            undoFrames.add(new StackFrame(frame));
        }
        Collection<StackFrame> redoFrames = new ArrayList<DrawingToolLayer.StackFrame>(
                redoStack.capacity());
        for (Collection<Geometry> frame : redoStack) {
            redoFrames.add(new StackFrame(frame));
        }

        synchronized (currentData) {
            this.currentData.geometries.clear();
            this.currentData.geometries.addAll(currentData);
            this.undoStack.clear();
            this.undoStack.addAll(undoFrames);
            this.redoStack.clear();
            this.redoStack.addAll(redoFrames);
        }
    }
}
