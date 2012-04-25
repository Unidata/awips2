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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.drawing.events.DrawingEvent;
import com.raytheon.uf.viz.drawing.events.DrawingEventBus;
import com.raytheon.uf.viz.drawing.events.DrawingListener;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.TopologyException;

/**
 * Implements a basic drawing layer
 * 
 * @author chammack
 * 
 */
public class DrawingLayer extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> {
    public static enum LayerState {
        DRAWING, ERASING, NONE;
    }

    protected List<Geometry> tempGeometries;

    protected Map<Geometry, IWireframeShape> wireframeShapes;

    protected Map<Geometry, IWireframeShape> deletedShapes;

    protected IWireframeShape tempWireframeShape;

    protected IWireframeShape eraseWireframeShape;

    protected IGraphicsTarget target;

    protected LayerState state;

    protected PaintProperties paintProps = null;

    private boolean needsRefresh = false;

    protected RGB color;

    protected OutlineCapability outline;

    // allowing for others to get events from the drawing tool, just having to
    // subscribe
    private EventBus eventBus;

    private DrawingListener eventListener = null;

    public DrawingLayer(PathDrawingResourceData data, LoadProperties props) {
        super(data, props);
        eventBus = DrawingEventBus.getEventBus();
        eventListener = DrawingEventBus.getDrawingListener();
        eventBus.register(eventListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    public String getName() {
        return "Telestrator Drawing Tool";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this, true);
        this.tempGeometries = new ArrayList<Geometry>();
        this.wireframeShapes = new LinkedHashMap<Geometry, IWireframeShape>();
        this.deletedShapes = new LinkedHashMap<Geometry, IWireframeShape>();

        this.target = target;
        outline = getCapability(OutlineCapability.class);
        color = getCapability(ColorableCapability.class).getColor();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.paintProps = paintProps;
        this.color = getCapability(ColorableCapability.class).getColor();
        if (tempWireframeShape == null || needsRefresh) {
            tempWireframeShape = target.createWireframeShape(true,
                    getDescriptor());
            needsRefresh = false;
        }
        if (eraseWireframeShape == null) {
            eraseWireframeShape = target.createWireframeShape(true,
                    getDescriptor());
        }

        outline = getCapability(OutlineCapability.class);

        for (IWireframeShape sh : wireframeShapes.values()) {
            target.drawWireframeShape(sh, color,
                    (float) outline.getOutlineWidth(), outline.getLineStyle());
        }

        if (state == LayerState.DRAWING) {
            for (Geometry g : this.tempGeometries) {
                drawTempLinePrimitive(g, tempWireframeShape);
            }
            target.drawWireframeShape(tempWireframeShape, color,
                    outline.getOutlineWidth(), outline.getLineStyle());
        }
    }

    /**
     * Add the geometry to the wireframe shape that is passed in
     * 
     * @param shape
     * @param wShape
     */
    protected void drawTempLinePrimitive(Geometry shape, IWireframeShape wShape) {
        LineString line = (LineString) shape;

        int pts = line.getNumPoints();

        for (int i = 1; i < pts; i++) {
            double[] p1 = new double[] { line.getPointN(i - 1).getX(),
                    line.getPointN(i - 1).getY() };
            double[] p2 = new double[] { line.getPointN(i).getX(),
                    line.getPointN(i).getY() };
            double[][] coords = new double[2][2];
            coords[0][0] = p1[0];
            coords[0][1] = p1[1];
            coords[1][0] = p2[0];
            coords[1][1] = p2[1];
            wShape.addLineSegment(coords);
        }
    }

    /**
     * Convert from world to gl pixels
     * 
     * @param line
     * @param i
     * @return
     */
    public LineString convertPixels(LineString line) {
        int pts = line.getNumPoints();
        GeometryFactory factory = new GeometryFactory();
        List<Coordinate> coords = new ArrayList<Coordinate>();
        for (int i = 0; i < pts; i++) {
            double[] point = this.descriptor.worldToPixel(new double[] {
                    line.getPointN(i).getX(), line.getPointN(i).getY() });
            coords.add(new Coordinate(point[0], point[1]));
        }
        return factory.createLineString(coords.toArray(new Coordinate[0]));
    }

    /**
     * Finalize a temporary line by putting it in the map of all the drawn
     * shapes
     * 
     * UUID is optional, an generally should be null
     * 
     * @param line
     * @param isFinal
     * @param uuid
     */
    public void finalizeLine(LineString line, String uuid) {
        if (state == LayerState.DRAWING) {
            tempWireframeShape.compile();
            wireframeShapes.put(line, tempWireframeShape);
        }
        // this will update the toolbar if necessary
        DrawingEvent event = new DrawingEvent();
        eventBus.post(event);
    }

    public void addTempDrawLine(LineString line) {
        this.tempGeometries.add(line);
    }

    public void addTempEraseLine(LineString line) {
        // this.tempGeometries.add(line);
        Map<Geometry, IWireframeShape> shapes = new HashMap<Geometry, IWireframeShape>();
        shapes.putAll(wireframeShapes);
        for (Geometry geom : shapes.keySet()) {
            double extentPercentageX = paintProps.getView().getExtent()
                    .getWidth()
                    / (double) paintProps.getCanvasBounds().width;
            double cursorSize = 16;
            double size = extentPercentageX * cursorSize;
            if (line.buffer(size / 2).intersects(geom)) {
                Geometry intersection = line.buffer(size / 2)
                        .intersection(geom);
                Geometry finalGeom = null;
                try {
                    finalGeom = geom.difference(intersection);
                } catch (TopologyException e) {
                    continue;
                }
                deletedShapes.put(geom, wireframeShapes.remove(geom));

                Geometry lString = null;
                // should be split into multiple pieces (half or more)
                if (finalGeom instanceof MultiLineString) {
                    lString = (MultiLineString) finalGeom;
                    for (int j = 0; j < lString.getNumGeometries(); j++) {
                        LineString lineString = (LineString) lString
                                .getGeometryN(j);
                        eraseWireframeShape = target.createWireframeShape(true,
                                descriptor);
                        drawTempLinePrimitive(lineString, eraseWireframeShape);
                        this.wireframeShapes.put(lineString,
                                eraseWireframeShape);
                    }
                }
                if (finalGeom instanceof LineString) {
                    GeometryFactory factory = new GeometryFactory();
                    lString = (LineString) finalGeom;
                    Point point = factory
                            .createPoint(lString.getCoordinates()[0]);
                    intersection = point.buffer(size / 2).intersection(geom);
                    finalGeom = geom.difference(intersection);
                    eraseWireframeShape = target.createWireframeShape(true,
                            descriptor);
                    drawTempLinePrimitive(lString, eraseWireframeShape);
                    this.wireframeShapes.put(lString, eraseWireframeShape);
                }
            }
            tempGeometries.clear();
        }
    }

    public void reset() {
        resetTemp();
        disposeInternal();
        issueRefresh();
    }

    /**
     * reset the temporary geometries so that we can start a new line
     */
    public void resetTemp() {
        this.tempGeometries.clear();
        needsRefresh = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        for (IWireframeShape shape : this.wireframeShapes.values()) {
            shape.dispose();
        }
        for (IWireframeShape shape : this.deletedShapes.values()) {
            shape.dispose();
        }
        if (this.tempWireframeShape != null) {
            this.tempWireframeShape.dispose();
        }
        if (this.eraseWireframeShape != null) {
            this.eraseWireframeShape.dispose();
        }

        this.wireframeShapes.clear();
        this.deletedShapes.clear();
    }

    /**
     * Remove the last drawn shape
     */
    public void undoAdd() {
        if (!this.wireframeShapes.isEmpty()) {
            Geometry geom = this.wireframeShapes.keySet().toArray(
                    new Geometry[0])[this.wireframeShapes.size() - 1];
            deletedShapes.put(geom, this.wireframeShapes.remove(geom));
            issueRefresh();
        }
    }

    /**
     * Redraw the last deleted shape
     */
    public void redoAdd() {
        if (!deletedShapes.isEmpty()) {
            Geometry geom = this.deletedShapes.keySet()
                    .toArray(new Geometry[0])[this.deletedShapes.size() - 1];
            this.wireframeShapes.put(geom, this.deletedShapes.remove(geom));
            issueRefresh();
        }
    }

    /**
     * @return the deletedShapes, these shapes will get disposed when the clear
     *         button is selected
     */
    public Map<Geometry, IWireframeShape> getDeletedShapes() {
        return deletedShapes;
    }

    /**
     * @return the wireframeShapes, these shapes will get disposed when the
     *         clear button is selected
     */
    public Map<Geometry, IWireframeShape> getWireframeShapes() {
        return wireframeShapes;
    }

    /**
     * @return the eventBus
     */
    public EventBus getEventBus() {
        return eventBus;
    }

    /**
     * @return the eventListener
     */
    public DrawingListener getEventListener() {
        return eventListener;
    }

    /**
     * @return the state
     */
    public LayerState getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(LayerState state) {
        this.state = state;
    }
    // /*
    // * (non-Javadoc)
    // *
    // * @see
    // * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
    // * referencing.crs.CoordinateReferenceSystem)
    // */
    // @Override
    // public void project(CoordinateReferenceSystem crs) throws VizException {
    // super.project(crs);
    // for (Geometry geom : wireframeShapes.keySet()) {
    // IWireframeShape shape = target.createWireframeShape(true,
    // getDescriptor());
    // for (int i = 0; i < geom.getNumGeometries(); i++) {
    // Geometry ls = convertPixels((LineString) geom.getGeometryN(i));
    // drawTempLinePrimitive(ls, shape);
    // }
    // wireframeShapes.put(geom, shape);
    // }
    // }
}