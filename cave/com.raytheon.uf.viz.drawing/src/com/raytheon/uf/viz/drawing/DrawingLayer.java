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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Implements a basic drawing layer
 * 
 * @author chammack
 * 
 */
public class DrawingLayer extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> implements
        IContextMenuContributor {

    protected List<Geometry> tempGeometries;

    protected Map<Geometry, IWireframeShape> wireframeShapes;

    protected Map<Geometry, IWireframeShape> deletedShapes;

    protected IWireframeShape tempWireframeShape;

    protected IWireframeShape eraseWireframeShape;

    // TODO take this out
    private List<IWireframeShape> erasedShapes;

    protected IGraphicsTarget target;

    protected boolean erase = false;

    private PaintProperties paintProps = null;

    private boolean needsRefresh = false;

    public DrawingLayer(PathDrawingResourceData data, LoadProperties props) {
        super(data, props);
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
        this.tempGeometries = new ArrayList<Geometry>();
        this.wireframeShapes = new LinkedHashMap<Geometry, IWireframeShape>();
        this.deletedShapes = new LinkedHashMap<Geometry, IWireframeShape>();

        this.erasedShapes = new ArrayList<IWireframeShape>();
        this.target = target;

        getCapability(OutlineCapability.class);
        getCapability(ColorableCapability.class);
        getCapability(EditableCapability.class);
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
        if (tempWireframeShape == null || needsRefresh) {
            tempWireframeShape = target.createWireframeShape(true,
                    getDescriptor());
            needsRefresh = false;
        }
        if (eraseWireframeShape == null) {
            eraseWireframeShape = target.createWireframeShape(true,
                    getDescriptor());
        }

        OutlineCapability outline = getCapability(OutlineCapability.class);
        ColorableCapability colorable = getCapability(ColorableCapability.class);

        // remove after debugging
        // MagnificationCapability magnification =
        // getCapability(MagnificationCapability.class);
        // for (Geometry ls : wireframeShapes.keySet()) {
        // if (ls instanceof LineString) {
        // LineString string = (LineString) ls;
        // for (int i = 0; i < string.getNumPoints(); i++) {
        // target.drawPoint(string.getPointN(i).getX(), string
        // .getPointN(i).getY(), 0, colorable.getColor(),
        // PointStyle.BOX, magnification.getMagnification()
        // .floatValue());
        // DrawableString dString = new DrawableString(string
        // .getPointN(i).getCoordinate().toString(),
        // colorable.getColor());
        // dString.basics.x = string.getPointN(i).getX();
        // dString.basics.y = string.getPointN(i).getY();
        // target.drawStrings(dString);
        // }
        // }
        // }
        for (IWireframeShape sh : wireframeShapes.values()) {
            target.drawWireframeShape(sh, colorable.getColor(),
                    (float) outline.getOutlineWidth(), outline.getLineStyle());
        }

        for (Geometry g : this.tempGeometries) {
            drawTempLinePrimitive(g, tempWireframeShape);
        }
        // if (erase) {
        // target.drawWireframeShape(tempWireframeShape, new RGB(255, 0, 0),
        // 1.0f);
        // } else {
        target.drawWireframeShape(tempWireframeShape, colorable.getColor(),
                outline.getOutlineWidth(), outline.getLineStyle());
        // }
    }

    private void drawTempLinePrimitive(Geometry shape, IWireframeShape wShape) {
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
     * UUID is optional, and generally should be null
     * 
     * @param line
     * @param isFinal
     * @param uuid
     */
    public void finalizeLine(LineString line, String uuid) {
        tempWireframeShape.compile();
        wireframeShapes.put(line, tempWireframeShape);
    }

    public void addTempDrawLine(LineString line) {
        this.tempGeometries.add(line);
    }

    public void addTempEraseLine(LineString line) {
        this.tempGeometries.add(line);
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
                Geometry finalGeom = geom.difference(intersection);
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
                        erasedShapes.add(eraseWireframeShape);
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
                    erasedShapes.add(eraseWireframeShape);
                } else {
                    lString = (GeometryCollection) finalGeom;
                    // for (int j = 0; j < lString.getNumGeometries(); j++) {
                    // System.out.println(lString.getGeometryN(j).getClass());
                    // }
                    // System.out.println(finalGeom.getClass() + " has "
                    // + lString.getNumGeometries() + " geometries");
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

    public void resetTemp() {
        // this.tempWireframeShape.dispose();
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
        this.tempWireframeShape.dispose();
        this.eraseWireframeShape.dispose();

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
     * Add items to the right click menu, in this case just a button to launch
     * the Drawing toolbar
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        ResourcePair pair = new ResourcePair();
        pair.setResource(this);
        Action action = new Action("Draw Toolbar...") {
            public void run() {
                PathToolbar.getToolbar().open();
            };
        };
        menuManager.add(action);
    }

    /**
     * @return the erase
     */
    public boolean isErase() {
        return erase;
    }

    /**
     * @param erase
     *            the erase to set
     */
    public void setErase(boolean erase) {
        this.erase = erase;
    }

    /**
     * @return the deletedShapes
     */
    public Map<Geometry, IWireframeShape> getDeletedShapes() {
        return deletedShapes;
    }

    /**
     * @return the wireframeShapes
     */
    public Map<Geometry, IWireframeShape> getWireframeShapes() {
        return wireframeShapes;
    }
}
