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

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;

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
import com.raytheon.uf.viz.drawing.actions.ClearDrawingAction;
import com.raytheon.uf.viz.drawing.actions.EraseObjectsAction;
import com.raytheon.uf.viz.drawing.actions.RedoAddAction;
import com.raytheon.uf.viz.drawing.actions.UndoAddAction;
import com.raytheon.uf.viz.drawing.tools.ToolsUtils;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
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

    protected IGraphicsTarget target;

    protected boolean needsRefresh = true;

    protected boolean erase = false;

    private EraseObjectsAction eAction = null;

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
        if (tempWireframeShape == null || needsRefresh) {
            tempWireframeShape = target.createWireframeShape(true,
                    getDescriptor());
            needsRefresh = false;
        }

        RGB rgb = getCapability(ColorableCapability.class).getColor();

        OutlineCapability outline = getCapability(OutlineCapability.class);
        ColorableCapability colorable = getCapability(ColorableCapability.class);
        for (IWireframeShape sh : wireframeShapes.values()) {
            target.drawWireframeShape(sh, colorable.getColor(),
                    (float) outline.getOutlineWidth());
        }

        for (Geometry g : this.tempGeometries) {
            drawTempLinePrimitive(g, target, rgb);
        }
        target.drawWireframeShape(tempWireframeShape, colorable.getColor(),
                outline.getOutlineWidth(), outline.getLineStyle());
    }

    private void drawTempLinePrimitive(Geometry shape, IGraphicsTarget target,
            RGB color) throws VizException {
        LineString line = (LineString) shape;

        int pts = line.getNumPoints();
        for (int i = 1; i < pts; i++) {
            double[] p1 = this.descriptor
                    .worldToPixel(new double[] { line.getPointN(i - 1).getX(),
                            line.getPointN(i - 1).getY() });
            double[] p2 = this.descriptor.worldToPixel(new double[] {
                    line.getPointN(i).getX(), line.getPointN(i).getY() });
            double[][] coords = new double[2][2];
            coords[0][0] = p1[0];
            coords[0][1] = p1[1];
            coords[1][0] = p2[0];
            coords[1][1] = p2[1];
            tempWireframeShape.addLineSegment(coords);
        }
    }

    /**
     * Draw a line using pixel coordinates
     * 
     * UUID is optional, and generally should be null
     * 
     * @param line
     * @param isFinal
     * @param uuid
     */
    public void addLine(LineString line, String uuid) {
        tempWireframeShape.compile();
        wireframeShapes.put(line, tempWireframeShape);
    }

    public void addTempLine(LineString line) {
        if (!erase) {
            this.tempGeometries.add(line);
        } else {
            Map<Geometry, IWireframeShape> shapes = new HashMap<Geometry, IWireframeShape>();
            shapes.putAll(wireframeShapes);
            GeometryFactory factory = new GeometryFactory();
            for (Geometry geom : shapes.keySet()) {
                if (line.intersects(geom)) {
                    Point point = factory
                            .createPoint(line.getCoordinates()[line
                                    .getNumPoints() - 1]);
                    if (point.buffer(8).intersects(geom)) {
                        Geometry intersection = point.intersection(geom);
                        Geometry finalGeom = geom.difference(intersection);
                        deletedShapes.put(geom, wireframeShapes.remove(geom));
                        this.tempGeometries.add(finalGeom);
                        System.out.println("removing things");
                    }
                }
            }
        }
    }

    public void reset() {
        resetTemp();
        disposeInternal();
    }

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
        this.wireframeShapes.clear();
        this.deletedShapes.clear();
    }

    public void undoAdd() {
        if (!this.wireframeShapes.isEmpty()) {
            Geometry geom = this.wireframeShapes.keySet().toArray(
                    new Geometry[0])[this.wireframeShapes.size() - 1];
            deletedShapes.put(geom, this.wireframeShapes.remove(geom));
            issueRefresh();
        }
    }

    /**
     * Add the ability to remove and redo
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
     * @return the wireframeShapes
     */
    public IWireframeShape getTempWireframeShape() {
        return tempWireframeShape;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuContributor#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        ResourcePair pair = new ResourcePair();
        pair.setResource(this);
        if (eAction == null) {
            eAction = new EraseObjectsAction();
            eAction.setSelectedRsc(pair);
        } else {
            eAction.setSelectedRsc(pair);
        }
        ClearDrawingAction cAction = new ClearDrawingAction();
        cAction.setSelectedRsc(pair);
        cAction.setImageDescriptor(ToolsUtils.getImageDescriptor("remove.gif"));
        UndoAddAction uAction = new UndoAddAction();
        uAction.setSelectedRsc(pair);
        uAction.setImageDescriptor(ToolsUtils.getImageDescriptor("undo.gif"));
        RedoAddAction rAction = new RedoAddAction();
        rAction.setSelectedRsc(pair);
        rAction.setImageDescriptor(ToolsUtils.getImageDescriptor("redo.gif"));
        menuManager.add(eAction);
        menuManager.add(cAction);
        menuManager.add(uAction);
        menuManager.add(rAction);
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
