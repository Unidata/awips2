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
package com.raytheon.uf.viz.drawing.polygon;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;

import org.eclipse.jface.action.IMenuManager;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

/**
 * A layer for displaying a filled polygon on a map and altering it through
 * mouse interactions. This layer only supports an exterior ring, ie a polygon
 * without holes/interior rings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2015  3974      njensen     Initial creation
 * Mar 31, 2015  3977      nabowle     Require non-empty coordinates in resetPolygon
 * May 15, 2015  4375      dgilling    Support multiple polygons.
 * Jun 12, 2015  4375      dgilling    Only show AddVertexAction when on polygon's
 *                                     edge and not near a current vertex.
 * Jun 17, 2015  4354      dgilling    Fix bugs that would clear polygons on 
 *                                     capability change or reproject.
 * 
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 * @param <T>
 */

public class PolygonLayer<T extends AbstractResourceData> extends
        AbstractVizResource<T, MapDescriptor> implements
        IContextMenuContributor {

    protected static final double VERTEX_RADIUS = 7.0;

    protected PolygonInputAdapter uiInput = new PolygonInputAdapter(this);

    protected final LinkedList<DrawablePolygon> polygons;

    public PolygonLayer(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapability(EditableCapability.class).setEditable(true);
        getCapability(OutlineCapability.class).setOutlineWidth(2);
        this.polygons = new LinkedList<>();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this,
                getCapability(EditableCapability.class).isEditable());
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(uiInput);
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        for (DrawablePolygon drawPolygon : polygons) {
            drawPolygon.paint(target, getDescriptor(), paintProps);
        }
    }

    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(uiInput);
        }

        for (DrawablePolygon drawPolygon : polygons) {
            drawPolygon.dispose();
        }
    }

    /**
     * Replaces the specified polygon with a new polygon based on the
     * coordinates and makes it the top-most polygon in the layer.
     * 
     * @param index
     *            Index of polygon to replace.
     * @param coords
     *            Coordinates of the vertices (in world space) of the new
     *            polygon.
     */
    public void resetPolygon(int index, Coordinate[] coords) {
        if ((index < polygons.size()) && (coords != null && coords.length > 0)) {
            DrawablePolygon polygon = polygons.remove(index);
            polygon.resetPolygon(coords);
            polygons.push(polygon);
            issueRefresh();
        }
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        if (type.equals(ChangeType.CAPABILITY)) {
            if (polygons != null) {
                for (DrawablePolygon polygon : polygons) {
                    polygon.resetPolygon();
                }
            }
            issueRefresh();
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        for (DrawablePolygon drawPolygon : polygons) {
            drawPolygon.project(crs);
        }
        issueRefresh();
    }

    public Polygon getPolygon(int index) {
        return polygons.get(index).getPolygon();
    }

    public void setPolygon(int index, Polygon polygon) {
        resetPolygon(index, polygon.getExteriorRing().getCoordinates());
    }

    public void resetPolygons(Collection<DrawablePolygon> newPolygons) {
        if ((polygons != null) && (!newPolygons.isEmpty())) {
            resizePolygonStack(newPolygons.size());

            int i = 0;
            for (DrawablePolygon newPolygon : newPolygons) {
                polygons.get(i).resetPolygon(newPolygon);
                i++;
            }

            issueRefresh();
        }
    }

    private void resizePolygonStack(int newSize) {
        int currentSize = polygons.size();
        if (newSize > currentSize) {
            int toAdd = newSize - currentSize;
            for (int i = 0; i < toAdd; i++) {
                polygons.add(getNewDrawable());
            }
        } else if (currentSize > newSize) {
            int toDelete = currentSize - newSize;
            for (int i = 0; i < toDelete; i++) {
                DrawablePolygon polygonToDelete = polygons.removeLast();
                polygonToDelete.dispose();
            }
        }
    }

    protected DrawablePolygon getNewDrawable() {
        return new DrawablePolygon(this);
    }

    public void addPolygon(Coordinate[] coords) {
        addPolygon(new DrawablePolygon(coords, this));
    }

    protected void addPolygon(DrawablePolygon newPolygon) {
        polygons.push(newPolygon);
    }

    public void deletePolygon(int index) {
        DrawablePolygon polygonToDelete = polygons.remove(index);
        polygonToDelete.dispose();
    }

    public Collection<Polygon> getPolygons() {
        Collection<Polygon> retVal = new ArrayList<>(polygons.size());
        for (DrawablePolygon drawPolygon : polygons) {
            retVal.add(drawPolygon.getPolygon());
        }
        return retVal;
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
        if (!getCapability(EditableCapability.class).isEditable()) {
            return;
        }

        int edgePolygonIdx = uiInput.pointOnEdge(x, y);
        boolean onEdge = (edgePolygonIdx >= 0);

        int[] indices = uiInput.pointOnVertex(x, y);
        boolean onVertex = (indices != null);

        if (onEdge && !onVertex) {
            menuManager.add(new AddVertexAction(edgePolygonIdx, x, y, uiInput));
        }
        if (onVertex) {
            int polygonIndex = indices[0];
            int vertexIndex = indices[1];
            menuManager.add(new RemoveVertexAction(polygonIndex, vertexIndex,
                    uiInput));
        }

        int onPolygonIdx = uiInput.pointOnPolygon(x, y);
        boolean onPolygon = (onPolygonIdx >= 0);
        if (!onEdge && !onVertex && !onPolygon) {
            menuManager.add(new AddPolygonAction(x, y, uiInput));
        }

        if (onPolygon) {
            menuManager.add(new DeletePolygonAction(onPolygonIdx, uiInput));
        }
    }
}
