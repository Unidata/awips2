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

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderable2;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * The individual polygon objects that comprise a {@code PolygonLayer}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  May 27, 2015  #4375     dgilling     Initial creation
 *  Jun 18, 2015  #4354     dgilling     Correct behavior of project.
 *  Jun 30, 2015  #4354     dgilling     Make PolygonLayer visible to
 *                                       subclasses.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class DrawablePolygon implements IRenderable2 {

    /** the polygon as represented in latlon coordinates **/
    private Polygon polygon;

    private IWireframeShape wireframeShape;

    private IShadedShape shadedShape;

    private final Object lock;

    /**
     * The PolygonLayer this polygon is attached to.
     */
    protected final PolygonLayer<?> polygonLayer;

    public DrawablePolygon(PolygonLayer<?> polygonLayer) {
        this.lock = new Object();
        this.polygonLayer = polygonLayer;
    }

    public DrawablePolygon(Polygon polygon, PolygonLayer<?> polygonLayer) {
        this(polygonLayer);
        this.polygon = polygon;
    }

    public DrawablePolygon(Coordinate[] coords, PolygonLayer<?> polygonLayer) {
        this(PolygonUtil.FACTORY.createPolygon(coords), polygonLayer);
    }

    @Override
    public PaintStatus paint(IGraphicsTarget target, IDescriptor descriptor,
            PaintProperties paintProps) throws VizException {
        // prepare to create lines of the polygon
        LinearRing ring = (LinearRing) polygon.getExteriorRing();
        Coordinate[] c = polygon.getCoordinates();
        RGB color = polygonLayer.getCapability(ColorableCapability.class)
                .getColor();
        OutlineCapability lineCap = polygonLayer
                .getCapability(OutlineCapability.class);
        float width = lineCap.getOutlineWidth();
        LineStyle style = lineCap.getLineStyle();

        synchronized (lock) {
            if (wireframeShape == null) {
                wireframeShape = target.createWireframeShape(true, descriptor);
            }
            if (shadedShape == null) {
                shadedShape = target.createShadedShape(false,
                        descriptor.getGridGeometry());
            }

            if (!wireframeShape.isDrawable() || !shadedShape.isDrawable()) {
                wireframeShape.addLineSegment(c);
                shadedShape.addPolygon(new LineString[] { ring }, color);
            }

            target.drawWireframeShape(wireframeShape, color, width, style);
            target.drawShadedShape(shadedShape, 0.5f);
        }

        // only show the vertices if in editable mode
        if (polygonLayer.getCapability(EditableCapability.class).isEditable()) {
            DrawableCircle[] vertices = new DrawableCircle[c.length - 1];
            double[] origCoord = new double[2];
            double[] pxCoord = new double[2];
            for (int i = 0; i < c.length - 1; i++) {
                DrawableCircle circle = new DrawableCircle();
                origCoord[0] = c[i].x;
                origCoord[1] = c[i].y;
                int polygonIdx = polygonLayer.uiInput.draggedVertexPolygonIndex;
                DrawablePolygon draggedPolygon = (polygonIdx > -1) ? polygonLayer.polygons
                        .get(polygonIdx) : null;
                /*
                 * Identity check is intended here; we only want to alter the
                 * vertex drawing behavior if this polygon is the same polygon
                 * that's having its vertex dragged.
                 */
                if ((draggedPolygon == this)
                        && (polygonLayer.uiInput.draggedVertexIndex == i)) {
                    /*
                     * they're dragging, replace this circle with mouse position
                     */
                    pxCoord = descriptor.getRenderableDisplay().screenToGrid(
                            polygonLayer.uiInput.lastX,
                            polygonLayer.uiInput.lastY, 0, target);
                } else {
                    pxCoord = descriptor.worldToPixel(origCoord);
                }
                circle.setCoordinates(pxCoord[0], pxCoord[1]);
                circle.screenRadius = PolygonLayer.VERTEX_RADIUS;
                circle.basics.color = color;
                circle.filled = true;
                vertices[i] = circle;
            }
            target.drawCircle(vertices);
        }

        return PaintStatus.PAINTED;
    }

    public Polygon getPolygon() {
        return polygon;
    }

    public void setPolygon(Polygon polygon) {
        this.polygon = polygon;
    }

    public void resetPolygon(Coordinate[] coords) {
        synchronized (lock) {
            if (coords != null && coords.length > 0) {
                polygon = PolygonUtil.FACTORY.createPolygon(coords);
            }

            resetPolygon();
        }
    }

    public void resetPolygon(DrawablePolygon newPolygon) {
        synchronized (lock) {
            if ((newPolygon != null) && (newPolygon.getPolygon() != null)) {
                polygon = newPolygon.getPolygon();
            }

            resetPolygon();
        }
    }

    public void resetPolygon() {
        synchronized (lock) {
            if (wireframeShape != null) {
                wireframeShape.reset();
            }
            if (shadedShape != null) {
                shadedShape.reset();
            }
        }
    }

    public void dispose() {
        synchronized (lock) {
            if (wireframeShape != null) {
                wireframeShape.dispose();
                wireframeShape = null;
            }
            if (shadedShape != null) {
                shadedShape.dispose();
                shadedShape = null;
            }
        }
    }

    public void project(CoordinateReferenceSystem crs) {
        synchronized (lock) {
            dispose();
            resetPolygon();
        }
    }
}
