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
package com.raytheon.viz.core.rsc.jts;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.geospatial.ReferencedGeometry;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.WorldWrapCorrector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Compile JTS Objects into renderable obs
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Oct 24, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class JTSCompiler {

    private static final RGB DEFAULT_COLOR = new RGB(255, 255, 255);

    public static enum PointStyle {
        SQUARE, CROSS
    };

    // TODO: parameterize this: also needs to be expressed in screen pixels
    // rather than arbitrary pixel units
    private static final double POINT_SIZE = 2.0;

    protected IShadedShape theShadedShape;

    protected IWireframeShape theWireframeShape;

    protected IDescriptor descriptor;

    protected PointStyle pointStyle;

    private WorldWrapCorrector corrector;

    /**
     * Constructor
     * 
     * @param shadedShp
     *            the shaded shape object (can be null)
     * @param wireShp
     *            the wireframe shape object
     * @param descriptor
     *            the descriptor
     */
    public JTSCompiler(IShadedShape shadedShp, IWireframeShape wireShp,
            IDescriptor descriptor) {
        this(shadedShp, wireShp, descriptor, PointStyle.SQUARE);
    }

    /**
     * Constructor
     * 
     * @param shadedShp
     *            the shaded shape object (can be null)
     * @param wireShp
     *            the wireframe shape object
     * @param descriptor
     *            the descriptor
     * @param pointStyle
     *            the pointStyle
     */
    public JTSCompiler(IShadedShape shadedShp, IWireframeShape wireShp,
            IDescriptor descriptor, PointStyle pointStyle) {
        this.theShadedShape = shadedShp;
        this.theWireframeShape = wireShp;
        this.descriptor = descriptor;
        this.pointStyle = pointStyle;
        if (descriptor instanceof IMapDescriptor) {
            this.corrector = new WorldWrapCorrector((IMapDescriptor) descriptor);
        }
    }

    private void handlePolygon(Polygon poly, RGB color) {

        LineString g;
        LineString g2;

        Coordinate[] c;
        LineString[] rings = null;

        // long tRead1 = System.nanoTime();

        int numRings;

        g = poly.getExteriorRing();
        c = g.getCoordinates();

        if (theWireframeShape != null) {
            double[][] pts = coordToDouble(c);
            theWireframeShape.addLineSegment(pts);
        }

        numRings = poly.getNumInteriorRing();

        if (theShadedShape != null) {
            rings = new LineString[numRings + 1];
            rings[0] = g;
        }

        // Handle inner rings
        for (int k = 0; k < numRings; k++) {
            g2 = poly.getInteriorRingN(k);
            c = g2.getCoordinates();
            if (theWireframeShape != null) {
                double[][] pts = coordToDouble(c);
                theWireframeShape.addLineSegment(pts);
            }
            if (theShadedShape != null) {
                rings[k + 1] = g2;
            }
        }

        if (theShadedShape != null) {
            theShadedShape.addPolygonPixelSpace(rings, color);
        }

    }

    private double[][] coordToDouble(Coordinate[] c) {
        double[][] pts = new double[c.length][3];
        for (int i = 0; i < pts.length; i++) {
            pts[i][0] = c[i].x;
            pts[i][1] = c[i].y;
            pts[i][2] = 0;
        }
        return pts;
    }

    private void handleLineString(LineString line, RGB color) {
        if (theWireframeShape != null) {
            Coordinate[] c = line.getCoordinates();
            double[][] pts = coordToDouble(c);
            theWireframeShape.addLineSegment(pts);
        }

    }

    private void handlePoint(Point point, RGB color) {
        // make a square point out of each "point"

        Coordinate[] c = point.getCoordinates();

        switch (pointStyle) {
        case CROSS:
            for (Coordinate coord : c) {
                double[][] ll = new double[2][3];
                ll[0][0] = coord.x - POINT_SIZE;
                ll[0][1] = coord.y;
                ll[0][2] = 0;

                ll[1][0] = coord.x + POINT_SIZE;
                ll[1][1] = coord.y;
                ll[1][2] = 0;

                if (theWireframeShape != null) {
                    theWireframeShape.addLineSegment(ll);
                }

                ll[0][0] = coord.x;
                ll[0][1] = coord.y - POINT_SIZE;
                ll[0][2] = 0;

                ll[1][0] = coord.x;
                ll[1][1] = coord.y + POINT_SIZE;
                ll[1][2] = 0;

                if (theWireframeShape != null) {
                    theWireframeShape.addLineSegment(ll);
                }
            }

            break;

        default: // use SQUARE
            for (Coordinate coord : c) {

                double[][] ll = new double[5][3];
                // UL
                ll[0][0] = coord.x - POINT_SIZE;
                ll[0][1] = coord.y - POINT_SIZE;
                ll[0][2] = 0;

                // UR
                ll[1][0] = coord.x - POINT_SIZE;
                ll[1][1] = coord.y + POINT_SIZE;
                ll[1][2] = 0;

                // LR
                ll[2][0] = coord.x + POINT_SIZE;
                ll[2][1] = coord.y + POINT_SIZE;
                ll[2][2] = 0;

                // LL
                ll[3][0] = coord.x + POINT_SIZE;
                ll[3][1] = coord.y - POINT_SIZE;
                ll[3][2] = 0;

                // Closing point
                ll[4] = ll[0];

                if (theWireframeShape != null) {
                    theWireframeShape.addLineSegment(ll);
                }
            }
            break;
        }

    }

    private void handleGeometryCollection(GeometryCollection coll, RGB color)
            throws VizException {
        int geoms = coll.getNumGeometries();
        for (int i = 0; i < geoms; i++) {
            Geometry g = coll.getGeometryN(i);
            disposition(g, color);
        }
    }

    private void disposition(Geometry geom, RGB color) throws VizException {
        if (geom instanceof Point) {
            handlePoint((Point) geom, color);
        } else if (geom instanceof GeometryCollection) {
            handleGeometryCollection((GeometryCollection) geom, color);
        } else if (geom instanceof LineString) {
            handleLineString((LineString) geom, color);
        } else if (geom instanceof Polygon) {
            handlePolygon((Polygon) geom, color);
        } else {
            throw new VizException("Unknown geometry type: "
                    + geom.getClass().getName());
        }
    }

    public void handle(ReferencedGeometry referencedGeom, RGB color)
            throws VizException {
        if (color == null) {
            color = DEFAULT_COLOR;
        }

        Geometry geom;
        try {
            geom = referencedGeom.asPixel(this.descriptor.getGridGeometry());
        } catch (Exception e) {
            throw new VizException("Unable to transform polygon", e);
        }

        disposition(geom, color);

    }

    /**
     * Handle lat lon based geometries, destructive to the passed in geometry.
     * 
     * @param geom
     * @throws VizException
     */
    public void handle(Geometry geom) throws VizException {
        handle(geom, DEFAULT_COLOR);
    }

    /**
     * Handle lat lon based geometries, destructive to the passed in geometry.
     * 
     * @param geom
     * @param wrapCheck
     * @throws VizException
     */
    public void handle(Geometry geom, boolean wrapCheck) throws VizException {
        handle(geom, DEFAULT_COLOR, wrapCheck);
    }

    /**
     * Handle lat lon based geometries, destructive to the passed in geometry.
     * 
     * @param geom
     * @param color
     * @throws VizException
     */
    public void handle(Geometry geom, RGB color) throws VizException {
        handle(geom, color, false);
    }

    /**
     * Handle lat lon based geometries, destructive to the passed in geometry.
     * 
     * @param geom
     * @param color
     * @param wrapCorrect
     * @throws VizException
     */
    public void handle(Geometry geom, RGB color, boolean wrapCorrect)
            throws VizException {
        if (wrapCorrect && corrector != null) {
            geom = corrector.correct(geom);
        }

        handle(new ReferencedGeometry(geom), color);
    }
}
