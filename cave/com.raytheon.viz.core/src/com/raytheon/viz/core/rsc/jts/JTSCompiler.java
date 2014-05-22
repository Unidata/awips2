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
import org.opengis.referencing.FactoryException;

import com.raytheon.uf.common.geospatial.ReferencedGeometry;
import com.raytheon.uf.common.geospatial.util.WorldWrapCorrector;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

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
 *  Feb 14, 2014 2804        mschenke    Rewrote to move clipping from GLWireframeShape2D to here
 *  Apr 21, 2014 2997        randerso    Improved error handling in handle(ReferencedGeometry, JTSGeometryData)
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

    public static class JTSGeometryData {

        private RGB geometryColor;

        private boolean worldWrapCorrect;

        private PreparedGeometry clippingArea;

        private PointStyle pointStyle;

        protected JTSGeometryData() {

        }

        protected JTSGeometryData(JTSGeometryData data) {
            setGeometryColor(data.geometryColor);
            setWorldWrapCorrect(data.worldWrapCorrect);
            setClippingArea(data.clippingArea);
            setPointStyle(data.pointStyle);
        }

        public void setGeometryColor(RGB geometryColor) {
            this.geometryColor = geometryColor;
        }

        public void setWorldWrapCorrect(boolean worldWrapCorrect) {
            this.worldWrapCorrect = worldWrapCorrect;
        }

        private void setClippingArea(PreparedGeometry clippingArea) {
            this.clippingArea = clippingArea;
        }

        public void setClippingArea(Polygon clippingArea) {
            setClippingArea(PreparedGeometryFactory.prepare(clippingArea));
        }

        public void setClippingExtent(IExtent clippingExtent) {
            double[] center = clippingExtent.getCenter();
            PixelCoverage pc = new PixelCoverage(new Coordinate(center[0],
                    center[1]), clippingExtent.getWidth() + 2,
                    clippingExtent.getHeight() + 2);
            GeometryFactory factory = new GeometryFactory();
            setClippingArea(factory.createPolygon(
                    factory.createLinearRing(new Coordinate[] { pc.getLl(),
                            pc.getLr(), pc.getUr(), pc.getUl(), pc.getLl() }),
                    null));
        }

        public void setPointStyle(PointStyle pointStyle) {
            this.pointStyle = pointStyle;
        }

        protected RGB getGeometryColor() {
            return geometryColor;
        }

        protected boolean isWorldWrapCorrect() {
            return worldWrapCorrect;
        }

        protected PreparedGeometry getClippingArea() {
            return clippingArea;
        }

        protected boolean isClipping() {
            return clippingArea != null;
        }

        protected PointStyle getPointStyle() {
            return pointStyle;
        }

    }

    // TODO: parameterize this: also needs to be expressed in screen pixels
    // rather than arbitrary pixel units
    private static final double POINT_SIZE = 2.0;

    protected IShadedShape theShadedShape;

    protected IWireframeShape theWireframeShape;

    protected IDescriptor descriptor;

    private WorldWrapCorrector corrector;

    private JTSGeometryData defaultData;

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
        this.theShadedShape = shadedShp;
        this.theWireframeShape = wireShp;
        this.descriptor = descriptor;
        if (descriptor instanceof IMapDescriptor) {
            this.corrector = new WorldWrapCorrector(
                    descriptor.getGridGeometry());
        }
        JTSGeometryData data = new JTSGeometryData();
        data.setGeometryColor(DEFAULT_COLOR);
        data.setClippingExtent(new PixelExtent(descriptor.getGridGeometry()
                .getGridRange()));
        data.setWorldWrapCorrect(false);
        data.setPointStyle(PointStyle.SQUARE);
        this.defaultData = data;
    }

    /**
     * @deprecated Use constructor without {@link PointStyle} and add
     *             {@link PointStyle} to {@link JTSGeometryData} for a
     *             {@link Geometry}
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
    @Deprecated
    public JTSCompiler(IShadedShape shadedShp, IWireframeShape wireShp,
            IDescriptor descriptor, PointStyle pointStyle) {
        this(shadedShp, wireShp, descriptor);
        this.defaultData.setPointStyle(pointStyle);
    }

    /**
     * @return A new {@link JTSGeometryData} for use with the compiler
     */
    public JTSGeometryData createGeometryData() {
        return new JTSGeometryData(defaultData);
    }

    private static double[][] coordToDouble(Coordinate[] c) {
        double[][] pts = new double[c.length][3];
        for (int i = 0; i < pts.length; i++) {
            pts[i][0] = c[i].x;
            pts[i][1] = c[i].y;
            pts[i][2] = c[i].z;
        }
        return pts;
    }

    /**
     * Clips the {@link Geometry} passed in to the clipping area
     * 
     * @param geometry
     * @param clippingArea
     * @return A clipped Geometry with no parts outside the clippingArea
     */
    private static Geometry complexClip(Geometry geometry,
            PreparedGeometry clippingArea) {
        if (clippingArea.contains(geometry)) {
            // Fully contained, don't clip
            return geometry;
        } else if (clippingArea.intersects(geometry)) {
            if (geometry.isValid() == false) {
                // Don't try and clip invalid geometries
                return geometry;
            }
            // Intersects, partial clip
            return clippingArea.getGeometry().intersection(geometry);
        } else {
            // No intersection, return empty
            return new GeometryFactory()
                    .createGeometryCollection(new Geometry[0]);
        }
    }

    private void handlePolygon(Polygon poly, JTSGeometryData data) {
        int numInteriorRings = poly.getNumInteriorRing();
        LineString[] rings = new LineString[numInteriorRings + 1];
        rings[0] = poly.getExteriorRing();
        for (int k = 0; k < numInteriorRings; k++) {
            rings[k + 1] = poly.getInteriorRingN(k);
        }

        if (theWireframeShape != null) {
            for (LineString ls : rings) {
                handleLineString(ls, data);
            }
        }

        if (theShadedShape != null) {
            theShadedShape.addPolygonPixelSpace(rings, data.getGeometryColor());
        }
    }

    private void handleLineString(LineString line, JTSGeometryData data) {
        if (theWireframeShape != null) {
            theWireframeShape.addLineSegment(coordToDouble(line
                    .getCoordinates()));
        }
    }

    private void handlePoint(Point point, JTSGeometryData data) {
        Coordinate coord = point.getCoordinate();
        double[][] ll;

        switch (data.getPointStyle()) {
        case CROSS:
            ll = new double[2][3];
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

            break;

        default: // use SQUARE
            ll = new double[5][3];
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
            break;
        }

    }

    private void handleGeometryCollection(GeometryCollection coll,
            JTSGeometryData data, boolean clipped) throws VizException {
        int geoms = coll.getNumGeometries();
        for (int i = 0; i < geoms; i++) {
            Geometry g = coll.getGeometryN(i);
            disposition(g, data, clipped);
        }
    }

    private void disposition(Geometry geom, JTSGeometryData data,
            boolean clipped) throws VizException {
        if (geom instanceof GeometryCollection) {
            handleGeometryCollection((GeometryCollection) geom, data, clipped);
        } else if ((clipped == false) && data.isClipping()) {
            geom = complexClip(geom, data.getClippingArea());
            if (geom.isEmpty() == false) {
                disposition(geom, data, true);
            }
        } else if (geom instanceof Point) {
            handlePoint((Point) geom, data);
        } else if (geom instanceof LineString) {
            handleLineString((LineString) geom, data);
        } else if (geom instanceof Polygon) {
            // Polygon is the only type doing complex clipping so it needs to
            // know if the Polygon has been clipped already or not
            handlePolygon((Polygon) geom, data);
        } else {
            throw new VizException("Unknown geometry type: "
                    + geom.getClass().getName());
        }
    }

    /**
     * @deprecated Use {@link #handle(ReferencedGeometry, JTSGeometryData)}
     * @param referencedGeom
     * @param color
     * @throws VizException
     */
    @Deprecated
    public void handle(ReferencedGeometry referencedGeom, RGB color)
            throws VizException {
        JTSGeometryData data = createGeometryData();
        data.setGeometryColor(color);
        handle(referencedGeom, data);
    }

    /**
     * @deprecated Use {@link #handle(Geometry, JTSGeometryData)}
     * @param geom
     * @param wrapCorrect
     * @throws VizException
     */
    @Deprecated
    public void handle(Geometry geom, boolean wrapCheck) throws VizException {
        JTSGeometryData data = createGeometryData();
        data.setWorldWrapCorrect(wrapCheck);
        handle(geom, data);
    }

    /**
     * @deprecated Use {@link #handle(Geometry, JTSGeometryData)}
     * @param geom
     * @param color
     * @throws VizException
     */
    @Deprecated
    public void handle(Geometry geom, RGB color) throws VizException {
        JTSGeometryData data = createGeometryData();
        data.setGeometryColor(color);
        handle(geom, data);
    }

    /**
     * @deprecated Use {@link #handle(Geometry, JTSGeometryData)}
     * @param geom
     * @param color
     * @param wrapCorrect
     * @throws VizException
     */
    @Deprecated
    public void handle(Geometry geom, RGB color, boolean wrapCorrect)
            throws VizException {
        JTSGeometryData data = createGeometryData();
        data.setGeometryColor(color);
        data.setWorldWrapCorrect(wrapCorrect);
        handle(new ReferencedGeometry(geom), data);
    }

    /**
     * Handles a Geometry in lat/lon space
     * 
     * @param geom
     * @throws VizException
     */
    public void handle(Geometry geom) throws VizException {
        handle(geom, defaultData);
    }

    /**
     * Handles a Geometry in lat/lon space using the geometry data
     * 
     * @param geom
     * @param data
     * @throws VizException
     */
    public void handle(Geometry geom, JTSGeometryData data) throws VizException {
        handle(new ReferencedGeometry(geom), data);
    }

    /**
     * Handles the referenced geometry
     * 
     * @param geom
     * @throws VizException
     */
    public void handle(ReferencedGeometry geom) throws VizException {
        handle(geom, defaultData);
    }

    /**
     * Handles the referenced geometry using the geometry data
     * 
     * @param geom
     * @param data
     * @throws VizException
     */
    public void handle(ReferencedGeometry geom, JTSGeometryData data)
            throws VizException {
        if ((corrector != null) && corrector.needsCorrecting()
                && data.isWorldWrapCorrect()) {
            try {
                geom = new ReferencedGeometry(
                        corrector.correct(geom.asLatLon()));
            } catch (FactoryException e) {
                throw new VizException("Error creating transform to Lat/Lon", e);
            } catch (Exception e) {
                throw new VizException(
                        "Error transforming geometry into Lat/Lon", e);
            }
        }

        try {
            disposition(geom.asPixel(descriptor.getGridGeometry()), data, false);
        } catch (FactoryException e) {
            throw new VizException(
                    "Error creating transform to descriptor pixel space", e);
        } catch (Exception e) {
            throw new VizException(
                    "Error transforming geometry into descriptor pixel space",
                    e);
        }
    }
}
