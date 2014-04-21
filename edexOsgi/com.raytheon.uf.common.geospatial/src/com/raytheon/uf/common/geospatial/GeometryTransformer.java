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
package com.raytheon.uf.common.geospatial;

import java.util.ArrayList;
import java.util.List;

import org.geotools.geometry.jts.CoordinateSequenceTransformer;
import org.geotools.geometry.jts.DefaultCoordinateSequenceTransformer;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.CoordinateSequenceFactory;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Replacement for GeoTools GeometryCoordinateSequenceTransformer that can
 * handle conversion of polygons containing NaNs.
 * 
 * Had to copy the entire class and modify it since it was not written to be
 * subclassed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2014 #2997      randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GeometryTransformer {

    private MathTransform transform = null;

    private CoordinateReferenceSystem crs;

    private CoordinateSequenceTransformer inputCSTransformer = null;

    private CoordinateSequenceTransformer csTransformer = null;

    private GeometryFactory currGeometryFactory = null;

    /**
     * Creates a transformer which uses the {@link CoordinateSequenceFactory} of
     * the source geometries.
     */
    public GeometryTransformer() {
        // the csTransformer is initialized from the first geometry
        // and the supplied transform
    }

    /**
     * Creates a transformer which uses a client-specified
     * {@link CoordinateSequenceTransformer}.
     * <p>
     * <b>WARNING:</b> The CoordinateSequenceTransformer must use the same
     * {@link CoordinateSequenceFactory} as the output GeometryFactory, so that
     * geometries are constructed consistently.
     * 
     * @param transformer
     */
    public GeometryTransformer(CoordinateSequenceTransformer transformer) {
        inputCSTransformer = transformer;
        csTransformer = transformer;
    }

    /**
     * Sets the math transform to be used for transformation
     * 
     * @param transform
     */
    public void setMathTransform(MathTransform transform) {
        this.transform = transform;
    }

    /**
     * Sets the target coordinate reference system.
     * <p>
     * This value is used to set the coordinate reference system of geometries
     * after they have been transformed.
     * </p>
     * 
     * @param crs
     *            The target coordinate reference system.
     */
    public void setCoordinateReferenceSystem(CoordinateReferenceSystem crs) {
        this.crs = crs;
    }

    /**
     * Initializes the internal CoordinateSequenceTransformer if not specified
     * explicitly.
     * 
     * @param gf
     *            the factory to use
     */
    private void init(GeometryFactory gf) {
        // don't init if csTransformer already exists
        if (inputCSTransformer != null) {
            return;
        }
        // don't reinit if gf is the same (the usual case)
        if (currGeometryFactory == gf) {
            return;
        }

        currGeometryFactory = gf;
        CoordinateSequenceFactory csf = gf.getCoordinateSequenceFactory();
        csTransformer = new DefaultCoordinateSequenceTransformer(csf);
    }

    /**
     * Applies the transform to the provided geometry, creating a new
     * transformed geometry.
     * 
     * @param g
     *            the geometry to transform
     * @return a new transformed geometry
     * @throws TransformException
     */
    public Geometry transform(Geometry g) throws TransformException {
        GeometryFactory factory = g.getFactory();
        Geometry transformed = null;

        // lazily init csTransformer using geometry's CSFactory
        init(factory);

        if (g instanceof Point) {
            transformed = transformPoint((Point) g, factory);
        } else if (g instanceof MultiPoint) {
            MultiPoint mp = (MultiPoint) g;
            Point[] points = new Point[mp.getNumGeometries()];

            for (int i = 0; i < points.length; i++) {
                points[i] = transformPoint((Point) mp.getGeometryN(i), factory);
            }

            transformed = factory.createMultiPoint(points);
        } else if (g instanceof LineString) {
            transformed = transformLineString((LineString) g, factory);
        } else if (g instanceof MultiLineString) {
            MultiLineString mls = (MultiLineString) g;
            LineString[] lines = new LineString[mls.getNumGeometries()];

            for (int i = 0; i < lines.length; i++) {
                lines[i] = transformLineString(
                        (LineString) mls.getGeometryN(i), factory);
            }

            transformed = factory.createMultiLineString(lines);
        } else if (g instanceof Polygon) {
            transformed = transformPolygon((Polygon) g, factory);
        } else if (g instanceof MultiPolygon) {
            // changed from original GeoTools implementation
            // to handle transformPolygon possibly returning LineStrings instead for polygons
            MultiPolygon mp = (MultiPolygon) g;
            int n = mp.getNumGeometries();
            List<Geometry> polygons = new ArrayList<Geometry>(n);

            for (int i = 0; i < n; i++) {
                polygons.add(transformPolygon((Polygon) mp.getGeometryN(i),
                        factory));
            }

            transformed = factory.buildGeometry(polygons);
        } else if (g instanceof GeometryCollection) {
            GeometryCollection gc = (GeometryCollection) g;
            Geometry[] geoms = new Geometry[gc.getNumGeometries()];

            for (int i = 0; i < geoms.length; i++) {
                geoms[i] = transform(gc.getGeometryN(i));
            }

            transformed = factory.createGeometryCollection(geoms);
        } else {
            throw new IllegalArgumentException("Unsupported geometry type "
                    + g.getClass());
        }

        // copy over user data
        // do a special check for coordinate reference system
        transformed.setUserData(g.getUserData());

        if ((g.getUserData() == null)
                || (g.getUserData() instanceof CoordinateReferenceSystem)) {
            // set the new one to be the target crs
            if (crs != null) {
                transformed.setUserData(crs);
            }
        }

        return transformed;
    }

    /**
     * 
     * @param ls
     * @param gf
     * @return transformed lineString
     * @throws TransformException
     */
    public LineString transformLineString(LineString ls, GeometryFactory gf)
            throws TransformException {

        // if required, init csTransformer using geometry's CSFactory
        init(gf);

        CoordinateSequence cs = projectCoordinateSequence(ls
                .getCoordinateSequence());
        LineString transformed = null;
        
        // changed from original GeoTools implementation
        // to check if CoordinateSequence is closed and return LineString if not
        if (isClosed(cs)) {
            transformed = gf.createLinearRing(cs);
        } else {
            transformed = gf.createLineString(cs);
        }

        transformed.setUserData(ls.getUserData());
        return transformed;
    }

    // changed from original GeoTools implementation
    // added function to check if CoordinateSequence
    // is closed and contains enough points to be a 
    // LinearRing
    private boolean isClosed(CoordinateSequence cs) {
        if (cs.size() < 4) {
            return false;
        }
        return cs.getCoordinate(0).equals2D(cs.getCoordinate(cs.size() - 1));
    }

    /**
     * @param point
     * @param gf
     * @return transformed point
     * 
     * @throws TransformException
     */
    public Point transformPoint(Point point, GeometryFactory gf)
            throws TransformException {

        // if required, init csTransformer using geometry's CSFactory
        init(gf);

        CoordinateSequence cs = projectCoordinateSequence(point
                .getCoordinateSequence());
        Point transformed = gf.createPoint(cs);
        transformed.setUserData(point.getUserData());
        return transformed;
    }

    /**
     * @param cs
     *            a CoordinateSequence
     * @return
     * 
     * @throws TransformException
     */
    private CoordinateSequence projectCoordinateSequence(CoordinateSequence cs)
            throws TransformException {
        return csTransformer.transform(cs, transform);
    }

    /**
     * @param polygon
     * @param gf
     * @return transformed Polygon or MultiLineString if Polygon no longer
     *         closes
     * @throws TransformException
     */
    public Geometry transformPolygon(Polygon polygon, GeometryFactory gf)
            throws TransformException {
        // changed from original GeoTools implementation
        // to return LineStrings if polygon no longer closed due to NaNs
        // returned by projection transformation (point outside valid range for projection)
        LineString[] lineStrings = new LineString[polygon.getNumInteriorRing() + 1];

        lineStrings[0] = transformLineString(polygon.getExteriorRing(), gf);

        for (int i = 1; i < lineStrings.length; i++) {
            lineStrings[i] = transformLineString(
                    polygon.getInteriorRingN(i - 1), gf);
        }

        boolean closed = true;
        for (LineString ls : lineStrings) {
            if (!ls.isClosed()) {
                closed = false;
                break;
            }
        }

        Geometry transformed;
        if (closed) {
            LinearRing[] interiors = new LinearRing[lineStrings.length - 1];
            for (int i = 0; i < interiors.length; i++) {
                interiors[i] = (LinearRing) lineStrings[i + 1];
            }
            transformed = gf.createPolygon((LinearRing) lineStrings[0],
                    interiors);
        } else {
            transformed = gf.createMultiLineString(lineStrings);
        }
        transformed.setUserData(polygon.getUserData());
        return transformed;
    }
}
