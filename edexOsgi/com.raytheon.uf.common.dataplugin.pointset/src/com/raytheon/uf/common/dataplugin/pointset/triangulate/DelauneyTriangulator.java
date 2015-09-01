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
package com.raytheon.uf.common.dataplugin.pointset.triangulate;

import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.List;

import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.pointset.PointSetLocation;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder;
import com.vividsolutions.jts.triangulate.quadedge.QuadEdgeSubdivision;

/**
 * Class for building triangles out of the scattered Lon/Lat coordinates in a
 * {@link PointSetLocation}.
 * 
 * This class will project the coordinates to a different CRS when performing
 * triangulation to avoid problems near the poles or antimeridian. The CRS can
 * be provided in the constructor or one will be chosen using the
 * {@link TriangulationCrsFinder}.
 * 
 * This class has the ability to perform alpha shaping to avoid creating large
 * triangles for concave areas of the data. This class can use either dynamic
 * alpha value as described in the {@link DynamicAlphaIndexBufferBuilder} or a
 * constant alphaValue. Because the triangulation is performed on a projection
 * of the data, the alpha value may need to be inflated to take into account
 * distortions in projections, the larger the area the data covers, the more
 * inflation that may be necessary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class DelauneyTriangulator {

    protected final CoordinateReferenceSystem crs;

    protected final double alphaValue;

    protected final boolean dynamicAlpha;

    public DelauneyTriangulator(CoordinateReferenceSystem crs, double alphaValue) {
        this.crs = crs;
        this.alphaValue = alphaValue;
        this.dynamicAlpha = false;
    }

    public DelauneyTriangulator(CoordinateReferenceSystem crs,
            boolean dynamicAlpha) {
        this.crs = crs;
        this.alphaValue = Double.POSITIVE_INFINITY;
        this.dynamicAlpha = dynamicAlpha;
    }

    public DelauneyTriangulator(CoordinateReferenceSystem crs) {
        this.crs = crs;
        this.alphaValue = Double.POSITIVE_INFINITY;
        this.dynamicAlpha = false;
    }

    public DelauneyTriangulator(double alphaValue) {
        this.crs = null;
        this.alphaValue = alphaValue;
        this.dynamicAlpha = false;
    }

    public DelauneyTriangulator(boolean dynamicAlpha) {
        this.crs = null;
        this.alphaValue = Double.POSITIVE_INFINITY;
        this.dynamicAlpha = dynamicAlpha;
    }

    public DelauneyTriangulator() {
        this.crs = null;
        this.alphaValue = Double.POSITIVE_INFINITY;
        this.dynamicAlpha = false;
    }

    protected float[] interleaveOrdinates(PointSetLocation location) {
        FloatBuffer longitude = location.getLongitudes();
        FloatBuffer latitude = location.getLatitudes();
        int numPoints = longitude.capacity();

        float[] lonLats = new float[numPoints * 2];
        for (int i = 0; i < numPoints; i += 1) {
            lonLats[i * 2] = longitude.get(i);
            lonLats[i * 2 + 1] = latitude.get(i);
        }
        return lonLats;
    }

    public IntBuffer triangulate(PointSetLocation location)
            throws FactoryException, TransformException {
        float[] lonLats = interleaveOrdinates(location);
        int numPoints = lonLats.length / 2;
        MathTransform lonLatToCrs = null;
        if (crs == null) {
            lonLatToCrs = new TriangulationCrsFinder()
                    .getTransfromFromLonLat(lonLats);
        } else {
            lonLatToCrs = CRS
                    .findMathTransform(DefaultGeographicCRS.WGS84, crs);
        }
        float[] xy = new float[lonLats.length];
        lonLatToCrs.transform(lonLats, 0, xy, 0, numPoints);
        List<Coordinate> sites = new ArrayList<>(numPoints);
        for (int i = 0; i < numPoints; i += 1) {
            sites.add(new Coordinate(xy[i * 2], xy[i * 2 + 1], i));
        }
        DelaunayTriangulationBuilder builder = new DelaunayTriangulationBuilder();
        builder.setSites(sites);
        QuadEdgeSubdivision subdiv = builder.getSubdivision();
        int numEdges = subdiv.getEdges().size();

        AbstractIndexBufferBuilder indexer = null;
        if (dynamicAlpha) {
            indexer = new DynamicAlphaIndexBufferBuilder(numEdges * 2);
        } else {
            indexer = new IndexBufferBuilder(numEdges * 2, alphaValue);
        }
        subdiv.visitTriangles(indexer, false);
        return indexer.getBuffer();
    }

}
