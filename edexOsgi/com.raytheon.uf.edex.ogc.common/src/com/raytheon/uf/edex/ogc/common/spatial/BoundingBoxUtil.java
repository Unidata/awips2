/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import net.opengis.gml.v_3_1_1.EnvelopeType;
import net.opengis.ows.v_1_1_0.BoundingBoxType;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralDirectPosition;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultCompoundCRS;
import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.SingleCRS;
import org.opengis.referencing.cs.CoordinateSystem;
import org.opengis.referencing.cs.CoordinateSystemAxis;

import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.gml3_1_1.EnvelopeConverter;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utilities for Bounding box parsing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BoundingBoxUtil {
    
    public static Unit<?> FL_UNIT = VerticalCoordinate.FLIGHT_LEVEL_UNIT;

    /**
     * Split 3D envelope into composite parts
     * 
     * @param env
     * @return
     * @throws OgcException
     */
    public static Composite3DBoundingBox separate3DEnvelope(EnvelopeType env)
            throws OgcException {
        if (EnvelopeConverter.getDims(env) != 3) {
            throw new OgcException(Code.InvalidParameterValue,
                    "3D bounding box requires 3 dimensions");
        }
        if (!env.isSetSrsName()) {
            throw new OgcException(Code.MissingParameterValue,
                    "Missing SRS name in bounding box");
        }
        Coordinate[] coords = new EnvelopeConverter().getCoordinates(env);
        GeneralDirectPosition min = new GeneralDirectPosition(new double[] {
                coords[0].x, coords[0].y, coords[0].z });
        GeneralDirectPosition max = new GeneralDirectPosition(new double[] {
                coords[1].x, coords[1].y, coords[1].z });

        return separate3DEnvelope(min, max, env.getSrsName());
    }

    /**
     * Split 3D envelope into composite parts
     * 
     * @param env
     * @return
     * @throws OgcException
     */
    public static Composite3DBoundingBox separate3DEnvelope(
            net.opengis.gml.v_3_2_1.EnvelopeType env) throws OgcException {
        if (com.raytheon.uf.edex.ogc.common.gml3_2_1.EnvelopeConverter
                .getDims(env) != 3) {
            throw new OgcException(Code.InvalidParameterValue,
                    "3D bounding box requires 3 dimensions");
        }
        if (!env.isSetSrsName()) {
            throw new OgcException(Code.MissingParameterValue,
                    "Missing SRS name in bounding box");
        }
        Coordinate[] coords = new com.raytheon.uf.edex.ogc.common.gml3_2_1.EnvelopeConverter()
                .getCoordinates(env);
        GeneralDirectPosition min = new GeneralDirectPosition(new double[] {
                coords[0].x, coords[0].y, coords[0].z });
        GeneralDirectPosition max = new GeneralDirectPosition(new double[] {
                coords[1].x, coords[1].y, coords[1].z });

        return separate3DEnvelope(min, max, env.getSrsName());
    }

    /**
     * Split 3D envelope into composite parts
     * 
     * @param bbox3d
     * @return
     * @throws OgcException
     */
    public static Composite3DBoundingBox separate3DEnvelope(
            BoundingBoxType bbox3d) throws OgcException {
        List<Double> lc = bbox3d.getLowerCorner();
        List<Double> uc = bbox3d.getUpperCorner();
        if (lc.size() != 3 || uc.size() != 3) {
            throw new OgcException(Code.InvalidParameterValue,
                    "3D bounding box requires 3 dimensions");
        }
        DirectPosition min = convert(lc);
        DirectPosition max = convert(uc);
        if (!bbox3d.isSetCrs()) {
            throw new OgcException(Code.MissingParameterValue,
                    "Missing SRS name in bounding box");
        }
        return separate3DEnvelope(min, max, bbox3d.getCrs());
    }

    /**
     * Convert list of doubles to a direct position
     * 
     * @param coordinate
     * @return
     */
    public static DirectPosition convert(List<Double> coordinate) {
        GeneralDirectPosition rval = new GeneralDirectPosition(
                coordinate.size());
        Iterator<Double> iter = coordinate.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            rval.setOrdinate(i, iter.next());
        }
        return rval;
    }

    /**
     * Split a 3d bounding box into composite parts
     * 
     * @param min3d
     *            lower corner
     * @param max3d
     *            upper corner
     * @param crsName
     * @return
     * @throws OgcException
     */
    public static Composite3DBoundingBox separate3DEnvelope(
            DirectPosition min3d, DirectPosition max3d, String crsName)
            throws OgcException {
        check3dBounds(min3d, max3d);
        Matcher m = CrsLookup.EXTENDED_3D_CRS_PATTERN.matcher(crsName);
        String horizCrs;
        Unit<?> units;
        Reference ref = Reference.UNKNOWN;
        if (!m.matches()) {
            String norm = CrsLookup.normalize(crsName);
            if (!"epsg:4979".equalsIgnoreCase(norm)) {
                return separate3DEnvelope(min3d, max3d, getCrs(crsName));
            }
            horizCrs = "EPSG:4326";
            units = SI.METER;
            ref = Reference.ABOVE_MSL;
        } else {
            horizCrs = m.group(1);
            units = Unit.valueOf(m.group(2));
            if (m.group(4) != null) {
                ref = Reference.fromAbbreviation(m.group(4).toUpperCase());
            } else {
                // check if unit string matches reference level
                ref = Reference.fromAbbreviation(m.group(2).toUpperCase());
            }
        }
        // TODO this assumes that Z is always the 3rd axis
        DirectPosition2D min2d = new DirectPosition2D(min3d.getOrdinate(0),
                min3d.getOrdinate(1));
        DirectPosition2D max2d = new DirectPosition2D(max3d.getOrdinate(0),
                max3d.getOrdinate(1));
        ReferencedEnvelope horiz = convert2D(min2d, max2d, horizCrs);
        VerticalCoordinate vert = new VerticalCoordinate(min3d.getOrdinate(2),
                max3d.getOrdinate(2), units, ref);
        return new Composite3DBoundingBox(horiz, vert);
    }

    /**
     * Split a 3d bounding box into composite parts
     * 
     * @param min3d
     *            lower corner
     * @param max3d
     *            upper corner
     * @param crs3d
     * @return
     * @throws OgcException
     */
    public static Composite3DBoundingBox separate3DEnvelope(
            DirectPosition min3d, DirectPosition max3d,
            CoordinateReferenceSystem crs3d) throws OgcException {
        if (crs3d.getCoordinateSystem().getDimension() != 3) {
            throw new OgcException(Code.InvalidCRS, "Expected 3D CRS");
        }
        check3dBounds(min3d, max3d);
        SingleCRS crs2d = CRS.getHorizontalCRS(crs3d);
        DirectPosition2D min2d = new DirectPosition2D();
        DirectPosition2D max2d = new DirectPosition2D();
        double maxVert = 0;
        double minVert = 0;
        CoordinateSystem cs3d = crs3d.getCoordinateSystem();
        CoordinateSystem cs2d = crs2d.getCoordinateSystem();
        CoordinateSystemAxis vertAxis = null;
        for (int i = 0; i < cs3d.getDimension(); ++i) {
            CoordinateSystemAxis axis3d = cs3d.getAxis(i);
            int j = 0;
            for (; j < cs2d.getDimension(); ++j) {
                CoordinateSystemAxis axis2d = cs2d.getAxis(j);
                if (CRS.equalsIgnoreMetadata(axis3d, axis2d)) {
                    min2d.setOrdinate(j, min3d.getOrdinate(i));
                    max2d.setOrdinate(j, max3d.getOrdinate(i));
                    break;
                }
            }
            if (j == cs2d.getDimension()) {
                // axis didn't match 2d axis, assume vert axis
                vertAxis = axis3d;
                minVert = min3d.getOrdinate(i);
                maxVert = max3d.getOrdinate(i);
            }
        }
        ReferencedEnvelope ref;
        // test 3d crs since authority info gets stripped
        if (CrsLookup.isEpsgGeoCrs(crs3d)) {
            // EPSG uses lat/lon, we use lon/lat
            ref = new ReferencedEnvelope(min2d.y, max2d.y, min2d.x, max2d.x,
                    crs2d);
        } else {
            ref = new ReferencedEnvelope(min2d.x, max2d.x,
                min2d.y, max2d.y, crs2d);
        }
        checkGeoBounds(ref);
        VerticalCoordinate vert = new VerticalCoordinate(minVert, maxVert,
                vertAxis.getUnit(), getReference(vertAxis));
        return new Composite3DBoundingBox(ref, vert);
    }

    /**
     * Ensure that if the envelope is lat/lon, it is in bounds
     * 
     * @param env
     * @throws OgcException
     *             if out of bounds
     */
    private static void checkGeoBounds(ReferencedEnvelope env)
            throws OgcException {
        // if (env.getCoordinateReferenceSystem() instanceof GeographicCRS) {
        // if (env.getMinX() < -180 || env.getMaxX() > 180
        // || env.getMinY() < -90 || env.getMaxY() > 90) {
        // throw new OgcException(Code.InvalidParameterValue,
        // "Geo bounds not in range. Check axis order");
        // }
        // }
    }

    /**
     * Ensure that the bounds are of the correct dimension and order
     * 
     * @param min2d
     * @param max2d
     * @throws OgcException
     */
    private static void check2dBounds(DirectPosition min2d, DirectPosition max2d)
            throws OgcException {
        checkBounds(min2d, max2d, 2);
    }

    /**
     * Ensure that the bounds are of the correct dimension and order
     * 
     * @param min3d
     * @param max3d
     * @throws OgcException
     */
    private static void check3dBounds(DirectPosition min3d, DirectPosition max3d)
            throws OgcException {
        checkBounds(min3d, max3d, 3);
    }

    /**
     * Ensure that the bounds are of the correct dimension and order
     * 
     * @param min
     * @param max
     * @param dims
     * @throws OgcException
     */
    private static void checkBounds(DirectPosition min, DirectPosition max,
            int dims) throws OgcException {
        if (min.getDimension() != dims || max.getDimension() != dims) {
            String msg = String.format(
                    "%dD bounding box must have %d dimensions", dims, dims);
            throw new OgcException(Code.InvalidParameterValue, msg);
        }
        for (int i = 0; i < dims; ++i) {
            if (min.getOrdinate(i) > max.getOrdinate(i)) {
                throw new OgcException(Code.InvalidParameterValue,
                        "Minimum coordinate cannot be larger than maximum coordinate");
            }
        }
    }

    /**
     * Convert a 2d bounding box with crs to a referenced envelope
     * 
     * @param min2d
     * @param max2d
     * @param crsName
     * @return
     * @throws OgcException
     */
    public static ReferencedEnvelope convert2D(DirectPosition min2d,
            DirectPosition max2d, String crsName) throws OgcException {
        CoordinateReferenceSystem crs = getCrs(crsName);
        if (crs instanceof DefaultCompoundCRS) {
            throw new OgcException(Code.InvalidCRS,
                    "Cannot use 3D CRS with 2D bounds");
        }
        check2dBounds(min2d, max2d);
        double minx;
        double miny;
        double maxx;
        double maxy;
        if (CrsLookup.isEpsgGeoCrs(crs)) {
            // EPSG uses lat/lon, we use lon/lat
            minx = min2d.getOrdinate(1);
            maxx = max2d.getOrdinate(1);
            miny = min2d.getOrdinate(0);
            maxy = max2d.getOrdinate(0);
        } else {
            minx = min2d.getOrdinate(0);
            maxx = max2d.getOrdinate(0);
            miny = min2d.getOrdinate(1);
            maxy = max2d.getOrdinate(1);
        }
        ReferencedEnvelope rval = new ReferencedEnvelope(minx, maxx, miny,
                maxy, crs);
        checkGeoBounds(rval);
        return rval;
    }

    /**
     * Convert 2d jaxb envelope to JTS envelope
     * 
     * @param env
     * @return
     * @throws OgcException
     */
    public static ReferencedEnvelope convert2D(EnvelopeType env)
            throws OgcException {
        if (EnvelopeConverter.getDims(env) != 2) {
            throw new OgcException(Code.InvalidParameterValue,
                    "2D bounding box must have 2 dimensions");
        }
        if (!env.isSetSrsName()) {
            throw new OgcException(Code.MissingParameterValue,
                    "Missing SRS name in bounding box");
        }
        Coordinate[] coords = new EnvelopeConverter().getCoordinates(env);
        DirectPosition2D min = new DirectPosition2D(coords[0].x, coords[0].y);
        DirectPosition2D max = new DirectPosition2D(coords[1].x, coords[1].y);
        return convert2D(min, max, env.getSrsName());
    }

    /**
     * Convert 2d jaxb envelope to JTS envelope
     * 
     * @param env
     * @return
     * @throws OgcException
     */
    public static ReferencedEnvelope convert2D(
            net.opengis.gml.v_3_2_1.EnvelopeType env) throws OgcException {
        if (com.raytheon.uf.edex.ogc.common.gml3_2_1.EnvelopeConverter
                .getDims(env) != 2) {
            throw new OgcException(Code.InvalidParameterValue,
                    "2D bounding box must have 2 dimensions");
        }
        if (!env.isSetSrsName()) {
            throw new OgcException(Code.MissingParameterValue,
                    "Missing SRS name in bounding box");
        }
        Coordinate[] coords = new com.raytheon.uf.edex.ogc.common.gml3_2_1.EnvelopeConverter()
                .getCoordinates(env);
        DirectPosition2D min = new DirectPosition2D(coords[0].x, coords[0].y);
        DirectPosition2D max = new DirectPosition2D(coords[1].x, coords[1].y);
        return convert2D(min, max, env.getSrsName());
    }

    /**
     * Convert 2d jaxb bounding box to JTS envelope
     * 
     * @param bbox2d
     * @return
     * @throws OgcException
     */
    public static ReferencedEnvelope convert2D(BoundingBoxType bbox2d)
            throws OgcException {
        List<Double> lc = bbox2d.getLowerCorner();
        List<Double> uc = bbox2d.getUpperCorner();
        if (lc.size() != 2 || uc.size() != 2) {
            throw new OgcException(Code.InvalidParameterValue,
                    "2D bounding box must have 2 dimensions");
        }
        if (!bbox2d.isSetCrs()) {
            throw new OgcException(Code.MissingParameterValue,
                    "Missing SRS name in bounding box");
        }
        DirectPosition min = convert(lc);
        DirectPosition max = convert(uc);
        return convert2D(min, max, bbox2d.getCrs());
    }

    /**
     * Determine altitude reference of vertical axis
     * 
     * @param verticalAxis
     * @return
     * @throws OgcException
     */
    public static VerticalCoordinate.Reference getReference(
            CoordinateSystemAxis verticalAxis) throws OgcException {
        String code = verticalAxis.getName().getCode().toLowerCase();
        if ( code.contains("ellipsoid")){
            return VerticalCoordinate.Reference.ABOVE_ELLIPSOID;
        } else if (code.contains("gravity") || code.contains("geoid")) {
            return VerticalCoordinate.Reference.ABOVE_MSL;
        } else if (code.contains("barometric")) {
            return VerticalCoordinate.Reference.PRESSURE_LEVEL;
        } else if(code.contains("flight")){
            return VerticalCoordinate.Reference.FLIGHT_LEVEL;
        } else if (code.contains("ground")){
            return VerticalCoordinate.Reference.ABOVE_GROUND;
        }
        throw new OgcException(Code.InvalidCRS, "Unable to parse vertical CRS");
    }


    /**
     * Parse CRS String to object
     * 
     * @param srs
     * @return
     * @throws OgcException
     */
    public static CoordinateReferenceSystem getCrs(String srs)
            throws OgcException {
        try {
            CoordinateReferenceSystem rval = CrsLookup.lookup(srs);
            if (rval == null) {
                throw new Exception();
            }
            return rval;
        } catch (Exception e) {
            throw new OgcException(Code.InvalidParameterValue, "Unknown SRS: "
                    + srs);
        }
    }

}
