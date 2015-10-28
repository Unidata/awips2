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
package com.raytheon.uf.edex.ogc.common.gml3_2_1;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXBElement;

import net.opengis.gml.v_3_2_1.AbstractCurveType;
import net.opengis.gml.v_3_2_1.AbstractGeometricAggregateType;
import net.opengis.gml.v_3_2_1.AbstractGeometricPrimitiveType;
import net.opengis.gml.v_3_2_1.AbstractGeometryType;
import net.opengis.gml.v_3_2_1.AbstractRingPropertyType;
import net.opengis.gml.v_3_2_1.AbstractRingType;
import net.opengis.gml.v_3_2_1.CoordinatesType;
import net.opengis.gml.v_3_2_1.CurveArrayPropertyType;
import net.opengis.gml.v_3_2_1.CurvePropertyType;
import net.opengis.gml.v_3_2_1.DirectPositionListType;
import net.opengis.gml.v_3_2_1.DirectPositionType;
import net.opengis.gml.v_3_2_1.GeometryArrayPropertyType;
import net.opengis.gml.v_3_2_1.GeometryPropertyType;
import net.opengis.gml.v_3_2_1.LineStringType;
import net.opengis.gml.v_3_2_1.LinearRingType;
import net.opengis.gml.v_3_2_1.MultiCurveType;
import net.opengis.gml.v_3_2_1.MultiGeometryType;
import net.opengis.gml.v_3_2_1.MultiPointType;
import net.opengis.gml.v_3_2_1.ObjectFactory;
import net.opengis.gml.v_3_2_1.PointArrayPropertyType;
import net.opengis.gml.v_3_2_1.PointPropertyType;
import net.opengis.gml.v_3_2_1.PointType;
import net.opengis.gml.v_3_2_1.PolygonType;

import org.apache.commons.lang3.ArrayUtils;
import org.geotools.geometry.jts.JTS;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
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
import com.vividsolutions.jts.io.ParseException;

/**
 * Converts between GML 3.2.1 and JTS geometries
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 25, 2011             bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GeometryConverter {

    protected final GeometryFactory factory = new GeometryFactory();

    protected final ObjectFactory gmlFactory = new ObjectFactory();

    private static final String DIGIT_STR = "Ee-.0123456789";

    private static final Set<Character> DIGIT_SET;

    static {
        char[] arr = DIGIT_STR.toCharArray();
        HashSet<Character> set = new HashSet<Character>(
                Arrays.asList(ArrayUtils.toObject(arr)));
        DIGIT_SET = Collections.unmodifiableSet(set);
    }

    /**
     * Supports geometry collection, polygon, point and linestring
     * 
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public AbstractGeometryType convert(Geometry value)
            throws IllegalArgumentException {
        AbstractGeometryType rval;
        if (value instanceof GeometryCollection) {
            rval = convert((GeometryCollection) value);
        } else if (value instanceof Polygon) {
            rval = convert((Polygon) value);
        } else if (value instanceof Point) {
            rval = convert((Point) value);
        } else if (value instanceof LineString) {
            rval = convert((LineString) value);
        } else {
            throw new IllegalArgumentException("Unsupported geometry type: "
                    + value.getClass());
        }
        return rval;
    }

    /**
     * Only supports multipolygon, multipoint and multilinestring
     * 
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public AbstractGeometricAggregateType convert(GeometryCollection value)
            throws IllegalArgumentException {
        AbstractGeometricAggregateType rval;
        if (value instanceof MultiPolygon) {
            rval = convert((MultiPolygon) value);
        } else if (value instanceof MultiPoint) {
            rval = convert((MultiPoint) value);
        } else if (value instanceof MultiLineString) {
            rval = convert((MultiLineString) value);
        } else {
            throw new IllegalArgumentException("Unsupported geometry type: "
                    + value.getClass());
        }
        return rval;
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public MultiGeometryType convert(MultiPolygon value)
            throws IllegalArgumentException {
        MultiGeometryType rval = new MultiGeometryType();
        int count = value.getNumGeometries();
        List<GeometryPropertyType> members = new ArrayList<GeometryPropertyType>(
                count);
        for (int i = 0; i < count; ++i) {
            GeometryPropertyType prop = new GeometryPropertyType();
            Polygon p = (Polygon) value.getGeometryN(i);
            PolygonType ptype = convert(p);
            prop.setAbstractGeometry(gmlFactory.createAbstractGeometry(ptype));
            members.add(prop);
        }
        rval.setGeometryMember(members);
        return rval;
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public MultiPointType convert(MultiPoint value)
            throws IllegalArgumentException {
        MultiPointType rval = new MultiPointType();
        int count = value.getNumGeometries();
        List<PointPropertyType> members = new ArrayList<PointPropertyType>(
                count);
        for (int i = 0; i < count; ++i) {
            PointPropertyType prop = new PointPropertyType();
            Point p = (Point) value.getGeometryN(i);
            PointType ptype = convert(p);
            prop.setPoint(ptype);
            members.add(prop);
        }
        rval.setPointMember(members);
        return rval;
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public MultiCurveType convert(MultiLineString value)
            throws IllegalArgumentException {
        MultiCurveType rval = new MultiCurveType();
        int count = value.getNumGeometries();
        List<CurvePropertyType> members = new ArrayList<CurvePropertyType>(
                count);
        for (int i = 0; i < count; ++i) {
            CurvePropertyType prop = new CurvePropertyType();
            LineString ls = (LineString) value.getGeometryN(i);
            LineStringType lstype = convert(ls);
            prop.setAbstractCurve(gmlFactory.createAbstractCurve(lstype));
        }
        rval.setCurveMember(members);
        return rval;
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public PolygonType convert(Polygon value) throws IllegalArgumentException {
        PolygonType rval = new PolygonType();
        AbstractRingPropertyType ext = new AbstractRingPropertyType();
        LinearRingType ring = convertStringRing(value.getExteriorRing());
        ext.setAbstractRing(gmlFactory.createAbstractRing(ring));
        rval.setExterior(ext);
        int holeCount = value.getNumInteriorRing();
        List<AbstractRingPropertyType> holes = new ArrayList<AbstractRingPropertyType>(
                holeCount);
        for (int i = 0; i < holeCount; ++i) {
            AbstractRingPropertyType prop = new AbstractRingPropertyType();
            LineString hole = value.getInteriorRingN(i);
            ring = convertStringRing(hole);
            prop.setAbstractRing(gmlFactory.createAbstractRing(ring));
        }
        rval.setInterior(holes);
        return rval;
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    protected LinearRingType convertStringRing(LineString value)
            throws IllegalArgumentException {
        int dims = value.getDimension();
        Coordinate[] coords = value.getCoordinates();
        return ringFromCoords(dims, coords);
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    protected LinearRingType convert(LinearRing value)
            throws IllegalArgumentException {
        int dims = value.getDimension();
        Coordinate[] coords = value.getCoordinates();
        return ringFromCoords(dims, coords);
    }

    /**
     * @param dims
     *            number of dimensions in each coordinate
     * @param coords
     * @return
     * @throws IllegalArgumentException
     */
    protected LinearRingType ringFromCoords(int dims, Coordinate[] coords)
            throws IllegalArgumentException {
        LinearRingType rval = new LinearRingType();
        DirectPositionListType posList = posListFromCoords(dims, coords);
        rval.setPosList(posList);
        return rval;
    }

    /**
     * @param dims
     *            number of dimensions in each coordinate
     * @param coords
     * @return
     * @throws IllegalArgumentException
     */
    protected DirectPositionListType posListFromCoords(int dims,
            Coordinate[] coords) throws IllegalArgumentException {
        DirectPositionListType posList = new DirectPositionListType();
        if (dims < 2 && coords.length > 0) {
            // check for uninitialized dims
            Coordinate sample = coords[0];
            // 3 if sample.z is not NaN
            // 2 if sample.z is NaN and sample.y is not NaN
            dims = Double.isNaN(sample.z) ? 2 : 3;
        }
        posList.setCount(BigInteger.valueOf(coords.length));
        posList.setSrsDimension(BigInteger.valueOf(dims));
        List<Double> dubs;
        if (dims == 2) {
            dubs = convert2D(coords);
        } else if (dims == 3) {
            dubs = convert3D(coords);
        } else {
            throw new IllegalArgumentException(
                    "Unsupported number of dimensions: " + dims);
        }
        posList.setValue(dubs);
        return posList;
    }

    /**
     * @param coords
     *            2D coordinates
     * @return
     */
    protected List<Double> convert2D(Coordinate... coords) {
        List<Double> rval = new ArrayList<Double>(coords.length * 2);
        for (Coordinate c : coords) {
            rval.add(c.x);
            rval.add(c.y);
        }
        return rval;
    }

    /**
     * @param coords
     *            3D coodinates
     * @return
     */
    protected List<Double> convert3D(Coordinate... coords) {
        List<Double> rval = new ArrayList<Double>(coords.length * 3);
        for (Coordinate c : coords) {
            rval.add(c.x);
            rval.add(c.y);
            rval.add(c.z);
        }
        return rval;
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public PointType convert(Point value) throws IllegalArgumentException {
        PointType rval = new PointType();
        int dims = value.getDimension();
        DirectPositionType pos = posFromCoord(dims, value.getCoordinate());
        rval.setPos(pos);
        return rval;
    }

    /**
     * @param dims
     *            number of dimensions in coordinate
     * @param c
     * @return
     * @throws IllegalArgumentException
     */
    protected DirectPositionType posFromCoord(int dims, Coordinate c)
            throws IllegalArgumentException {
        DirectPositionType rval = new DirectPositionType();
        if (dims == 0) {
            // check for uninitialized dims
            dims = Double.isNaN(c.z) ? 2 : 3;
        }
        if (dims == 2) {
            rval.setValue(convert2D(c));
        } else if (dims == 3) {
            rval.setValue(convert3D(c));
        } else {
            throw new IllegalArgumentException(
                    "Unsupported number of dimensions: " + dims);
        }
        return rval;
    }

    /**
     * @param value
     * @return
     * @throws IllegalArgumentException
     */
    public LineStringType convert(LineString value)
            throws IllegalArgumentException {
        LineStringType rval = new LineStringType();
        int dims = value.getDimension();
        Coordinate[] coords = value.getCoordinates();
        DirectPositionListType posList = posListFromCoords(dims, coords);
        rval.setPosList(posList);
        return rval;
    }

    /**
     * @param env
     * @return
     * @throws ParseException
     */
    public Geometry convert(Envelope env) throws ParseException {
        return JTS.toGeometry(env);
    }

    /**
     * @param value
     * @return
     * @throws Exception
     */
    public Geometry convert(AbstractGeometryType value) throws Exception {
        Geometry rval;
        if (value instanceof AbstractGeometricAggregateType) {
            rval = convert((AbstractGeometricAggregateType) value);
        } else if (value instanceof AbstractGeometricPrimitiveType) {
            rval = convert((AbstractGeometricPrimitiveType) value);
        } else {
            throw new Exception("Unsupported Geometry type: "
                    + value.getClass());
        }
        return rval;
    }

    /**
     * Supports multipoint, multilinestring and multipolygon
     * 
     * @param agg
     * @return
     * @throws Exception
     */
    public GeometryCollection convert(AbstractGeometricAggregateType agg)
            throws Exception {
        GeometryCollection rval;
        if (agg instanceof MultiGeometryType) {
            rval = convert((MultiGeometryType) agg);
        } else if (agg instanceof MultiPointType) {
            rval = convert((MultiPointType) agg);
        } else if (agg instanceof MultiCurveType) {
            rval = convert((MultiCurveType) agg);
        } else {
            throw new Exception("Unsupported geometry aggregate type: "
                    + agg.getClass());
        }
        return rval;
    }

    /**
     * Only supports multilinestring
     * 
     * @param multiCurve
     * @return
     * @throws Exception
     */
    public MultiLineString convert(MultiCurveType multiCurve) throws Exception {
        List<AbstractCurveType> curves = new ArrayList<AbstractCurveType>();
        if (multiCurve.isSetCurveMember()) {
            List<CurvePropertyType> members = multiCurve.getCurveMember();
            for (CurvePropertyType cpt : members) {
                curves.add(cpt.getAbstractCurve().getValue());
            }
        }
        if (multiCurve.isSetCurveMembers()) {
            CurveArrayPropertyType members = multiCurve.getCurveMembers();
            List<JAXBElement<? extends AbstractCurveType>> elems = members
                    .getAbstractCurve();
            for (JAXBElement<? extends AbstractCurveType> e : elems) {
                curves.add(e.getValue());
            }
        }
        LineString[] lineStrings = new LineString[curves.size()];
        Iterator<AbstractCurveType> iter = curves.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            AbstractCurveType curve = iter.next();
            if (!(curve instanceof LineStringType)) {
                throw new Exception("Unsupported multicurve member type: "
                        + curve.getClass());
            }
            lineStrings[i] = convert((LineStringType) curve);
        }
        return factory.createMultiLineString(lineStrings);
    }

    /**
     * 
     * @param multiPoint
     * @return
     * @throws Exception
     */
    public MultiPoint convert(MultiPointType multiPoint) throws Exception {
        List<PointType> points = new ArrayList<PointType>();
        if (multiPoint.isSetPointMember()) {
            List<PointPropertyType> members = multiPoint.getPointMember();
            for (PointPropertyType ppt : members) {
                points.add(ppt.getPoint());
            }
        }
        if (multiPoint.isSetPointMembers()) {
            PointArrayPropertyType members = multiPoint.getPointMembers();
            points.addAll(members.getPoint());
        }
        Point[] rval = new Point[points.size()];
        Iterator<PointType> iter = points.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            PointType ptype = iter.next();
            rval[i] = convert(ptype);
        }
        return factory.createMultiPoint(rval);
    }

    /**
     * Only supports multipolygon
     * 
     * @param multiGeom
     * @return
     * @throws Exception
     */
    public MultiPolygon convert(MultiGeometryType multiGeom) throws Exception {
        List<AbstractGeometryType> geoms = new ArrayList<AbstractGeometryType>();
        if (multiGeom.isSetGeometryMember()) {
            List<GeometryPropertyType> members = multiGeom.getGeometryMember();
            for (GeometryPropertyType gpt : members) {
                geoms.add(gpt.getAbstractGeometry().getValue());
            }
        }
        if (multiGeom.isSetGeometryMembers()) {
            GeometryArrayPropertyType members = multiGeom.getGeometryMembers();
            List<JAXBElement<? extends AbstractGeometryType>> elems = members
                    .getAbstractGeometry();
            for (JAXBElement<? extends AbstractGeometryType> e : elems) {
                geoms.add(e.getValue());
            }
        }
        Polygon[] polys = new Polygon[geoms.size()];
        Iterator<AbstractGeometryType> iter = geoms.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            AbstractGeometryType geom = iter.next();
            if (geom instanceof AbstractGeometricAggregateType) {
                throw new Exception("Nested geometry aggregates not supported");
            }
            if (!(geom instanceof PolygonType)) {
                throw new Exception("Unsupported multi geometry type: "
                        + geom.getClass());
            }
            PolygonType ptype = (PolygonType) geom;
            polys[i] = convert(ptype);
        }
        return factory.createMultiPolygon(polys);
    }

    /**
     * Supports point, linestring and polygon
     * 
     * @param primitive
     * @return
     * @throws Exception
     */
    public Geometry convert(AbstractGeometricPrimitiveType primitive)
            throws Exception {
        Geometry rval;
        if (primitive instanceof LineStringType) {
            rval = convert((LineStringType) primitive);
        } else if (primitive instanceof PolygonType) {
            rval = convert((PolygonType) primitive);
        } else if (primitive instanceof PointType) {
            rval = convert((PointType) primitive);
        } else {
            throw new Exception("Unsupported Geometry type: "
                    + primitive.getClass());
        }
        return rval;
    }

    /**
     * @param point
     * @return
     * @throws Exception
     */
    public Point convert(PointType point) throws Exception {
        Coordinate coord;
        if (point.isSetCoordinates()) {
            CoordinatesType ct = point.getCoordinates();
            coord = convert(ct)[0];
        } else if (point.isSetPos()) {
            DirectPositionType pos = point.getPos();
            coord = convert(pos);
        } else {
            throw new Exception("Unable to find coordinate for point: " + point);
        }
        return factory.createPoint(coord);
    }

    /**
     * @param poly
     * @return
     * @throws Exception
     */
    public Polygon convert(PolygonType poly) throws Exception {
        AbstractRingPropertyType exterior = poly.getExterior();
        LinearRing shell = convert(exterior);
        List<AbstractRingPropertyType> interior = poly.getInterior();
        LinearRing[] holes = new LinearRing[interior.size()];
        Iterator<AbstractRingPropertyType> iter = interior.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            holes[i] = convert(iter.next());
        }
        return factory.createPolygon(shell, holes);
    }

    /**
     * @param ring
     * @return
     * @throws Exception
     */
    public LinearRing convert(AbstractRingPropertyType ring) throws Exception {
        JAXBElement<? extends AbstractRingType> abs = ring.getAbstractRing();
        AbstractRingType value = abs.getValue();
        if (value instanceof LinearRingType) {
            return convert((LinearRingType) value);
        } else {
            throw new Exception("Unsupported ring type: " + ring.getClass());
        }
    }

    /**
     * @param ring
     * @return
     * @throws Exception
     */
    public LinearRing convert(LinearRingType ring) throws Exception {
        Coordinate[] coords;
        if (ring.isSetCoordinates()) {
            CoordinatesType ctype = ring.getCoordinates();
            coords = convert(ctype);
        } else if (ring.isSetPosList()) {
            DirectPositionListType posList = ring.getPosList();
            coords = convert(posList);
        } else if (ring.isSetPosOrPointPropertyOrPointRep()) {
            List<JAXBElement<?>> innerChoice = ring
                    .getPosOrPointPropertyOrPointRep();
            coords = convertPosOrPointChoice(innerChoice);
        } else {
            throw new Exception("Unable to find coordinates for ring: " + ring);
        }
        return factory.createLinearRing(coords);
    }

    /**
     * @param line
     * @return
     * @throws Exception
     */
    public LineString convert(LineStringType line) throws Exception {
        Coordinate[] coords;
        if (line.isSetCoordinates()) {
            CoordinatesType ctype = line.getCoordinates();
            coords = convert(ctype);
        } else if (line.isSetPosList()) {
            DirectPositionListType posList = line.getPosList();
            coords = convert(posList);
        } else if (line.isSetPosOrPointPropertyOrPointRep()) {
            List<JAXBElement<?>> innerChoice = line
                    .getPosOrPointPropertyOrPointRep();
            coords = convertPosOrPointChoice(innerChoice);
        } else {
            throw new Exception("Unable to find coordinates for line: " + line);
        }
        return factory.createLineString(coords);
    }

    /**
     * Converts list of Pos (DirectPositionType), PointPropertyType or PointType
     * elements to list of coordinates
     * 
     * @param list
     * @return
     * @throws Exception
     */
    protected Coordinate[] convertPosOrPointChoice(List<JAXBElement<?>> list)
            throws Exception {
        Coordinate[] rval = new Coordinate[list.size()];
        Iterator<JAXBElement<?>> iter = list.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            JAXBElement<?> elem = iter.next();
            Object obj = elem.getValue();
            if (obj instanceof DirectPositionType) {
                DirectPositionType dpt = (DirectPositionType) obj;
                rval[i] = convert(dpt);
            } else if (obj instanceof PointPropertyType) {
                PointPropertyType ppt = (PointPropertyType) obj;
                rval[i] = convert(ppt);
            } else if (obj instanceof PointType) {
                rval[i] = convertToCoord((PointType) obj);
            }
        }
        return rval;
    }

    /**
     * @param ppt
     * @return
     * @throws Exception
     */
    protected Coordinate convert(PointPropertyType ppt) throws Exception {
        PointType point = ppt.getPoint();
        return convertToCoord(point);
    }

    /**
     * @param point
     * @return
     * @throws Exception
     */
    protected Coordinate convertToCoord(PointType point) throws Exception {
        Coordinate rval;
        if (point.isSetCoordinates()) {
            CoordinatesType coordinates = point.getCoordinates();
            rval = convert(coordinates)[0];
        } else if (point.isSetPos()) {
            DirectPositionType pos = point.getPos();
            rval = convert(pos);
        } else {
            throw new Exception("Unable to find coordinates for point: "
                    + point);
        }
        return rval;
    }

    /**
     * @param pos
     * @return
     */
    protected Coordinate convert(DirectPositionType pos) {
        Coordinate rval;
        int dims = 2;
        if (pos.isSetSrsDimension()) {
            dims = pos.getSrsDimension().intValue();
        }
        List<Double> value = pos.getValue();
        double x = value.get(0);
        double y = value.get(1);
        if (dims == 2) {
            rval = new Coordinate(x, y);
        } else {
            rval = new Coordinate(x, y, value.get(3));
        }
        return rval;
    }

    /**
     * @param posList
     * @return
     * @throws Exception
     */
    protected Coordinate[] convert(DirectPositionListType posList)
            throws Exception {
        int len;
        int dims;
        List<Double> doubles = posList.getValue();
        if (posList.isSetCount()) {
            len = posList.getCount().intValue();
            dims = posList.getSrsDimension().intValue();
        } else {
            // default 2D TODO should use outer geom SRS dim
            dims = 2;
            len = doubles.size() / dims;
        }
        if (dims < 2 || dims > 3) {
            throw new Exception("Unsupported number of dimensions: " + dims);
        }
        if (dims * len != doubles.size()) {
            throw new Exception("Invalid pos list");
        }
        Coordinate[] rval = new Coordinate[len];
        Iterator<Double> iter = doubles.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            double x = iter.next();
            double y = iter.next();
            if (dims == 2) {
                rval[i] = new Coordinate(x, y);
            } else {
                rval[i] = new Coordinate(x, y, iter.next());
            }
        }
        return rval;
    }

    /**
     * Ensure that a string only contains one character
     * 
     * @param str
     *            string containing character
     * @param isSet
     *            if false, returns fallback
     * @param fallback
     *            default character for use if string is not set
     * @return
     * @throws Exception
     *             if string isn't 1 character long and is set
     */
    protected Character ensureChar(String str, boolean isSet, Character fallback)
            throws Exception {
        if (!isSet) {
            return fallback;
        }
        if (str.length() != 1) {
            throw new Exception("Invalid character string length: " + str);
        }
        return str.charAt(0);
    }

    /**
     * states for coordinate conversion state machine
     */
    protected static enum CoordState {
        START, NUM, BETWEEN, OUTER, INNER
    }

    /**
     * @param ctype
     * @return
     * @throws Exception
     */
    protected Coordinate[] convert(CoordinatesType ctype) throws Exception {
        String value = ctype.getValue();
        Character innerSeparator = ensureChar(ctype.getCs(), ctype.isSetCs(),
                ',');
        Character outerSeparator = ensureChar(ctype.getTs(), ctype.isSetTs(),
                ' ');
        String decimal = ".";
        if (ctype.isSetDecimal()) {
            decimal = ctype.getDecimal();
            if (!decimal.equals(".")) {
                value = value.replace(decimal, ".");
            }
        }

        int len = value.length();
        StringBuilder sb = new StringBuilder();
        CoordState state = CoordState.START;
        ArrayList<Double> nums = new ArrayList<Double>(3);
        List<Coordinate> coords = new ArrayList<Coordinate>();
        for (int i = 0; i < len; ++i) {
            char c = value.charAt(i);
            switch (state) {
            case START:
                if (isNumber(c)) {
                    state = CoordState.NUM;
                    sb.append(c);
                }
                break;
            case NUM:
                if (isNumber(c)) {
                    sb.append(c);
                } else {
                    if (innerSeparator.equals(c)) {
                        state = CoordState.INNER;
                    } else if (outerSeparator.equals(c)) {
                        state = CoordState.OUTER;
                    } else {
                        state = CoordState.BETWEEN;
                    }
                    nums.add(Double.parseDouble(sb.toString()));
                    sb = new StringBuilder();
                }
                break;
            case BETWEEN:
                if (isNumber(c)) {
                    throw new Exception("Invalid coordinates string: " + value);
                } else if (innerSeparator.equals(c)) {
                    state = CoordState.INNER;
                } else if (outerSeparator.equals(c)) {
                    state = CoordState.OUTER;
                }
                break;
            case OUTER:
                if (isNumber(c)) {
                    sb.append(c);
                    state = CoordState.NUM;
                    coords.add(convert(nums));
                } else if (innerSeparator.equals(c)) {
                    state = CoordState.INNER;
                }
                break;
            case INNER:
                if (isNumber(c)) {
                    sb.append(c);
                    state = CoordState.NUM;
                }
                break;
            }
        }
        if (state.equals(CoordState.NUM)) {
            nums.add(Double.parseDouble(sb.toString()));
        }
        coords.add(convert(nums));
        return coords.toArray(new Coordinate[coords.size()]);
    }

    /**
     * @param c
     * @return true if c is a valid character used in representing a number
     */
    protected boolean isNumber(Character c) {
        return DIGIT_SET.contains(c);
    }

    /**
     * @param nums
     * @return
     * @throws Exception
     */
    protected Coordinate convert(List<Double> nums) throws Exception {
        Coordinate rval;
        if (nums.size() == 2) {
            rval = new Coordinate(nums.get(0), nums.get(1));
        } else if (nums.size() == 3) {
            rval = new Coordinate(nums.get(0), nums.get(1), nums.get(2));
        } else {
            throw new Exception("Unsupported dimension length: " + nums.size());
        }
        nums.clear();
        return rval;
    }
}
