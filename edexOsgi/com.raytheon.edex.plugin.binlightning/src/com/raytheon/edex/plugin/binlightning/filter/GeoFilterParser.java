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
package com.raytheon.edex.plugin.binlightning.filter;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * Parses geographic filter configuration files. <br>
 * <br>
 * Example file:
 * 
 * <pre>
 * {@code
 * <!-- example binlightning geospatial filter file
 *     name of file corresponds to data source (eg NLDN.xml)
 *     data is persisted if lightning strike point is in any of the filter areas
 *     if a data source does not have any filters, all data is persisted -->
 * <filters>
 *     <!-- bounding box filter: x is degrees longitude, y is degrees latitude-->
 *     <bbox minx="-98.23" maxx="-97.24" miny="38.27" maxy="39.39" />
 *     <!-- CWA filters: correspond to polygons defining the county warning areas -->
 *     <cwa>OAX</cwa>
 *     <cwa>DMX</cwa>
 *     <cwa>FSD</cwa>
 *     <!-- well known text geometries: x coordinates are longitude, y coordinates are latitude
 *         first coordinate is always the same as last to close the polygon region -->
 *     <wkt>POLYGON ((-101.24 41.4, -100.6 41.15, -101.01 40.75, -101.24 41.4))</wkt>
 * </filters>
 * }
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2014 3226       bclement    Initial creation
 * Jul 08, 2016 5744       mapeters    Added example file to javadoc
 * 
 * </pre>
 * 
 * @author bclement
 */
public class GeoFilterParser {

    private static final double LAT_MIN = -90;

    private static final double LAT_MAX = 90;

    private static final double LON_MIN = -180;

    private static final double LON_MAX = 180;

    /* CWA database constants */

    private static final String CWA_TABLE = "cwa";

    private static final String CWA_COLUMN = "cwa";

    /* helper factories */

    private static JAXBManager jaxbManager;

    private static volatile boolean initialized = false;

    private static final GeometryFactory GEOM_FACTORY = new GeometryFactory();

    /**
     * initialize jaxb manager
     * 
     * @throws JAXBException
     */
    private static synchronized void initialize() throws JAXBException {
        if (!initialized) {
            jaxbManager = new JAXBManager(GeoFilters.class);
            initialized = true;
        }
    }

    /**
     * Parse filter config file from input stream. Does not close input stream.
     * Any non-fatal parsing errors are returned in result.
     * 
     * @param in
     * @return
     * @throws JAXBException
     * @throws SerializationException
     */
    public static GeoFilterResult parse(InputStream in) throws JAXBException,
            SerializationException {
        if (!initialized) {
            initialize();
        }
        Object obj = jaxbManager.unmarshalFromInputStream(in);
        if (obj instanceof GeoFilters) {
            return parseInternal((GeoFilters) obj);
        } else {
            throw new SerializationException("Unexpected XML object type: "
                    + obj.getClass());
        }
    }

    /**
     * @see #parse
     * @param filters
     *            jaxb pojo
     * @return
     * @throws XMLStreamException
     */
    private static GeoFilterResult parseInternal(GeoFilters filters) {
        List<Geometry> geoms = new ArrayList<>();
        List<GeoFilterException> errors = new ArrayList<>();
        List<GeoFilterBbox> bboxes = filters.getBboxes();
        if (bboxes != null && !bboxes.isEmpty()) {
            for (GeoFilterBbox bbox : bboxes) {
                try {
                    geoms.addAll(convertBbox(bbox));
                } catch (GeoFilterException e) {
                    errors.add(e);
                }
            }
        }
        List<String> cwas = filters.getCwas();
        if (cwas != null && !cwas.isEmpty()) {
            for (String cwa : cwas) {
                try {
                    geoms.addAll(getCWA(cwa));
                } catch (GeoFilterException e) {
                    errors.add(e);
                }
            }
        }
        List<Geometry> geometries = filters.getGeometries();
        if (geometries != null) {
            geoms.addAll(geometries);
        }
        List<PreparedGeometry> rval = new ArrayList<>(geoms.size());
        for (Geometry geom : geoms) {
            rval.add(PreparedGeometryFactory.prepare(geom));
        }
        return new GeoFilterResult(rval, errors);
    }

    /**
     * Get county warning area geometries by name. Performs a database lookup.
     * 
     * @param cwa
     * @return
     * @throws GeoFilterException
     */
    private static Collection<Geometry> getCWA(String cwa)
            throws GeoFilterException {
        Map<String, RequestConstraint> map = new HashMap<>();
        map.put(CWA_COLUMN, new RequestConstraint(cwa));
        SpatialQueryResult[] results;
        try {
            results = SpatialQueryFactory.create().query(CWA_TABLE, null, null,
                    map, null);
        } catch (SpatialException e) {
            throw new GeoFilterException("Unable to query database for CWA: "
                    + cwa, e);
        }
        if (results == null || results.length == 0) {
            return Collections.emptyList();
        }
        List<Geometry> rval = new ArrayList<>(results.length);
        for (SpatialQueryResult result : results) {
            rval.add(result.geometry);
        }
        return rval;
    }

    /**
     * Validate bounding box coordinates in attributes. Converts bounding box to
     * polygon.
     * 
     * @param bbox
     * @return
     */
    private static Collection<Geometry> convertBbox(GeoFilterBbox bbox)
            throws GeoFilterException {

        Coordinate upper = new Coordinate(bbox.getMaxx(), bbox.getMaxy());
        Coordinate lower = new Coordinate(bbox.getMinx(), bbox.getMiny());

        if (lower.x > upper.x || lower.y > upper.y) {
            throw new GeoFilterException(
                    "Invalid bounding box tag. Minimum coordinate values "
                            + "cannot be larger than maximum coordinate values: "
                            + lower + " " + upper);
        }

        if (lower.y < LAT_MIN || upper.y > LAT_MAX) {
            throw new GeoFilterException(
                    "Invalid bounding box tag. Box cannot cross poles: "
                            + lower.y + "," + upper.y);
        }

        List<Geometry> rval = null;
        lower.x = normalizeLon(lower.x);
        upper.x = normalizeLon(upper.x);
        /*
         * if normalization switched order, it means it crossed the antimeridian
         * and needs to be split into two boxes
         */
        if (lower.x > upper.x) {
            /* create a new box on the left of the map */
            Coordinate leftLower = new Coordinate(LON_MIN, lower.y);
            Coordinate leftUpper = new Coordinate(upper.x, upper.y);
            /* change the old box to only cover the right of the map */
            upper.x = LON_MAX;
            Geometry left = createPolygon(leftLower, leftUpper);
            Geometry right = createPolygon(lower, upper);
            rval = Arrays.asList(left, right);
        } else {
            rval = Arrays.asList(createPolygon(lower, upper));
        }
        return rval;
    }

    /**
     * @param lon
     * @return longitude normalized to be between -180 and 180
     */
    private static double normalizeLon(double lon) {
        /* 360 degrees */
        final double LON_TOTAL = LON_MAX * 2;
        /* account for any far out degrees */
        double rval = lon % LON_TOTAL;
        /* subtract or add to get in range */
        if (rval > LON_MAX) {
            rval -= LON_TOTAL;
        } else if (rval < LON_MIN) {
            rval += LON_TOTAL;
        }
        return rval;
    }

    /**
     * @param mins
     * @param maxes
     * @return polygon defining a bounding box using corner points
     */
    private static Geometry createPolygon(Coordinate mins, Coordinate maxes) {
        Coordinate[] coordinates = new Coordinate[5];
        coordinates[0] = new Coordinate(mins.x, mins.y);
        coordinates[1] = new Coordinate(mins.x, maxes.y);
        coordinates[2] = new Coordinate(maxes.x, maxes.y);
        coordinates[3] = new Coordinate(maxes.x, mins.y);
        /* polygons need a closed ring */
        coordinates[4] = coordinates[0];
        return GEOM_FACTORY.createPolygon(coordinates);
    }

}
