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

package com.raytheon.edex.uengine.chain;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Adapter class for converting meta-data attributes into geometries.
 * 
 * <pre>
 * 
 *   SOFTWARE HISTORY
 *  
 *   Date           Ticket#     Engineer    Description
 *   ------------   ----------  ----------- --------------------------
 *   24Aug2006                  C Hammack   Initial Creation
 *   23Feb2007      TO5         MW Fegan    Modified to support new meta-data storage.
 * 
 * </pre>
 */
public class SpatialChaindataAdapter {

    private static final double FILL_VALUE = -999.99;

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_PROJECTION = "ProjType";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_FIRST_LAT = "Lat1";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_FIRST_LON = "Lon1";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_Y_POINTS = "Ny";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_X_POINTS = "Nx";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_X_INCREMENT = "Dx";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_Y_INCREMENT = "Dy";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_TANGENT_LAT1 = "Latin1";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_TANGENT_LAT2 = "Latin2";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_LAST_LAT = "Lat2";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAP_LAST_LON = "Lon2";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String LAT_OF_ORIGIN = "Lad";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MAJOR_AXIS = "MajorAxis";

    /**
     * Geolocation Attribute for use by WorldFile, etc, <code>String</code>.
     */
    public static final String MINOR_AXIS = "MinorAxis";

    /**
     * Lov value
     */
    public static final String BASE_LONGITUDE = "Lov";

    /**
     * Scan mode
     */
    public static final String SCAN_MODE = "ScanMode";

    /**
     * Radar latitude
     */
    public static final String RADAR_RADIAL_LAT = "radial_latitude";

    /**
     * Radar longitude
     */
    public static final String RADAR_RADIAL_LON = "radial_longitude";

    /**
     * Station ICAO
     */
    public static final String ICAO = "icao";

    public static final String SPATIAL_INFO = "spatial_info";

    public static final String GRIB_CRS_WKT = "crswkt";

    public static final String GRIB_COVERAGE_WKT = "coveragewkt";

    // private RadarStationDao radarStationDao;

    /**
     * A list of all spatial attributes
     */
    private static String[] SPATIAL_ATTRIBUTES = { MAP_PROJECTION,
            MAP_FIRST_LAT, MAP_FIRST_LON, MAP_Y_POINTS, MAP_X_POINTS,
            MAP_X_INCREMENT, MAP_Y_INCREMENT, MAP_TANGENT_LAT1,
            MAP_TANGENT_LAT2, MAP_LAST_LAT, MAP_LAST_LON, BASE_LONGITUDE,
            LAT_OF_ORIGIN, MAJOR_AXIS, MINOR_AXIS, SCAN_MODE, RADAR_RADIAL_LAT,
            RADAR_RADIAL_LON, ICAO, GRIB_CRS_WKT, GRIB_COVERAGE_WKT,
            SPATIAL_INFO };

    public static enum PROJECTION {
        LAMBERT_CONFORMAL, MERCATOR, POLAR_STEREOGRAPHIC, LAT_LON
    };

    public static enum POINT_LOCATION {
        UPPER_LEFT, UPPER_RIGHT, LOWER_LEFT, LOWER_RIGHT, ENVELOPE
    };

    private PROJECTION projection;

    private double firstLat = FILL_VALUE;

    private double firstLon = FILL_VALUE;

    private double lastLat = FILL_VALUE;

    private double lastLon = FILL_VALUE;

    private double baseLon = FILL_VALUE;

    private double tangentLat1 = FILL_VALUE;

    private double tangentLat2 = FILL_VALUE;

    private double xIncrement = FILL_VALUE;

    private double yIncrement = FILL_VALUE;

    // private double latOfOrigin = FILL_VALUE;

    private double majorAxis = FILL_VALUE;

    private double minorAxis = FILL_VALUE;

    // private double radialLat = FILL_VALUE;

    // private double radialLon = FILL_VALUE;

    private String icao = ICAO;

    private POINT_LOCATION pointLocation = POINT_LOCATION.ENVELOPE;

    private int xPoints = 0;

    private int yPoints = 0;

    private String crswkt = null;

    private String coveragewkt = null;

    private CoordinateReferenceSystem crs;

    /**
     * Determine whether an attribute is a spatial attribute
     * 
     * @param attribute
     *            the attribute to check
     * @return true if an attribute is spatial
     */
    public static boolean isSpatialAttribute(String attribute) {
        for (int i = 0; i < SPATIAL_ATTRIBUTES.length; i++) {
            if (SPATIAL_ATTRIBUTES[i].equalsIgnoreCase(attribute)) {
                return true;
            }
        }
        return false;
    }

    public SpatialChaindataAdapter() {

    }

    /**
     * Convenience method. The value of the spatial attribute is passed in as a
     * String. The WKT attributes are assigned to the appropriate fields. Other
     * attributes values are converted to Double and passed to {@line
     * #addSpatialAttribute(String, Number) addSpatialAttribute(String,Number)}.
     * 
     * @param attribute
     *            the name of the attribute to set
     * @param value
     *            the value for the attribute
     */
    public void addSpatialAttribute(String attribute, String value) {
        if (attribute.equalsIgnoreCase(GRIB_CRS_WKT)) {
            crswkt = value;
        } else if (attribute.equalsIgnoreCase(GRIB_COVERAGE_WKT)) {
            coveragewkt = value;
            setCoverageWkt();
        } else if (attribute.equalsIgnoreCase(ICAO)) {
            icao = value;
        } else {
            Double d = Double.parseDouble(value);
            addSpatialAttribute(attribute, d);
        }
    }

    public void addSpatialAttribute(String attribute, Number value) {

        if (attribute.equalsIgnoreCase(MAP_FIRST_LAT)) {
            firstLat = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MAP_FIRST_LON)) {
            firstLon = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MAP_LAST_LAT)) {
            if (value.doubleValue() != 0.0) {
                lastLat = value.doubleValue();
            }
        } else if (attribute.equalsIgnoreCase(MAP_LAST_LON)) {
            if (value.doubleValue() != 0.0) {
                lastLon = value.doubleValue();
            }
        } else if (attribute.equalsIgnoreCase(MAP_X_INCREMENT)) {
            xIncrement = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MAP_Y_INCREMENT)) {
            yIncrement = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MAP_X_POINTS)) {
            xPoints = value.intValue();
        } else if (attribute.equalsIgnoreCase(MAP_Y_POINTS)) {
            yPoints = value.intValue();
        } else if (attribute.equalsIgnoreCase(SCAN_MODE)) {
            int iValue = value.intValue();
            switch (iValue) {
            case 0:
                pointLocation = POINT_LOCATION.UPPER_LEFT;
                break;
            case 64:
                pointLocation = POINT_LOCATION.LOWER_LEFT;
                break;
            case 128:
                pointLocation = POINT_LOCATION.UPPER_RIGHT;
                break;
            case 192:
                pointLocation = POINT_LOCATION.LOWER_RIGHT;
                break;
            }
        } else if (attribute.equalsIgnoreCase(MAP_PROJECTION)) {
            int iValue = value.intValue();

            if (30 == iValue || 3 == iValue) {
                projection = PROJECTION.LAMBERT_CONFORMAL;
            } else if (10 == iValue || 1 == iValue) {
                projection = PROJECTION.MERCATOR;
            } else if (20 == iValue || 5 == iValue) {
                projection = PROJECTION.POLAR_STEREOGRAPHIC;
            } else if (0 == iValue) {
                projection = PROJECTION.LAT_LON;
            } else {
                projection = null;
            }

        } else if (attribute.equalsIgnoreCase(BASE_LONGITUDE)) {
            baseLon = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MAP_TANGENT_LAT1)) {
            tangentLat1 = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MAP_TANGENT_LAT2)) {
            tangentLat2 = value.doubleValue();
            // } else if (attribute.equalsIgnoreCase(LAT_OF_ORIGIN)) {
            // latOfOrigin = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MAJOR_AXIS)) {
            majorAxis = value.doubleValue();
        } else if (attribute.equalsIgnoreCase(MINOR_AXIS)) {
            minorAxis = value.doubleValue();
            // } else if (attribute.equalsIgnoreCase(RADAR_RADIAL_LAT)) {
            // radialLat = value.doubleValue();
            // } else if (attribute.equalsIgnoreCase(RADAR_RADIAL_LON)) {
            // radialLon = value.doubleValue();
        }
    }

    public CoordinateReferenceSystem getCRS() {

        if (projection == PROJECTION.LAMBERT_CONFORMAL) {
            crs = MapUtil.constructLambertConformal(this.majorAxis,
                    this.minorAxis, this.tangentLat1, this.tangentLat2,
                    this.baseLon);
            return crs;
        } else if (projection == PROJECTION.LAT_LON) {
            crs = MapUtil.LATLON_PROJECTION;
            return crs;
        }

        return null;

    }

    public GridGeometry2D getGridGeometry(CoordinateReferenceSystem inputCRS)
            throws TransformException, FactoryException {
        crs = inputCRS;
        return getGridGeometry();
    }

    public GridGeometry2D getGridGeometry() throws TransformException,
            FactoryException {

        if (crs == null) {
            getCRS();
        }

        if (xPoints > 0 && yPoints > 0) {
            MathTransform mt = MapUtil.getTransformFromLatLon(crs);
            MathTransform revTransform = MapUtil.getTransformToLatLon(crs);

            // Construct an envelope
            double pt1x, pt1y, pt2x, pt2y;
            // If we have first and last lat, use them

            // Adjust pt1 by one grid cell
            DirectPosition nativeP1 = mt.transform(new DirectPosition2D(
                    firstLon, firstLat), new DirectPosition2D());
            double nativeP1x = nativeP1.getOrdinate(0);
            double nativeP1y = nativeP1.getOrdinate(1);

            // If scanning mode is on right, grid must be shifted in x
            if (pointLocation == POINT_LOCATION.UPPER_RIGHT
                    || pointLocation == POINT_LOCATION.LOWER_RIGHT) {
                nativeP1x -= (xIncrement * 1);
            }

            // If scanning mode is on bottom, grid must be shifted in y
            if (pointLocation == POINT_LOCATION.LOWER_LEFT
                    || pointLocation == POINT_LOCATION.LOWER_RIGHT) {
                nativeP1y -= (yIncrement * 1);
            }

            DirectPosition p1Adjusted = revTransform.transform(
                    new DirectPosition2D(nativeP1x, nativeP1y),
                    new DirectPosition2D());

            pt1x = p1Adjusted.getOrdinate(0);
            pt1y = p1Adjusted.getOrdinate(1);

            pt2x = lastLon;
            pt2y = lastLat;

            if ((pt2x == FILL_VALUE || pt2y == FILL_VALUE) && xPoints > 0
                    && yPoints > 0 && xIncrement != FILL_VALUE
                    && yIncrement != FILL_VALUE) {
                // try to derive the second point
                double nativeP2x = nativeP1x + (xPoints * xIncrement);

                double nativeP2y = nativeP1y + (yPoints * yIncrement);

                // If scanning mode is in on right, grid must be shifted in y
                if (pointLocation == POINT_LOCATION.UPPER_RIGHT
                        || pointLocation == POINT_LOCATION.UPPER_LEFT) {
                    nativeP2y -= (yIncrement * 1);
                }

                // If scanning mode is on left, grid must be shifted in x
                if (pointLocation == POINT_LOCATION.LOWER_LEFT
                        || pointLocation == POINT_LOCATION.UPPER_LEFT) {
                    nativeP2x -= (xIncrement * 1);
                }

                DirectPosition p2LatLon = revTransform.transform(
                        new DirectPosition2D(nativeP2x, nativeP2y),
                        new DirectPosition2D());
                pt2x = p2LatLon.getOrdinate(0);
                pt2y = p2LatLon.getOrdinate(1);

            }

            if (pt1x == FILL_VALUE || pt1y == FILL_VALUE || pt2x == FILL_VALUE
                    || pt2y == FILL_VALUE) {
                return null;
            }

            GeneralEnvelope env = new GeneralEnvelope(2);
            DirectPosition pt1 = mt.transform(new DirectPosition2D(pt1x, pt1y),
                    new DirectPosition2D());
            DirectPosition pt2 = mt.transform(new DirectPosition2D(pt2x, pt2y),
                    new DirectPosition2D());
            env.setRange(0, Math.min(pt1.getOrdinate(0), pt2.getOrdinate(0)),
                    Math.max(pt1.getOrdinate(0), pt2.getOrdinate(0)));
            env.setRange(1, Math.min(pt1.getOrdinate(1), pt2.getOrdinate(1)),
                    Math.max(pt1.getOrdinate(1), pt2.getOrdinate(1)));
            env.setCoordinateReferenceSystem(crs);

            return new GridGeometry2D(new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { xPoints, yPoints }, false),
                    env);

        }
        return null;
    }

    // private boolean withinTol(double v1, double v2) {
    //
    // long roundedV1 = Math.round(v1 * 100);
    // long roundedV2 = Math.round(v2 * 100);
    // if (roundedV1 == roundedV2)
    // return true;
    //
    // return false;
    // }

    private void setCoverageWkt() {
        try {
            WKTReader reader = new WKTReader();
            Geometry geometry = reader.read(this.coveragewkt);
            Coordinate[] coordinates = geometry.getCoordinates();
            firstLat = coordinates[0].y;
            firstLon = coordinates[0].x;
            lastLat = coordinates[2].y;
            lastLon = coordinates[2].x;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Returns the CRS as derived from the GRIB meta-data's CRS WKT.
     * 
     * @return the GRIB CRS
     */
    public CoordinateReferenceSystem getGribCRS() {
        try {
            return CRSCache.getInstance().getCoordinateReferenceSystem(crswkt);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Creates the Radar CRS for this adapter.
     * 
     * @return the Radar CRS - returns null if an error occurs.
     */
    public CoordinateReferenceSystem getRadarCRS() {
        // ArrayList<String> fields = new ArrayList<String>();
        // fields.add("lat");
        // fields.add("lon");
        //
        // RadarStation station = null;
        // try {
        // radarStationDao = (RadarStationDao) DaoPool.getInstance()
        // .borrowObject(RadarStationDao.class);
        // station = radarStationDao.queryByRdaId(this.icao);
        // DaoPool.getInstance().returnObject(radarStationDao);
        //
        // } catch (Exception e) {
        // e.printStackTrace();
        // return null;
        // }
        //
        // if (station != null) {
        //
        // double lat = station.getLat();
        // double lon = station.getLon();
        // return MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
        // MapUtil.AWIPS_EARTH_RADIUS, lat, lon);
        // } else {
        // return null;
        // }
        return null;

    }

}
