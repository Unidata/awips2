/**
 * 
 * This class is a factory for retrieving or creating a McidasMapCoverage object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/2009		144			T. Lee		Created
 * 12/2009		144			T. Lee		Migrated to TO11D6
 * 11/2013      1066        G. Hull     constructCRSfromWKT (from McidasMapCoverage)
 * 
 * </pre>
 * 
 * @author tlee
 * @version 1
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.dao.McidasMapCoverageDao;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchIdentifierException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

public class McidasSpatialFactory {

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static McidasSpatialFactory instance;

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public static synchronized McidasSpatialFactory getInstance() {
        if (instance == null) {
            instance = new McidasSpatialFactory();
        }
        return instance;
    }

    /**
     * Retrieves or generates a satellite map coverage object for remapped
     * projections
     * 
     * @param mapProjection
     *            The projection
     * @param nx
     *            The number of columns
     * @param ny
     *            The number of rows
     * @param dx
     *            The distance between x points
     * @param dy
     *            The distance between y points
     * @param clon
     *            The orientation of the grid
     * @param stdlat1
     *            The latitude at which the Lambert projection cone is tangent
     *            to the earth
     * @param lllat
     *            Latitude of first point
     * @param lllon
     *            Longitude of first point
     * @param urlat
     *            Latitude of last point
     * @param urlon
     *            Longitude of last point
     * @return A SatMapCoverage object with the given values
     * @throws Exception
     *             If errors occur during db interaction or creation of the
     *             coverage object
     */
    public synchronized McidasMapCoverage getMapCoverage(Integer iproj,
            Integer nx, Integer ny, Float dx, Float dy, Float clon,
            Float stdlat1, Float stdlat2, Float lllat, Float lllon,
            Float urlat, Float urlon, double earthRadius) throws Exception {
        McidasMapCoverage mapCoverage = null;
        McidasMapCoverageDao satDao = new McidasMapCoverageDao();

        try {
            // Check the database to see if a coverage already exists
            mapCoverage = satDao.getSatCoverage(iproj, nx, ny, dx, dy, clon,
                    stdlat1, stdlat2, lllat, lllon, urlat, urlon);

            // If the database does not contain an existing sat map coverage for
            // the given values, create one
            if (mapCoverage == null) {
                mapCoverage = createMapCoverage(iproj, nx, ny, dx, dy, clon,
                        stdlat1, stdlat2, lllat, lllon, urlat, urlon,
                        earthRadius);

                // Persist the new coverage to the database
                satDao.persist(mapCoverage);
            }
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to retrieve or construct valid Satellite Map Coverage",
                    e);
        }

        if (mapCoverage == null) {
            throw new DataAccessLayerException(
                    "Unable to retrieve or construct valid Satellite Map Coverage");
        }

        return mapCoverage;
    }

    /**
     * Creates a new SatMapCoverage object from scratch with the given
     * parameters
     * 
     * @param mapProjection
     *            The projection
     * @param nx
     *            The number of columns
     * @param ny
     *            The number of rows
     * @param dx
     *            The distance between x points
     * @param dy
     *            The distance between y points
     * @param clon
     *            The orientation of the grid
     * @param latin
     *            The latitude at which the Lambert projection cone is tangent
     *            to the earth
     * @param lllat
     *            Latitude of first point
     * @param lllon
     *            Longitude of first point
     * @param urlat
     *            Latitude of last point
     * @param urlon
     *            Longitude of last point
     * @return A SatMapCoverage object with the given values
     * @throws Exception
     *             If errors occur during generation of the coverage object
     */
    private synchronized McidasMapCoverage createMapCoverage(
            Integer mapProjection, Integer nx, Integer ny, Float dx, Float dy,
            Float clon, Float stdlat1, Float stdlat2, Float lllat, Float lllon,
            Float urlat, Float urlon, double earthRadius) throws Exception {

        logger.debug("Creating map coverage object");
        ProjectedCRS crs = null;

        // Get the correct CRS
        if (mapProjection == 1) {
            // double cm = 0.0;
            // if ((lllon > 0.0) && (urlon < 0.0)) {
            // cm = 180.0;
            // }
            crs = MapUtil.constructMercator(earthRadius, earthRadius, stdlat1,
                    clon);
        } else if (mapProjection == 3) {
            crs = MapUtil.constructLambertConformal(earthRadius, earthRadius,
                    stdlat1, stdlat2, clon);
        } else {
            if (stdlat1 >= 0.)
                crs = MapUtil.constructNorthPolarStereo(earthRadius,
                        earthRadius, stdlat1, clon);
            else
                crs = constructSouthPolarStereo(earthRadius, earthRadius,
                        stdlat1, clon);
        }

        DirectPosition2D firstPosition = null;
        DirectPosition2D secondPosition = null;
        DirectPosition2D thirdPosition = null;
        DirectPosition2D fourthPosition = null;
        DirectPosition2D corner1 = new DirectPosition2D();
        DirectPosition2D corner2 = new DirectPosition2D();
        DirectPosition2D corner3 = new DirectPosition2D();
        DirectPosition2D corner4 = new DirectPosition2D();

        /*
         * Projection is Mercator. Determine corner points from
         * lllat,lllon,urlat,urlon provided in the satellite file
         */
        if (mapProjection == 1) {
            logger.debug("Determining corner points for Mercator projection");
            corner1.x = lllon;
            corner1.y = lllat;

            corner3.x = urlon;
            corner3.y = urlat;

            corner2.x = urlon;
            corner2.y = lllat;

            corner4.x = lllon;
            corner4.y = urlat;

        }
        /*
         * Projection is Lambert Conformal or Polar Stereographic. Therefore,
         * the corner points must be calculated
         */
        else {
            logger.debug("Determining corner points for Lambert Conformal or Polar Stereographic projection");

            // Get the transforms to be used to convert between meters and
            // lat/lon
            MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
            MathTransform toLatLon = fromLatLon.inverse();

            // Use lllat and lllon to specify the first point
            firstPosition = new DirectPosition2D();
            fromLatLon.transform(new DirectPosition2D(lllon, lllat),
                    firstPosition);

            int nxm1 = nx - 1;
            int nym1 = ny - 1;
            // Determine the 3 other corner points using the given dx,dy,nx, and
            // ny in meters
            secondPosition = new DirectPosition2D(dx * nxm1 + firstPosition.x,
                    firstPosition.y);
            thirdPosition = new DirectPosition2D(dx * nxm1 + firstPosition.x,
                    dy * nym1 + firstPosition.y);
            fourthPosition = new DirectPosition2D(firstPosition.x, dy * nym1
                    + firstPosition.y);

            // Convert the corner points from meters to lat/lon
            toLatLon.transform(firstPosition, corner1);
            toLatLon.transform(secondPosition, corner2);
            toLatLon.transform(thirdPosition, corner3);
            toLatLon.transform(fourthPosition, corner4);
        }

        // Construct the polygon constructor String
        StringBuffer buffer = new StringBuffer();
        buffer.append("POLYGON((");
        buffer.append(corner1.x + " " + corner1.y + ",");
        buffer.append(corner2.x + " " + corner2.y + ",");
        buffer.append(corner3.x + " " + corner3.y + ",");
        buffer.append(corner4.x + " " + corner4.y + ",");
        buffer.append(corner1.x + " " + corner1.y);
        buffer.append("))");

        // Create the geometry from the constructed String
        Geometry geometry = new WKTReader().read(buffer.toString());

        McidasMapCoverage mapCoverage = new McidasMapCoverage(mapProjection,
                nx, ny, dx, dy, clon, stdlat1, stdlat2, lllat, lllon, urlat,
                urlon, crs, geometry);

        return mapCoverage;
    }

    /**
     * Construct a south polar stereographic projection
     * 
     * @param majorAxis
     *            the major axis in meters
     * @param minorAxis
     *            the minor axis in meters
     * @param stdParallel
     *            the standard parallel
     * @param centralMeridian
     *            the central meridian
     * @return the constructed projection
     */
    private ProjectedCRS constructSouthPolarStereo(double majorAxis,
            double minorAxis, double stdParallel, double centralMeridian) {
        try {
            DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Stereographic_South_Pole");

            parameters.parameter("semi_major").setValue(majorAxis);
            parameters.parameter("semi_minor").setValue(minorAxis);
            parameters.parameter("standard_parallel_1").setValue(stdParallel);
            parameters.parameter("central_meridian").setValue(centralMeridian);
            parameters.parameter("false_easting").setValue(0.0);
            parameters.parameter("false_northing").setValue(0.0);

            String name = "AWIPS Polar Stereographic (SP: " + stdParallel
                    + ", CM: " + centralMeridian + ")";

            return MapUtil.constructProjection(name, parameters);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Retrieves or generates a satellite map coverage object for native
     * satellite navigation
     * 
     * @param iproj
     * @param nx
     *            Number of elements per line in area
     * @param ny
     *            Number of lines in area
     * @param reflon
     *            Reference Longitude
     * @param upperLeftElement
     *            image element coordinate of area line 0, element 0
     * @param upperLeftLine
     *            image line coordinate of area line 0, element 0
     * @param xres
     *            Element resolution
     * @param yres
     *            Line resolution
     * @param navigation
     *            Satellite NAV BLOCK
     * @return
     * @throws Exception
     */
    public McidasMapCoverage getMapCoverage(Integer iproj, Integer nx,
            Integer ny, Float reflon, int upperLeftElement, int upperLeftLine,
            int xres, int yres, byte[] navigation) throws Exception {

        McidasMapCoverage mapCoverage = null;
        McidasMapCoverageDao satDao = new McidasMapCoverageDao();

        String type = new String(navigation, 0, 4);
        String encodedNav = encodeNavBlock(navigation);

        try {
            // Check the database to see if a coverage already exists
            mapCoverage = satDao.getSatCoverage(iproj, nx, ny,
                    upperLeftElement, upperLeftLine, xres, yres, encodedNav);
            // TODO

            // If the database does not contain an existing sat map coverage for
            // the given values, create one
            if (mapCoverage == null) {
                mapCoverage = createMapCoverage(iproj, nx, ny, reflon,
                        upperLeftElement, upperLeftLine, xres, yres, type,
                        encodedNav);

                // Persist the new coverage to the database
                satDao.persist(mapCoverage);
            }
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to retrieve or construct valid raw Satellite Map Coverage",
                    e);
        }

        if (mapCoverage == null) {
            throw new DataAccessLayerException(
                    "Unable to retrieve or construct valid raw Satellite Map Coverage");
        }

        return mapCoverage;
    }

    private McidasMapCoverage createMapCoverage(Integer mapProjection,
            Integer nx, Integer ny, Float reflon, int upperLeftElement,
            int upperLeftLine, int xres, int yres, String type,
            String encodedNav) throws Exception {

        ProjectedCRS crs = null;

        // Get the correct CRS
        if (mapProjection == 7585) {
            crs = constructCRS(type, encodedNav);
        }

        // Construct the polygon constructor String
        StringBuffer buffer = new StringBuffer();
        buffer.append("POLYGON((");
        buffer.append(reflon - 90. + " -90.0,");
        buffer.append(reflon + 90. + " -90.0,");
        buffer.append(reflon + 90. + " 90.0,");
        buffer.append(reflon - 90. + " 90.0,");
        buffer.append(reflon - 90. + " -90.0");
        buffer.append("))");

        // Create the geometry from the constructed String
        Geometry geometry = new WKTReader().read(buffer.toString());

        McidasMapCoverage mapCoverage = new McidasMapCoverage(mapProjection,
                nx, ny, reflon, upperLeftElement, upperLeftLine, xres, yres,
                crs, geometry);

        return mapCoverage;

    }

    private String encodeNavBlock(byte[] navigation) {

        Base64 b64 = new Base64();
        byte[] coded = b64.encode(navigation);

        return new String(coded);
    }

    public ProjectedCRS constructCRSfromWKT( String crsWKT) {
        Pattern p = Pattern.compile("PROJCS\\[\"MCIDAS\\sAREA\\s(.*)\"");
        Matcher m = p.matcher(crsWKT);
        m.find();
        ProjectedCRS crsObject=null;
        
        if ( m.groupCount() == 1 ) {
        	String type = m.group(1);
        	//System.out.println("FOUND PROJCS:"+m.group(0)+":"+type);
        	p = Pattern.compile("\\[\"NAV_BLOCK_BASE64\",\\s\"(.*)\"\\]");
        	m = p.matcher(crsWKT);
        	boolean found = m.find();

        	//System.out.println(m.group());
        	//System.out.println(m.groupCount()+m.group(1));
        	if ( found ) {
        		String navBlock = m.group(1);
        		crsObject = McidasSpatialFactory.getInstance().constructCRS(type, navBlock);
        	}
        }

        return crsObject;
    }
    
    public ProjectedCRS constructCRS(String type, String encoded) {

        ParameterValueGroup pvg = null;

        DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();
        try {
            pvg = dmtFactory.getDefaultParameters("MCIDAS_AREA_NAV");
        } catch (NoSuchIdentifierException e1) {
            e1.printStackTrace();
        }

        /*
         * semi_major and semi_minor parameters are set to 1, so that no global
         * scaling is performed during coordinate transforms by
         * org.geotools.referencing.operation.projection.MapProjection based on
         * the radius of earth
         */
        pvg.parameter("semi_major").setValue(1.0);
        pvg.parameter("semi_minor").setValue(1.0);
        pvg.parameter("central_meridian").setValue(0.0);
        // pvg.parameter("scale_factor").setValue(1.0);

        pvg.parameter("NAV_BLOCK_BASE64").setValue(encoded);
        // System.out.println(pvg.toString() );

        String projectionName = "MCIDAS AREA " + type;
        ProjectedCRS mcidasCRS = null;
        try {
            mcidasCRS = MapUtil.constructProjection(projectionName, pvg);
        } catch (NoSuchIdentifierException e) {
            e.printStackTrace();
        } catch (FactoryException e) {
            e.printStackTrace();
        }

        return mcidasCRS;
    }

}
