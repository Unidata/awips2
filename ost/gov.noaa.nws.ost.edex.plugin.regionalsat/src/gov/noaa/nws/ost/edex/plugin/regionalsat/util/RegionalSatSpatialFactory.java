/**
 * 
 * gov.noaa.nws.ost.edex.plugin.regionalsat.util.RegionalSatSpatialFactory
 * 
 * 12-01-11
 * 
 * This code has been developed by the NWS/OST/SEC for use in the AWIPS2 system.
 *
 **/

package gov.noaa.nws.ost.edex.plugin.regionalsat.util;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.satellite.dao.SatMapCoverageDao;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

/**
 * The RegionalSatSpatialFactory class is responsible for creating a SatMapCoverage object.
 * This class is based on the SatSpatialFactory class.  TODO; The class needs to be refactored 
 * to reduce code duplication. The class should use the Abstract Factory Method design pattern, 
 * but the code refactor requires changes to the base code. A TTR has been submitted to refactor 
 * the Satellite spatial factory classes in edex.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                   
 * date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 7/15/11                      tk    	Initial Creation                          
 * 
 * </pre>
 * 
 * @author tk
 * @version 1.0
 */

public class RegionalSatSpatialFactory {

	 /** The logger */
    private Log logger = LogFactory.getLog(getClass());
    
    /** The singleton instance */
    private static RegionalSatSpatialFactory instance;

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public static synchronized RegionalSatSpatialFactory getInstance() {
        if (instance == null) {
            instance = new RegionalSatSpatialFactory();
        }
        return instance;
    }
    
    /**
     * make default constructor private to disable 
     * object creation from outside the class
     * 
     */
    private RegionalSatSpatialFactory()  {
    	
    }
    /**
     * Retrieves or generates a satellite map coverage object
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
     * @param lov
     *            The orientation of the grid
     * @param latin
     *            The latitude at which the Lambert projection cone is tangent
     *            to the earth
     * @param la1
     *            Latitude of first point
     * @param lo1
     *            Longitude of first point
     * @param la2
     *            Latitude of last point
     * @param lo2
     *            Longitude of last point
     * @return A SatMapCoverage object with the given values
     * @throws Exception
     *             If errors occur during db interaction or creation of the
     *             coverage object
     */
    public synchronized SatMapCoverage getMapCoverage(
            Integer mapProjection, Integer nx, Integer ny, Float dx, Float dy,
            Float lov, Float latin, Float la1, Float lo1, Float la2, Float lo2)
            throws Exception {

        SatMapCoverage mapCoverage = null;
        SatMapCoverageDao satDao = new SatMapCoverageDao();

        try {
            // Check the database to see if a coverage already exists
            mapCoverage = satDao.getSatCoverage(mapProjection, nx, ny, dx, dy,
                    lov, latin, la1, lo1, la2, lo2);

            // If the database does not contain an existing sat map coverage for
            // the given values, create one
            if (mapCoverage == null) {
                mapCoverage = createMapCoverage(mapProjection, nx, ny, dx, dy,
                        lov, latin, la1, lo1, la2, lo2);
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
     * @param lov
     *            The orientation of the grid
     * @param latin
     *            The latitude at which the Lambert projection cone is tangent
     *            to the earth
     * @param la1
     *            Latitude of first point
     * @param lo1
     *            Longitude of first point
     * @param la2
     *            Latitude of last point
     * @param lo2
     *            Longitude of last point
     * @return A SatMapCoverage object with the given values
     * @throws Exception
     *             If errors occur during generation of the coverage object
     */
	private synchronized SatMapCoverage createMapCoverage(
            Integer mapProjection, Integer nx, Integer ny, Float dx, Float dy,
            Float lov, Float latin, Float la1, Float lo1, Float la2, Float lo2)
            throws Exception {

        logger.debug("Creating map coverage object");

        ProjectedCRS crs = null;
        // Get the correct CRS
        if (mapProjection == 1) {
            crs = MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, latin, lov);
        } else if (mapProjection == 3) {
            crs = MapUtil.constructLambertConformal(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, latin, latin, lov);
        } else if (mapProjection == 7) {
            crs = MapUtil.constructEquidistantCylindrical(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, lov, latin);
        } else {
            crs = MapUtil.constructNorthPolarStereo(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, 60, lov);
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
         * Projection is Mercator. Determine corner points from la1,lo1,la2,lo2
         * provided in the satellite file. Image starts with ul corner
         */
        if (mapProjection == 1) {
            logger.debug("Determining corner points for Mercator projection");
            
            corner4.x = lo1;
            corner4.y = la1;

            corner2.x = lo2;
            corner2.y = la2;

            corner3.x = lo2;
            corner3.y = la1;

            corner1.x = lo1;
            corner1.y = la2;
        }
        else { 
    	   /*
    	   * Projection is for Polar Stereographic, EquidistantCylindrical or " 
           * "Lambert Conformal. Therefore,the corner points must be calculated
           */
           logger.debug("Determining corner points for Polar Stereographic, " + 
        		   "EquidistantCylindrical or Lambert Conformal");

           // make lon values positive since -170 is the same as +190 degrees
           double lon1 = 0.0f;
           if (lo1 < 0.0f) {
        	   lon1 = (180.0 + lo1) + 180.0;
           } else {
        	   lon1 = lo1;
           }
           
           double lon2 = 0.0f;
           if (lo2 < 0.0f) {
        	   lon2 = (180.0 + lo2) + 180.0;
           } else {
        	   lon2 = lo1;
           }
           
           
           // Get the transforms to be used to convert between meters and
           // lat/lon
           MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
           MathTransform toLatLon = fromLatLon.inverse();

           if (la2 < la1) {
        	   if (lon2 < lon1) {
        		// Use la2 and lo2 to specify the first point starting with ll corner
        		   logger.debug("Starting with lower left corner Fourth Position");
        		   
                   fourthPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo2, la2), fourthPosition);
                            
                   logger.debug("Fourth Position:x,y " + fourthPosition.x + ", " + fourthPosition.y);
                   
                   secondPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo1, la1), secondPosition);
                   
                   logger.debug("Second Position:x,y " + secondPosition.x + ", " + secondPosition.y);

                   firstPosition = new DirectPosition2D(secondPosition.x, fourthPosition.y);
                                     
                   logger.debug(" First Position:x,y " + firstPosition.x + ", " + firstPosition.y);
                  
                   thirdPosition = new DirectPosition2D(fourthPosition.x, secondPosition.y);

                   logger.debug("Third Position:x,y " + thirdPosition.x + ", " + thirdPosition.y);

        	   } else {
        		   // Use la2 and lo2 to specify the fourth point starting with lr corner
        		   logger.debug("Polorsterographic for Alaska start with lower right corner " +
        				   "Third Position");
        		   
         		   thirdPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo2, la2), thirdPosition);
                              
                   logger.debug("Third Position:x,y " + thirdPosition.x + ", " + thirdPosition.y);
               
                   firstPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo1, la1), firstPosition);
                   logger.debug("First Position:x,y " + firstPosition.x + ", " + firstPosition.y);

                   fourthPosition = new DirectPosition2D(thirdPosition.x, firstPosition.y);
                   
                   logger.debug("Fourth Position:x,y " + fourthPosition.x + ", " + fourthPosition.y);

                   secondPosition = new DirectPosition2D(firstPosition.x, thirdPosition.y);

                   logger.debug("Second Position:x,y " + secondPosition.x + ", " + secondPosition.y);
                   
        	   }
           } else {
        	   if (lon2 < lon1) {
        		// Use la2 and lo2 to specify the first point starting with ul corner
        		   logger.debug("Starting with lower upper left Fourth Position");
        		   
        		   firstPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo2, la2), firstPosition);
                             
                   logger.debug("First Position:x,y " + firstPosition.x + ", " + firstPosition.y);
                 
                   thirdPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo1, la1), thirdPosition);

                   logger.debug("Third Position:x,y " + thirdPosition.x + ", " + thirdPosition.y); 
 
                   secondPosition = new DirectPosition2D(firstPosition.x, thirdPosition.y);
                    
                   logger.debug("Second Position:x,y " + secondPosition.x + ", " + secondPosition.y);

                   fourthPosition = new DirectPosition2D(thirdPosition.x, firstPosition.y);
                   
                   logger.debug("Fourth Position:x,y " + fourthPosition.x + ", " + fourthPosition.y);

        	   } else {
        		// Use la2 and lo2 to specify the fourth point starting with ur corner
        		   logger.debug("Starting with lower right corner Fourth Position");
        		   
        		   secondPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo2, la2), secondPosition);
                                
                   logger.debug("Second Position:x,y " + secondPosition.x + ", " + secondPosition.y);
	
                   fourthPosition = new DirectPosition2D();
                   fromLatLon.transform(new DirectPosition2D(lo1, la1), fourthPosition);
                   
                   logger.debug("Fourth Position:x,y " + fourthPosition.x + ", " + fourthPosition.y);

                   firstPosition = new DirectPosition2D(secondPosition.x, fourthPosition.y);
                   
                   logger.debug("First Position:x,y " + firstPosition.x + ", " + firstPosition.y);

                   thirdPosition = new DirectPosition2D(fourthPosition.x, secondPosition.y);

                   logger.debug("Third Position:x,y " + thirdPosition.x + ", " + thirdPosition.y);
        		
        	   }
           }

           // Convert the corner points from meters to lat/lon
           toLatLon.transform(firstPosition, corner1);
           toLatLon.transform(secondPosition, corner2);
           toLatLon.transform(thirdPosition, corner3);
           toLatLon.transform(fourthPosition, corner4);
           
           logger.debug("First Corner:" + corner1.toString());

           logger.debug("Second Corner:" + corner2.toString());

           logger.debug("Third Corner:" + corner3.toString());

           logger.debug("Fourth Corner:" + corner4.toString());
       }
         /* 
         * Projection is EquidistantCylindrical or Polar Stereographic. Therefore,
         * the corner points must be calculated
         */

        // Construct the polygon constructor String
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("POLYGON((");
        buffer.append(corner4.x + " " + corner4.y + ",");
        buffer.append(corner1.x + " " + corner1.y + ",");
        buffer.append(corner2.x + " " + corner2.y + ",");
        buffer.append(corner3.x + " " + corner3.y + ",");
        buffer.append(corner4.x + " " + corner4.y);
        buffer.append("))");
        
        logger.debug("Polygon: " + buffer.toString());

        // Create the geometry from the constructed String
        Geometry geometry = new WKTReader().read(buffer.toString());
        la2 = (float)corner2.y;
        lo2 = (float)corner2.x;
        la1 = (float)corner4.y;
        lo1 = (float)corner4.x;
        logger.debug("(Lo1, La1),(lo2, La2) (" + lo1 + ", " + la1 + "), (" + lo2 + ", " + la2 +")");
        SatMapCoverage mapCoverage = new SatMapCoverage(mapProjection,
                nx, ny, dx, dy, lov, latin, la1, lo1, la2, lo2, crs, geometry);

        return mapCoverage;
    }

}
