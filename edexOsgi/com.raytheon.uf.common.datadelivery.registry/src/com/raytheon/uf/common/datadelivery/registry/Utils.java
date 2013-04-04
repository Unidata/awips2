package com.raytheon.uf.common.datadelivery.registry;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Collection of convenience methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation.
 * Oct  1, 2012   1103      jpiatt      Added invalid subscription status.
 * Nov 20, 2012 1286       djohnson     Add UNSCHEDULED.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class Utils {

    /**
     * Get the geometry for a bounding box
     * 
     * @param upperLeft
     *           upper left corner
     * @param LowerRight
     *          lower right corner
     * 
     * @return bounding box
     *           counding box coordinates
     */
    public static Geometry getGeometry(Coordinate upperLeft,
            Coordinate LowerRight) {

        GeometryFactory factory = new GeometryFactory();
        Coordinate[] coors = new Coordinate[5];

        coors[0] = upperLeft;
        coors[1] = new Coordinate(upperLeft.x, LowerRight.y);
        coors[2] = LowerRight;
        coors[3] = new Coordinate(LowerRight.x, upperLeft.y);
        // complete the square
        coors[4] = coors[0];

        LinearRing lr = factory.createLinearRing(coors);
        Polygon poly = factory.createPolygon(lr, null);

        return poly;
    }

    /**
     * Date conversion.
     * 
     * @param format
     *          pass in date format
     * @param dateString
     *          date in string format
     *          
     * @return Date
     *          converted date
     * @throws ParseException
     */
    public static Date convertDate(String format, String dateString)
            throws ParseException {
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        return (sdf.parse(dateString));
    }

    /**
     * Subscription status options.
     */
    public static enum SubscriptionStatus {
        
        /** Active Subscription Status */
        ACTIVE("Active"), 
        /** Inactive Subscription Status */
        INACTIVE("Inactive"), 
        /** Expired Subscription Status */
        EXPIRED("Expired"), 
        /** Invalid Subscription Status */
        INVALID("Invalid"),
        /** Unscheduled Subscription Status */
        UNSCHEDULED("Unscheduled");

        private final String status;

        private SubscriptionStatus(String status) {
            this.status = status;
        }

        @Override
        public String toString() {
            return status;
        }
    }
}
