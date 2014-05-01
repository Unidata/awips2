/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.uf.common.geospatial;

import java.util.TimeZone;

import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Utility to retrieve local time zone given a lat/lon
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/04/2011        #7175 randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class LocalTimeZone {

    /**
     * Retriev the time zone for a given lat/lon
     * 
     * @param lonLat
     *            desired point
     * @return the local time zone
     * 
     * @throws SpatialException
     */
    public static TimeZone getLocalTimeZone(Coordinate lonLat)
            throws SpatialException {

        GeometryFactory gf = new GeometryFactory();
        ISpatialQuery sq = SpatialQueryFactory.create();
        SpatialQueryResult[] results = sq.query("timezones",
                new String[] { "unix_time" }, gf.createPoint(lonLat), null,
                SearchMode.WITHIN);

        if (results.length > 0) {
            String tz = (String) results[0].attributes.get("unix_time");
            return TimeZone.getTimeZone(tz);
        }

        return TimeZone.getTimeZone("GMT");
    }
}
