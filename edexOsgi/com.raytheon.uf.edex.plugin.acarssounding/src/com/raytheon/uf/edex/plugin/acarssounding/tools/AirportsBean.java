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
package com.raytheon.uf.edex.plugin.acarssounding.tools;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010            jkorman     Initial creation
 * Aug 18, 2014 3530       bclement    moved from common to edex
 * Dec 10, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AirportsBean {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String PATH_EXT = "stations";

    private static final String AIRPORTS = "airports.xml";

    private Airports airports = null;

    public AirportsBean() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String filePath = pathMgr.getFile(commonCx, PATH_EXT).getPath();

        airports = Airports.loadFromFile(filePath, AIRPORTS, logger);
        logger.info(String.format("%4d airports read from config.", airports
                .getAirport().size()));
    }

    /**
     * 
     * @param latitude
     * @param longitude
     * @param maxDistance
     * @return
     */
    public Airport nearest(Double latitude, Double longitude, Double maxDistance) {
        Airport retValue = null;
        if (airports != null) {
            retValue = airports.nearest(latitude, longitude, maxDistance);
        }
        return retValue;
    }

    /**
     * 
     * @param latLon
     * @param maxDistance
     * @return
     */
    public Airport nearest(LatLonPoint latLon, Double maxDistance) {
        Airport retValue = null;
        if (airports != null) {
            retValue = airports.nearest(latLon, maxDistance);
        }
        return retValue;
    }

    /**
     * 
     * @param latitude
     * @param longitude
     * @param maxDistance
     * @return
     */
    public Airport nearest(ACARSRecord acars, Double maxDistance) {
        Airport airport = null;
        if (airports != null) {

            LatLonPoint latLon = new LatLonPoint(acars.getLatitude(),
                    acars.getLongitude(), LatLonPoint.INDEGREES);

            airport = airports.nearest(latLon, maxDistance);
        }
        return airport;
    }

    public Airport getAirport(String airportId) {
        Airport retValue = null;
        if (airports != null) {
            retValue = airports.getAirport(airportId);
        }
        return retValue;
    }

}
