/**
 *  LatLonLocTbl - A Java class to define some known VORs and Intlsig talbes
 *  used to define convective/nonconvective/airmet/intl SIGMET locations.
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 Jun 2009  95/132     B. Hebbard  Initial creation.
 * 10 Sep 2009  39/87/114  L. Lin	   Remove the temporary enum
 *                                     and add xml for VORs and 
 *                                     Intlsig gempak tables.
 * 30 Sep 2009       3102  jkorman     Changed printlns to logging statements.
 * 07 Jan 2014             njensen     Better error messages
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.tools.decoder;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField.StationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

public class LatLonLocTbl {
    private static Log logger = LogFactory.getLog(LatLonLocTbl.class);

    static StationTable vorsloc = null;

    static StationTable intlsigloc = null;

    static StationTable myloc = null;

    private double latitude;

    private double longitude;

    private LatLonLocTbl(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    public static void readLocTable(String tableName) throws Exception {

//        final String NCEP_DIR = "ncep";
//        final String stnsDir = "stns";
//        final String vorsLocTableName = "vors.xml";

        final String VORS_TABLE = "ncep" + File.separator + "stns" + File.separator + "vors.xml";
        IPathManager manager = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = null;

        baseContext = manager.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        LocalizationFile file = null;
        if (tableName == "vors") {
              file = manager.getStaticLocalizationFile(VORS_TABLE); 
        }

        if ( file != null )
        myloc = new StationTable(file.getFile().getAbsolutePath());

    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public LatLonPoint getLatLonPoint() {
        return new LatLonPoint(latitude, longitude, LatLonPoint.INDEGREES);
    }

    private enum Direction {
        N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW;
        public double getDegrees() {
            return ordinal() * 22.5;
        }
    }

    private static final double ONE_NM_RADIANS = Math.toRadians(1.0 / 60.0);

    /**
     * Given a relative reference string, returns a LatLonPoint
     * (com.raytheon.uf.edex.decodertools.core.LatLonPoint).
     * 
     * @param location
     *            A String such as... "BOS" "20S EMI" "30 WNW BUM" " 40ENE HUH "
     *            ...referencing a VOR listed in AC 00-45F (Appendix F),
     *            optionally preceded by distance in nautical miles and 16-point
     *            compass direction string.
     * @param locTable
     *            A string such as "vors" referring to "vors" location table or
     *            "intlsig" referring to intl location table
     * @return The decoded location as a LatLonPoint; null on error (such as
     *         unrecognized VOR identifier or direction string).
     * 
     */
    public static LatLonPoint getLatLonPoint(String location, String locTable) {
        LatLonPoint point = null;
        Station vor = null;
        // Wrap decoding in a try block, in case of exception on
        // one of the xml or direction enum, or other problems.

        try {
            location = location.trim();

            // VOR is always last 3 nonblank char of location
            String navaid = location.substring(location.length() - 3);

            // Read in the location table XML if not exists
            if (myloc == null) {
                readLocTable(locTable);
                logger.debug(" - read vors.xml to cache");
            }
            // Search station ID and return whole station record
            if (myloc != null) {
                logger.debug(" - navaid = " + navaid);
                vor = myloc.getStation(StationField.STID, navaid);
            } else {
                logger.debug(" - myloc is null");
            }

            // Get LatLonPoint from lat/lon
            if (vor != null) {
                point = new LatLonPoint(vor.getLatitude(), vor.getLongitude(),
                        LatLonPoint.INDEGREES);
            } else {
                logger.warn(" - DID NOT find station ID " + navaid + " in vors.xml");
            }

            // If there's an offset direction/bearing, process it
            if (location.length() > 3 && point != null) {
                String u = location.substring(0, location.length() - 3);

                Pattern p = Pattern.compile("^([0-9]+)\\s*([A-Z]+)");
                Matcher m = p.matcher(u);
                if (m.find()) {
                    String distanceStr = m.group(1);

                    String bearingStr = m.group(2);

                    int distanceNM = Integer.parseInt(distanceStr);

                    double distanceRad = distanceNM * ONE_NM_RADIANS;
                    // LatLonPoint.positionOf thinks bearing is CCW, not CW...
                    double bearingDeg = 360.0 - Direction.valueOf(bearingStr)
                            .getDegrees();
                    double bearingRad = Math.toRadians(bearingDeg);
                    point = point.positionOf(bearingRad, distanceRad);
                    logger.debug(" - get a good latlon point");
                }
            }
            return point;
        } catch (Exception e) {
            logger.error("[Error decoding location in LatLonLocTbl:  "
                    + location + "]", e);
            return null;
        }
    }

}
