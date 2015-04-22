/**
 *  SnsTnsLocTbl - A Java class to define some known 
 *  snstns stationNumbers.
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/2010  	210		   L. Lin      Initial creation
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

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;

public class SnsTnsLocTbl {

    private static StationTable snstnsTbl = null;

    private static double stnLat;

    private static double stnLon;

    private static int stnElev;

    private static String stnId;

    public static synchronized void readLocTable(String tableName)
            throws Exception {

        final String NCEP_DIR = "ncep";
        final String stnsDir = "stns";
        final String snstnsLocTableName = "snstns.xml";
        StationTable myTbl = null;

        IPathManager manager = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = null;
        File baseDir = null;
        String stnsFileName = null;
        baseContext = manager.getContext(EDEX_STATIC, LocalizationLevel.BASE);
        baseContext.setContextName(NCEP_DIR);
        baseDir = manager.getFile(baseContext, "");
        if (tableName == "snstns.xml") {
            stnsFileName = baseDir + File.separator + stnsDir + File.separator
                    + snstnsLocTableName;
            // System.out.println("SnsTnsLocTbl- stnsFileName=" + stnsFileName);
        }
        myTbl = getSnstnsTbl();
        if (myTbl == null) {
            myTbl = new StationTable(stnsFileName);
            setSnstnsTbl(myTbl);
            // System.out.println("SnsTnsLocTbl- stnsFileName=" + stnsFileName);
        }

    }

    /**
     * Given a valid station number, find station in "snstns.xml" and set
     * stnLat, stnLon, stnId, and stnElev.
     * 
     * @param stationNumber
     *            A String such as "01001"
     * @param locTable
     *            A string such as "snstns.xml"
     * @return
     * 
     */
    public static void getSnsTnsLocation(String stationNumber, String locTable) {
        Station stnLoc = null;
        stnId = "";
        // Wrap decoding in a try block, in case of exception on xml.
        try {
            String stnnum = stationNumber;

            // Read in the stationNumber table XML if not exists
            if (snstnsTbl == null) {
                readLocTable(locTable);
            }
            // Search station ID and return whole station record
            if (snstnsTbl != null) {
                stnLoc = snstnsTbl.getStation(StationField.STNM, stnnum);
            }

            if (stnLoc != null) {
                // Chin: to fix a digit precision issue in Java
                stnLat = Double
                        .parseDouble(Float.toString(stnLoc.getLatitude()));
                stnLon = Double.parseDouble(Float.toString(stnLoc
                        .getLongitude()));
                stnId = stnLoc.getStid();
                stnElev = stnLoc.getElevation();
            }
        } catch (Exception e) {
            // TODO: Use central error logging if this code is kept
            // System.out.println("[Error decoding stationNumber in SnsTnsLocTbl:  "
            // + stationNumber + "]");
        }
    }

    /**
     * @return the snstnsTbl
     */
    public static StationTable getSnstnsTbl() {
        return snstnsTbl;
    }

    /**
     * @param StationTable
     *            the snstnsTbl to set
     */
    public static void setSnstnsTbl(StationTable snstnsTbl) {
        SnsTnsLocTbl.snstnsTbl = snstnsTbl;
    }

    public static double getStnLat() {
        return stnLat;
    }

    public static void setStnLat(double stnLat) {
        SnsTnsLocTbl.stnLat = stnLat;
    }

    public static double getStnLon() {
        return stnLon;
    }

    public static void setStnLon(double stnLon) {
        SnsTnsLocTbl.stnLon = stnLon;
    }

    public static int getStnElev() {
        return stnElev;
    }

    public static void setStnElev(int stnElev) {
        SnsTnsLocTbl.stnElev = stnElev;
    }

    public static String getStnId() {
        return stnId;
    }

    public static void setStnId(String stnId) {
        SnsTnsLocTbl.stnId = stnId;
    }

}
