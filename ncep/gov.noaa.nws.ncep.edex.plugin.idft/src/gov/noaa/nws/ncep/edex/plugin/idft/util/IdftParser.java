/**
 * 
 * IdftParser
 * 
 * This class provides parser processing utilities for the IDFT Decoder Plug-In.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01Jun2009	   100		F. J. Yen	Initial creation
 * 01Oct2009	   100		F. J. Yen	Allow for multiple reads of idftLoc table with synchronized;
 * 										Remove unneeded imports after using generic StationTable
 * 08Dec2009	   100		F. J. Yen	Modified to11d3 to to11d6
 * 27May2010       100		F. J. Yen	Migrated from to11dr3 to to11dr11
 *
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS system.
 */
package gov.noaa.nws.ncep.edex.plugin.idft.util;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;
import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;

import java.io.File;
import java.util.List;
import java.util.regex.Matcher;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;

public class IdftParser {
    private static List<Station> list = null;

    public IdftParser() {
    }

    public static synchronized void readIdftLocs() throws Exception {

        final String NCEP_DIR = "ncep";
        final String stnsDir = "stns";
        final String idftLocTableName = "idftLoc.xml";

        IPathManager manager = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = null;
        File baseDir = null;
        String stnsFileName = null;
        baseContext = manager.getContext(EDEX_STATIC, LocalizationLevel.BASE);
        baseDir = manager.getFile(baseContext, NCEP_DIR);
        stnsFileName = baseDir + File.separator + stnsDir + File.separator
                + idftLocTableName;
        StationTable myloc = new StationTable(stnsFileName);
        /*
         * Read and put the IdftLocsTable in list.
         */
        list = myloc.getStationList();
    }

    /**
     * Decode the IDFT report.
     * 
     * @param matchLn
     *            IDFT point record to decode.
     * @param itype
     *            is 0 if no latitude/longitude in record. is 6 if
     *            latitude/longitude in record.
     * @param record
     *            IDFT record.
     * 
     */
    public static void processIdft(Matcher matchLn, int itype, IdftRecord record) {
        /*
         * Regular expression for IDFT report
         */
        String stnum;
        int istnum;
        Float signLl;
        record.setPointNum(Integer.parseInt(matchLn.group(1)));
        if (itype == 0) {
            /*
             * If itype equals 0, then no lat/lon in raw data for point. So, get
             * lat/lon from unmarshalled idftLoc.xml and then setLat and setLon.
             * 
             * Subtract 1 from indx since index of list starts at 0 whereas the
             * IDFT point numbers start at 1.
             */
            int indx = Integer.parseInt(matchLn.group(1)) - 1;
            try {
                stnum = list.get(indx).getStnnum();
                istnum = Integer.parseInt(stnum);
                /*
                 * Verify if index matches data.
                 */
                if (indx == istnum - 1) {
                    record.setLat(list.get(indx).getLatitude());
                    record.setLon(list.get(indx).getLongitude());
                } else {
                    System.out
                            .println("Point number "
                                    + istnum
                                    + " does not correspond to its position in idftLoc.xml");
                    record.setLat(IDecoderConstantsN.FLOAT_MISSING);
                    record.setLon(IDecoderConstantsN.FLOAT_MISSING);
                }
            } catch (IndexOutOfBoundsException e) {
                record.setLat(IDecoderConstantsN.FLOAT_MISSING);
                record.setLon(IDecoderConstantsN.FLOAT_MISSING);
                System.out
                        .println("Bad index to idftPoint created from idftLocs.xml");
                e.printStackTrace();
            }
        } else {
            /*
             * Else itype is (6) not 0, so lat/lon is given in raw data. Process
             * the lat/lon strings and setLat and setLong
             */
            if (matchLn.group(4).equals("S")) {
                signLl = -1f;
            } else {
                signLl = 1f;
            }
            record.setLat((Integer.parseInt(matchLn.group(2)) + (Integer
                    .parseInt(matchLn.group(3)) * .1f)) * signLl);
            if (matchLn.group(7).equals("W")) {
                signLl = -1f;
            } else {
                signLl = 1f;
            }
            record.setLon((Integer.parseInt(matchLn.group(5)) + (Integer
                    .parseInt(matchLn.group(6)) * .1f)) * signLl);
        }
        record.setDirection(Integer.parseInt(matchLn.group(2 + itype)));
        record.setDistanceNm(Integer.parseInt(matchLn.group(3 + itype))
                + (Integer.parseInt(matchLn.group(4 + itype)) * .1f));
    }
}
