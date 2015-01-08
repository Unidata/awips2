/**
 * NcPafmParser 
 * 
 * This java class intends to serve as an utility only for PAFM decoder.
 * 
 * HISTORY
 *
 * Date     	Ticket # Author	  	Description
 * ------------	-------- ---------- ---------------------------------------------
 * 08/21/09		126		 F. J. Yen  Initial creation	
 * 09/29/09		126	     F. J. Yen  Add additional precip types and hazard types.
 * 02/10/11		126		 F. J. Yen	Add WINTER WEATH, WINTER STORM, FLOOD, and
 * 									LKSHORE FLD to hazard types. Clean up.
 * 03/30/11		126		 F. J. Yen	Add BLIZZARD, FRZNG RAIN, and HARD FREEZE to
 * 									hazard types
 * 09/30/11		126		 B. Hebbard	PafmParser becomes new NcPafmParser
 * 03/24/14    1064      B. Hebbard (TTR 892) (a) update readZoneLocs() localization
 *                                  code to fix failure to access zones.xml;
 *                                  (b) when parsing ELEV (in PFM only), also get
 *                                  lat/lon, and store as point location (instead of
 *                                  using FIPS centroid as for AFM)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.edex.plugin.ncpafm.util;

import gov.noaa.nws.ncep.common.dataplugin.ncpafm.NcPafmBulletin;
import gov.noaa.nws.ncep.common.dataplugin.ncpafm.NcPafmFips;
import gov.noaa.nws.ncep.common.dataplugin.ncpafm.NcPafmParameters;
import gov.noaa.nws.ncep.common.dataplugin.ncpafm.NcPafmUgc;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField.StationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.edex.tools.decoder.MndTime;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

enum Param {
    MIN_MAX, MN_MX, MAX_MIN, MX_MN, TEMP, DEWPT, RH, WIND_CHAR, WIND_DIR, WIND_GUST, WIND_SPD, AVG_CLOUDS, CLOUDS, POP_12HR, QPF_12HR, MAX_QPF, SNOW_12HR, PWIND_DIR, RAIN, RAIN_SHWRS, SPRINKLES, TSTMS, DRIZZLE, SNOW, SNOWSHWRS, FLURRIES, SLEET, FRZG_RAIN, FRZG_DRZL, OBVIS, WIND_CHILL, HEAT_INDEX, MIN_CHILL, MAX_HEAT, BLIZZARD, COAST_FLOOD, DENSE_FOG, EXCESS_HEAT, FIRE_WEATHER, FLASH_FLOOD, FLOOD, FREEZE, FROST, FRZNG_RAIN, HARD_FREEZE, HEAT, HIGH_SURF, HIGH_WIND, LAKE_WIND, LKSHORE_FLD, SVR_TSTORM, TORNADO, WIND, WINTER_STORM, WINTER_WEATH, NOVALUE;

    static Param toParam(String paramStr) {
        try {
            return valueOf(paramStr);
        } catch (Exception ex) {
            return NOVALUE;
        }
    }
}

public class NcPafmParser {

    private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;

    private static final int IMISSD = IDecoderConstantsN.INTEGER_MISSING;

    private static StationTable zonesList;

    private static int ndxLast;

    private static final int maxNmHr = 48;

    private static String locTimeZone = " ";

    private static int[] locHrs = new int[maxNmHr];

    private static Float[] tmpfAr = new Float[maxNmHr];

    private static Float[] dwpfAr = new Float[maxNmHr];

    private static Float[] relhAr = new Float[maxNmHr];

    private static Float[] pop12Ar = new Float[maxNmHr];

    private static Float[] qpf12MnAr = new Float[maxNmHr];

    private static Float[] qpf12MxAr = new Float[maxNmHr];

    private static Float[] highestMaxQpfAr = new Float[maxNmHr];

    private static Float[] lowestMaxQpfAr = new Float[maxNmHr];

    private static Float[] snow12MnAr = new Float[maxNmHr];

    private static Float[] snow12MxAr = new Float[maxNmHr];

    private static Float[] maxHeatAr = new Float[maxNmHr];

    private static Float[] minChillAr = new Float[maxNmHr];

    private static Float[] heatIndexAr = new Float[maxNmHr];

    private static Float[] windChillAr = new Float[maxNmHr];

    private static Float[] avgMxTmpfAr = new Float[maxNmHr];

    private static Float[] hiMxTmpfAr = new Float[maxNmHr];

    private static Float[] loMxTmpfAr = new Float[maxNmHr];

    private static Float[] avgMnTmpfAr = new Float[maxNmHr];

    private static Float[] hiMnTmpfAr = new Float[maxNmHr];

    private static Float[] loMnTmpfAr = new Float[maxNmHr];

    private static String[] windDirAr = new String[maxNmHr];

    private static String[] windSmphAr = new String[maxNmHr];

    private static String[] gust_MphAr = new String[maxNmHr];

    private static String[] pwindDirAr = new String[maxNmHr];

    private static String[] windCharAr = new String[maxNmHr];

    private static String[] skyCoverAr = new String[maxNmHr];

    private static String[] avgSkyCoverAr = new String[maxNmHr];

    private static String[] obvisAr = new String[maxNmHr];

    private static String[] rainAr = new String[maxNmHr];

    private static String[] rainShwrsAr = new String[maxNmHr];

    private static String[] sprinklesAr = new String[maxNmHr];

    private static String[] tstmsAr = new String[maxNmHr];

    private static String[] drizzleAr = new String[maxNmHr];

    private static String[] snowAr = new String[maxNmHr];

    private static String[] snowShwrsAr = new String[maxNmHr];

    private static String[] flurriesAr = new String[maxNmHr];

    private static String[] sleetAr = new String[maxNmHr];

    private static String[] frzgRainAr = new String[maxNmHr];

    private static String[] frzgDrzlAr = new String[maxNmHr];

    private static String[] hazardsAr = new String[maxNmHr];

    private static Calendar[] utcDateTm = new Calendar[maxNmHr];

    /**
     * Constructor
     */
    public NcPafmParser() {
    }

    /**
     * Parse the WMO line and store WMO header, OfficeID, issue time,
     * designatorBBB,...
     * 
     * @param wmoline
     *            The bulletin message
     * 
     * @return a NcPafmBulletin
     */
    public static NcPafmBulletin processWMO(String wmoline, Calendar mndTime) {

        NcPafmBulletin record = null;
        // Regular expression for WMO/ICAO, station ID, and issue date (and
        // maybe designator BBB)
        final String WMO_EXP = "([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})( ([A-Z]{3}))?";

        // Pattern used for extracting WMO header, officeID, product purge time,
        // and issue date and designatorBBB
        final Pattern wmoPattern = Pattern.compile(WMO_EXP);
        Matcher theMatcher = wmoPattern.matcher(wmoline);

        if (theMatcher.find()) {
            record = new NcPafmBulletin();

            record.setWmoHeader(theMatcher.group(1));
            record.setIssueOffice(theMatcher.group(2));
            if (theMatcher.group(5) == null) {
                record.setDesignatorBBB(" ");
            } else {
                record.setDesignatorBBB(theMatcher.group(5));
            }

            // Decode the issue time.
            Calendar issueTime = UtilN.findDataTime(theMatcher.group(3),
                    mndTime);
            record.setIssueTime(issueTime);

            // DataTime dataTime = new DataTime(issueTime);
            // record.setDataTime(dataTime);
        }
        return record;
    }

    /**
     * Process UGC line; then process Parameters and FIPS
     * 
     * @param ugcline
     *            The UGC information
     * @param segment
     *            The entire segment of this UGC line
     * @param mndTime
     *            The Calendar date/time from MND Line
     * @return a NcPafmUgc table
     */
    public static NcPafmUgc processUgc(String ugcline, String segment,
            Calendar mndTime) {
        final String frameDelm = "\\x0d\\x0d\\x0aDATE";
        final String prependDate = "DATE";
        final Pattern datePattern = Pattern.compile(prependDate);
        int ndxFcstHr = 0;
        int frameIndex = 0;
        int maxFrame = 3;
        boolean findSegMndTime;
        String[] frame = new String[maxFrame];
        Float lat;
        Float lon;
        Float elev;
        Calendar segMndTime = null;
        Matcher segMndTimeMatcher = null;

        // Regular expression for latitude, longitude, and elevation (PFM only)
        final String LAT_LON_ELEV_RGEX = "(\\d{2}\\.\\d{2})(N|S)\\s{1,2}(\\d{2,3}\\.\\d{2})(E|W) (ELEV\\..*) (-?\\d+|\\?{4}) FT";
        final Pattern latLonElevPattern = Pattern.compile(LAT_LON_ELEV_RGEX);

        // Regular expression for MND Time
        final String SEG_MND_TIME = "(\\d{3,4}) ((AM)|(PM)|(am)|(pm)) (\\p{Alpha}{3}) "
                + "(\\p{Alpha}{3}) (\\p{Alpha}{3}) (\\d{1,2}) (\\d{4})\\r\\r\\n";
        final Pattern segMndTimePattern = Pattern.compile(SEG_MND_TIME);

        NcPafmUgc currentUgc = new NcPafmUgc();
        /*
         * Decode the point LAT/LON and elevation string (PFM product only)
         */
        Matcher latLonElevMatcher = latLonElevPattern.matcher(segment);
        if (latLonElevMatcher.find()) {
            try {
                lat = Float.parseFloat(latLonElevMatcher.group(1));
                if (latLonElevMatcher.group(2).equals("S")) {
                    lat = -lat;
                }
            } catch (Exception e) {
                e.printStackTrace();
                System.out
                        .println("PAFM WARNING:  Bad format encountered in latitude:"
                                + latLonElevMatcher.group(1)
                                + latLonElevMatcher.group(2));
                lat = RMISSD;
            }
            try {
                lon = Float.parseFloat(latLonElevMatcher.group(3));
                if (latLonElevMatcher.group(4).equals("W")) {
                    lon = -lon;
                }
            } catch (Exception e) {
                e.printStackTrace();
                System.out
                        .println("PAFM WARNING:  Bad format encountered in longitude:"
                                + latLonElevMatcher.group(3)
                                + latLonElevMatcher.group(4));
                lon = RMISSD;
            }
            if (latLonElevMatcher.group(6).equals("????")) {
                elev = RMISSD;
            } else {
                try {
                    elev = Float.parseFloat(latLonElevMatcher.group(6));
                } catch (Exception e) {
                    e.printStackTrace();
                    System.out
                            .println("PAFM WARNING:  Bad format encountered in elevation:"
                                    + latLonElevMatcher.group(6));
                    elev = RMISSD;
                }
            }
        } else {
            lat = RMISSD;
            lon = RMISSD;
            elev = RMISSD;
        }

        NcPafmParser.processFips(ugcline, lat, lon, elev, currentUgc, mndTime);
        /*
         * Break up into Matrix frames which begins with "DATE". There should be
         * 2 frames for each UGC, but sometimes there is only one. (When there
         * are more than 2 frames, then there is an invalid format, so, it is
         * ignored.)
         */
        Scanner scFrm = new Scanner(segment).useDelimiter(frameDelm);

        while (scFrm.hasNext() && (frameIndex < maxFrame)) {
            if (frameIndex == 0) {
                frame[frameIndex] = scFrm.next();
            } else {
                frame[frameIndex] = prependDate + scFrm.next();
            }
            frameIndex++;
        }
        ndxFcstHr = 0;
        findSegMndTime = false;
        for (int idx = 0; (idx < frameIndex) && (idx < maxFrame); idx++) {
            /*
             * Decode the MND Time line in the segment and convert it into a
             * Calendar object
             */
            if (idx == 0) {
                MndTime mnt = new MndTime(segment.getBytes());
                segMndTime = mnt.getMndTime();
                segMndTimeMatcher = segMndTimePattern.matcher(frame[0]);
                if (segMndTimeMatcher.find()) {
                    findSegMndTime = true;
                }
            } else {
                Matcher frameMatcher = datePattern.matcher(frame[idx]);
                if (frameMatcher.find() && findSegMndTime) {
                    /*
                     * Have a frame matrix beginning with "DATE" so process it
                     */
                    NcPafmParser.processParameters(frame[idx], ndxFcstHr,
                            currentUgc, segMndTime, segMndTimeMatcher);

                } else {
                    /*
                     * Don't have frame matrix
                     */
                    System.out.println("PAFM WARNING:  No frame matrix");
                }
            }
        }

        // Set UGC line
        currentUgc.setUgc(ugcline);
        // Replace special characters with a blank; it is more readable and set
        // the segment
        currentUgc.setSegment(UtilN.removeLeadingWhiteSpaces(segment
                .replace('\r', ' ').replace('\003', ' ').replace('\000', ' ')
                .replace('\001', ' ')));

        return currentUgc;
    }

    /**
     * Parse the parameters in the matrix
     * 
     * @param params
     *            The latlon lines
     * @param UGC
     *            The NcPafmUgc table
     * @param latlonIndex
     *            The index of Lat/Long
     * 
     */
    public static void processParameters(String matrixF, int ndxFcstHr,
            NcPafmUgc UGC, Calendar mndTime, Matcher segMndTimeMatcher) {

        final String rowDelm = "\\r\\r\\n";

        final String dateStr = "DATE  ";
        final Pattern datePattern = Pattern.compile(dateStr);
        final String dateLine = "DATE +(()|(SUN)|(MON)|(TUE)|(WED)|(THU)|(FRI)|(SAT)) +(\\d{2})/(\\d{2})((/(\\d{2}))|())";
        final Pattern dateLinePattern = Pattern.compile(dateLine,
                Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
        Matcher dateLineMatcher = null;
        // hrlyStr is the regex for lines such as EDT 3HRLY, UTC 6HRLY
        final String hrlyStr = "(\\p{Alpha}{3}) \\dHRLY";
        final Pattern hrlyStrPattern = Pattern.compile(hrlyStr);
        // fcstHr is for the 2 digit hour of the forecast (column label)
        final String fcstHr = " \\d{2}";
        final Pattern fcstHrPattern = Pattern.compile(fcstHr);
        final String qpf12Val = "( MM( |\\r))|( 0( |\\r))|( \\d{0,2}\\.\\d{2}( |\\r))|( \\d{0,2}\\.\\d{2}-\\d{0,2}\\.\\d{2}( |\\r))";
        final Pattern qpf12ValPattern = Pattern.compile(qpf12Val);
        final String maxQpf = "( MM( |\\r))|( 0( |\\r))|( \\d{0,2}\\.\\d{2}( |\\r))|( \\d{0,2}\\.\\d{2}-\\d{0,2}\\.\\d{2}( |\\r))";
        final Pattern maxQpfPattern = Pattern.compile(maxQpf);
        final String snow12Val = "( MM( |\\r))|( T( |\\r))|( 00-00( |\\r))|( \\d{1,2}( |\\r))|( \\d{1,2}-\\d{1,2}( |\\r))";
        final Pattern snow12ValPattern = Pattern.compile(snow12Val);

        int ndxHr = 0;
        int ndxHr2 = 0;
        int idxst;
        int jr, jdx, jdxsv, jdxsv2;
        int[] utcHrs = new int[maxNmHr];
        int[] utcStNdx = new int[maxNmHr];
        int[] utcEnNdx = new int[maxNmHr];
        int[] locStNdx = new int[maxNmHr];
        int[] locEnNdx = new int[maxNmHr];
        int rowIndex = 0;
        int maxRow = 80;
        String[] row = new String[maxRow];
        /*
         * Initialize the temporary storage arrays for the parameters at each
         * forecast time
         */
        initParameters();
        /*
         * Separate the records (lines) of the matrix into rows.
         */
        Scanner scRow = new Scanner(matrixF).useDelimiter(rowDelm);
        rowIndex = 0;
        while (scRow.hasNext() && (rowIndex < maxRow)) {
            row[rowIndex] = scRow.next();
            rowIndex++;
        }
        if (rowIndex > 0) {
            // Prepend row[0] with DATE: row [0] = "DATE" + row[0]);
            Matcher dateMatcher = datePattern.matcher(row[0]);
            if (dateMatcher.find()) {
                // Process date line
                dateLineMatcher = dateLinePattern.matcher(row[0]);
                if (dateLineMatcher.find()) {
                    /*
                     * 888888888888888888888888888 if (dateLineMatcher.group(14)
                     * == null) { System.out
                     * .println("  equals test group(14) year is null"); } else
                     * { if (dateLineMatcher.group(14).equals("09")) {
                     * System.out
                     * .println("  equals test group(14) year is 09"); } } 888
                     */
                    /*
                     * In the above Matcher: group(9) is the month; group(10) is
                     * the day; group(13) is the 2 digit year (all without
                     * slashes) if not year, then null
                     */
                } else {
                    /*
                     * Set rowIndex to 0, so won't process
                     */
                    rowIndex = 0;
                    System.out.println("PAFM WARNING: No DATE Line");
                }
            } else {
                /*
                 * Set rowIndex to 0, so won't process
                 */
                rowIndex = 0;
                System.out.println("PAFM WARNING: No DATE found in frame");
            }
        } else {
            System.out.println("PAFM WARNING: rowIndex is !> 0,  it is="
                    + rowIndex);
        }

        // Process the local and UTC forecast hour lines
        jr = 1;
        jdxsv = 1;
        // localZone = " ";
        locTimeZone = " ";
        for (jdx = jr; jdx < rowIndex; jdx++) {
            Matcher hrlyStrMatcher = hrlyStrPattern.matcher(row[jdx]);
            if (hrlyStrMatcher.find()) {
                jdxsv = jdx;
                ndxHr = ndxFcstHr;
                Matcher fcstHrMatcher = fcstHrPattern.matcher(row[jdx]);
                while (fcstHrMatcher.find()) {

                    if (hrlyStrMatcher.group(1).equals("UTC")) {
                        /*
                         * Get the forecast hours in UTC time. Also store the
                         * indices of the start and end of the hour string.
                         */
                        utcHrs[ndxHr] = Integer.parseInt(fcstHrMatcher.group()
                                .trim());
                        utcStNdx[ndxHr] = fcstHrMatcher.start();
                        utcEnNdx[ndxHr] = fcstHrMatcher.end();
                    } else {
                        /*
                         * Get the forecast hours in Local time. Also store the
                         * indices of the start and end of the hour string.
                         */
                        locTimeZone = hrlyStrMatcher.group(1);
                        locHrs[ndxHr] = Integer.parseInt(fcstHrMatcher.group()
                                .trim());
                        locStNdx[ndxHr] = fcstHrMatcher.start();
                        locEnNdx[ndxHr] = fcstHrMatcher.end();
                    }
                    ndxHr++;
                }
            }
        }
        if (jdxsv > 0 && jdxsv < rowIndex + 3) {
            jdxsv = jdxsv + 1;
            jdxsv2 = jdxsv;
            for (jdx = jdxsv; jdx < rowIndex; jdx++) {
                Matcher hrlyStr2Matcher = hrlyStrPattern.matcher(row[2]);
                if (hrlyStr2Matcher.find()) {
                    jdxsv2 = jdx;
                    Matcher fcstHr2Matcher = fcstHrPattern.matcher(row[2]);
                    ndxHr2 = ndxFcstHr;
                    while (fcstHr2Matcher.find()) {

                        if (hrlyStr2Matcher.group(1).equals("UTC")) {

                            utcHrs[ndxHr2] = Integer.parseInt(fcstHr2Matcher
                                    .group().trim());
                            utcStNdx[ndxHr2] = fcstHr2Matcher.start();
                            utcEnNdx[ndxHr2] = fcstHr2Matcher.end();
                        } else {
                            locTimeZone = hrlyStr2Matcher.group(1);
                            locHrs[ndxHr2] = Integer.parseInt(fcstHr2Matcher
                                    .group().trim());
                            locStNdx[ndxHr2] = fcstHr2Matcher.start();
                            locEnNdx[ndxHr2] = fcstHr2Matcher.end();
                        }
                        ndxHr2++;
                    }
                }
            }

            if (jdxsv2 > 1 && jdxsv2 < rowIndex + 2) {
                /*
                 * Process date/time from DATE and HRLY lines
                 */
                ndxLast = Math.max(ndxHr, ndxHr2);
                if (ndxHr != ndxHr2) {
                    System.out
                            .println("PAFM WARNING: ndxHr NOT EQUAL ndxHr2 use min");
                    ndxLast = Math.min(ndxHr, ndxHr2);
                }

                processDateLineTime(dateLineMatcher, mndTime,
                        segMndTimeMatcher, ndxLast, locStNdx, locHrs,
                        locTimeZone);

                // Process the parameter rows
                idxst = ndxFcstHr;
                for (int idx = 3; idx < rowIndex; idx++) {

                    if (row[idx].length() > 13) {
                        String parmStr = row[idx].substring(0, 12).trim()
                                .replace(" ", "_").replace("/", "_");

                        switch (Param.toParam(parmStr)) {
                        /*
                         * The following for single floating point values
                         */
                        case DEWPT:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, dwpfAr);
                            break;
                        case HEAT_INDEX:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, heatIndexAr);
                            break;
                        case MAX_HEAT:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, maxHeatAr);
                            break;
                        case MIN_CHILL:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, minChillAr);
                            break;
                        case POP_12HR:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, pop12Ar);
                            break;
                        case RH:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, relhAr);
                            break;
                        case TEMP:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, tmpfAr);
                            break;
                        case WIND_CHILL:
                            NcPafmParser.parseFlParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, windChillAr);
                            break;
                        /*
                         * The following for a value of MM, 0, a single floating
                         * point value, and floating point ranges
                         */
                        case MAX_QPF:
                            NcPafmParser.parseFlRangeParmRow(row[idx],
                                    maxQpfPattern, idxst, ndxLast, locStNdx,
                                    locEnNdx, highestMaxQpfAr, lowestMaxQpfAr);
                            break;
                        case QPF_12HR:
                            NcPafmParser.parseFlRangeParmRow(row[idx],
                                    qpf12ValPattern, idxst, ndxLast, locStNdx,
                                    locEnNdx, qpf12MnAr, qpf12MxAr);
                            break;
                        /*
                         * The following for a single floating point value or a
                         * group of three floating point values.
                         */
                        case MAX_MIN:
                        case MIN_MAX:
                        case MX_MN:
                        case MN_MX:
                            NcPafmParser.parseMaxMinParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, locHrs);
                            break;
                        /*
                         * The following for strings (MM, T, d, dd, dd-dd,
                         * 00-00) with possible range
                         */
                        case SNOW_12HR:
                            NcPafmParser.parseSnowRangeParmRow(row[idx],
                                    snow12ValPattern, idxst, ndxLast, locStNdx,
                                    locEnNdx, snow12MnAr, snow12MxAr);
                            break;
                        /*
                         * The following for strings only
                         */
                        case AVG_CLOUDS:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, avgSkyCoverAr);
                            break;
                        case CLOUDS:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, skyCoverAr);
                            break;
                        case DRIZZLE:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, drizzleAr);
                            break;
                        case FLURRIES:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, flurriesAr);
                            break;
                        case FRZG_DRZL:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, frzgDrzlAr);
                            break;
                        case FRZG_RAIN:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, frzgRainAr);
                            break;
                        case OBVIS:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, obvisAr);
                            break;
                        case PWIND_DIR:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, pwindDirAr);
                            break;
                        case RAIN:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, rainAr);
                            break;
                        case RAIN_SHWRS:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, rainShwrsAr);
                            break;
                        case SLEET:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, sleetAr);
                            break;
                        case SNOW:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, snowAr);
                            break;
                        case SNOWSHWRS:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, snowShwrsAr);
                            break;
                        case SPRINKLES:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, sprinklesAr);
                            break;
                        case TSTMS:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, tstmsAr);
                            break;
                        case WIND_CHAR:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, windCharAr);
                            break;
                        case WIND_DIR:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, windDirAr);
                            break;
                        case WIND_GUST:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, gust_MphAr);
                            break;
                        case WIND_SPD:
                            NcPafmParser.parseStrParmRow(row[idx], idxst,
                                    ndxLast, locStNdx, locEnNdx, windSmphAr);
                            break;
                        /*
                         * The following are currently known Watch, Warning, and
                         * Advisory phenomena (hazards) when Valid Time Event
                         * Code (VTEC) becomes available and included when a
                         * valid Watch, Warning and/or Advisory is issued by a
                         * WFO. Although PAFM raw data do not have VTECs, the
                         * phenomena appear in the left column of the PAFM
                         * matrix. It is possible that there are phenomena that
                         * are not listed here because they have not been seen
                         * in a PAFM raw data and so it is not known what they
                         * are and/or what the actual text string would be. In
                         * such an instance, a warning message would appear that
                         * the "Parameter is currently not known:  ". If the
                         * parameter is a Watch, Warning, or Advisory phenomena,
                         * then it can be added below and in the enum Param.
                         */
                        case BLIZZARD:
                        case COAST_FLOOD:
                        case DENSE_FOG:
                        case EXCESS_HEAT:
                        case FIRE_WEATHER:
                        case FLASH_FLOOD:
                        case FLOOD:
                        case FREEZE:
                        case FROST:
                        case FRZNG_RAIN:
                        case HARD_FREEZE:
                        case HEAT:
                        case HIGH_SURF:
                        case HIGH_WIND:
                        case LAKE_WIND:
                        case LKSHORE_FLD:
                        case SVR_TSTORM:
                        case TORNADO:
                        case WIND:
                        case WINTER_STORM:
                        case WINTER_WEATH:
                            NcPafmParser.parseHazardParmRow(row[idx], parmStr,
                                    idxst, ndxLast, locStNdx, locEnNdx);
                            break;
                        case NOVALUE:
                            System.out
                                    .println("PAFM WARNING:  Parameter is currently not known: "
                                            + row[idx]);
                            break;
                        default:
                            System.out
                                    .println("PAFM WARNING: Switch case DEFAULT: parmStr="
                                            + parmStr);
                        }
                    }
                }
            }
            /*
             * Add current parameters to set
             */
            addCurrParameters(UGC);
        }
    }

    /**
     * Parse the values in the row of the matrix having floating point values
     * and range data (with "-" separating the beginning and end of range
     * values).
     * 
     * @param parmRow
     *            The row of the matrix
     * @param flRangePattern
     *            Regex pattern for a value of the field
     * @param ndxSt
     *            Starting index
     * @param ndxLast
     *            Ending index
     * @param locStNdx
     *            Array of the starting positions of the fields
     * @param locEnNdx
     *            Array of the ending positions of the fields
     * @param flRangeBegAr
     *            Array of the beginning range values of the fields to be
     *            returned
     * @param flRangeEndAr
     *            Array of the ending range values of the fields to be returned
     */
    public static void parseFlRangeParmRow(String parmRow,
            Pattern flRangePattern, int ndxSt, int ndxLast, int[] locStNdx,
            int[] locEnNdx, Float[] flRangeBegAr, Float[] flRangeEndAr) {
        int jx, kx, idxDash;
        Matcher flRangeMatcher = flRangePattern.matcher(parmRow + "\r\r\n");
        while (flRangeMatcher.find()) {
            /*
             * Determine the row index to store the field based on the string
             * being right-justified.
             */
            jx = -1;
            for (kx = ndxSt; kx < ndxLast; kx++) {
                if (locEnNdx[kx] == flRangeMatcher.end() - 1) {
                    /*
                     * Found index of the forecast hour
                     */
                    jx = kx;
                    break;
                }
            }
            if (jx != -1) {
                if (flRangeMatcher.group().trim().equals("MM")) {
                    flRangeBegAr[jx] = RMISSD;
                    flRangeEndAr[jx] = RMISSD;
                } else {
                    if (flRangeMatcher.group().trim().equals("0")) {
                        flRangeBegAr[jx] = 0.f;
                        flRangeEndAr[jx] = 0.f;
                    } else {
                        idxDash = flRangeMatcher.group().indexOf("-");
                        if (idxDash == -1) {
                            /*
                             * Single floating point value
                             */
                            flRangeBegAr[jx] = Float.parseFloat(flRangeMatcher
                                    .group());
                            flRangeEndAr[jx] = flRangeBegAr[jx];
                        } else {
                            /*
                             * Floating point range.
                             */
                            flRangeBegAr[jx] = Float.parseFloat(flRangeMatcher
                                    .group().substring(0, idxDash).trim());
                            flRangeEndAr[jx] = Float.parseFloat(flRangeMatcher
                                    .group()
                                    .substring(idxDash + 1,
                                            flRangeMatcher.group().length())
                                    .trim());
                        }
                    }
                }
            } else {
                /*
                 * Problem with finding the forecast hour it is right-justified
                 * to.
                 */
                System.out
                        .println("PAFM WARNING:  Problem finding the forecast hour in "
                                + "parseFlRangeParmRow for:" + parmRow);
            }
        }
    }

    /**
     * Parse the values in the row of the matrix having floating point values.
     * 
     * @param parmRow
     *            The row of the matrix
     * @param flRangePattern
     *            Regex pattern for a value of the field
     * @param ndxSt
     *            Starting index
     * @param ndxLast
     *            Ending index
     * @param locStNdx
     *            Array of the starting positions of the fields
     * @param locEnNdx
     *            Array of the ending positions of the fields
     * @param flParmAr
     *            Array of the values of the floating point fields to be
     *            returned
     */
    public static void parseFlParmRow(String parmRow, int ndxSt, int ndxLast,
            int[] locStNdx, int[] locEnNdx, Float[] flParmAr) {

        final String floatVal = "(( |1|-)\\d{2})|((  )|( -)\\d{1})";
        final Pattern floatValPattern = Pattern.compile(floatVal);
        String parmValStr;
        int ndxEn = Math.min(ndxLast, maxNmHr);
        int mdxEn = Math.max(ndxLast, maxNmHr);
        boolean goodFormat, badLenFlag;
        goodFormat = true;
        badLenFlag = false;
        for (int idx = ndxSt; idx < ndxEn && goodFormat && !badLenFlag; idx++) {
            if (parmRow.length() >= locEnNdx[idx]) {
                try {
                    parmValStr = parmRow
                            .substring(locStNdx[idx], locEnNdx[idx]);
                    if (parmValStr.equals("   ")
                            || parmValStr.trim().equals("MM")) {
                        flParmAr[idx] = RMISSD;
                    } else {
                        Matcher floatValMatcher = floatValPattern
                                .matcher(parmValStr);
                        if (floatValMatcher.find()) {
                            try {
                                flParmAr[idx] = Float.parseFloat(parmValStr
                                        .trim());
                            } catch (Exception e) {
                                goodFormat = false;
                                e.printStackTrace();
                                System.out
                                        .println("PAFM WARNING:  Bad format encountered in row="
                                                + parmRow
                                                + "\n    For parmValStr="
                                                + parmValStr);
                                return;
                            }
                        } else {
                            /*
                             * Bad value--could possibly be bad format for row.
                             * Set value to missing and print out warning
                             * message.
                             */
                            // flParmAr[idx] = RMISSD;
                            System.out
                                    .println("PAFM WARNING:  Bad format encountered for index ="
                                            + idx
                                            + " value="
                                            + parmValStr
                                            + "  bad row=" + parmRow);
                            goodFormat = false;
                            return;
                        }
                    }
                } catch (Exception e) {
                    goodFormat = false;
                    e.printStackTrace();
                    System.out
                            .println("PAFM WARNING:  Bad format encountered in row="
                                    + parmRow
                                    + "\n    For index idx="
                                    + idx
                                    + " starting at >>"
                                    + parmRow.substring(locStNdx[idx]) + "<<");
                    return;
                }
            } else {
                badLenFlag = true;
                System.out
                        .println("PAFM WARNING:  Row length does not agree with Date line: "
                                + parmRow);
            }
        }
        if (mdxEn > ndxEn) {
            for (int idx = ndxEn; idx < mdxEn; idx++) {
                flParmAr[idx] = RMISSD;
            }
        }
        return;
    }

    /**
     * Parse the values in the row of the matrix having floating point values
     * and range data in groups of threes.
     * 
     * @param parmRow
     *            The row of the matrix
     * @param flRangePattern
     *            Regex pattern for a value of the field
     * @param ndxSt
     *            Starting index
     * @param ndxLast
     *            Ending index
     * @param locStNdx
     *            Array of the starting positions of the fields
     * @param locEnNdx
     *            Array of the ending positions of the fields
     * @param flRangeBegAr
     *            Array of the beginning range values of the fields to be
     *            returned
     * @param flRangeEndAr
     *            Array of the ending range values of the fields to be returned
     */
    public static void parseMaxMinParmRow(String parmRow, int ndxSt,
            int ndxLast, int[] locStNdx, int[] locEnNdx, int[] locHrs) {
        final String floatVal = "((1|-)\\d{2})|( ((\\d{2})|( \\d{1})|(-\\d{1}))|( MM)|(   ))";
        final Pattern floatValPattern = Pattern.compile(floatVal);
        Float flval;
        int numfl = 0;
        String parmValStr;
        int ndxEn = Math.min(ndxLast, maxNmHr);
        boolean badLenFlag;
        badLenFlag = false;
        numfl = 0;
        /*
         * Find floating point values starting from end of row
         */
        for (int idx = ndxEn - 1; idx >= ndxSt && !badLenFlag; idx--) {
            if (parmRow.length() >= locEnNdx[idx]) {
                parmValStr = parmRow.substring(locStNdx[idx], locEnNdx[idx]);
                Matcher floatValMatcher = floatValPattern.matcher(parmValStr);
                if (floatValMatcher.find()) {
                    switch (numfl) {
                    case 0:
                        if (parmValStr.equals("   ")
                                || parmValStr.trim().equals("MM")) {
                            avgMxTmpfAr[idx] = RMISSD;
                            avgMnTmpfAr[idx] = RMISSD;
                            numfl = 0;
                        } else {
                            try {
                                flval = Float.parseFloat(parmValStr.trim());
                            } catch (Exception e) {
                                e.printStackTrace();
                                System.out
                                        .println("PAFM WARNING:  Bad format encountered in row="
                                                + parmRow
                                                + "\n    For parmValStr="
                                                + parmValStr);
                                return;
                            }
                            numfl = 1;
                            /*
                             * Determine if Max or Min
                             */
                            if (locHrs[idx] > 14 && locHrs[idx] < 23) {
                                avgMxTmpfAr[idx] = flval;
                                avgMnTmpfAr[idx] = RMISSD;
                            } else {
                                avgMxTmpfAr[idx] = RMISSD;
                                avgMnTmpfAr[idx] = flval;
                            }
                        }
                        break;
                    case 1:
                        if (parmValStr.equals("   ")
                                || parmValStr.trim().equals("MM")) {
                            numfl = 0;
                        } else {
                            try {
                                flval = Float.parseFloat(parmValStr.trim());
                            } catch (Exception e) {
                                e.printStackTrace();
                                System.out
                                        .println("PAFM WARNING:  Bad format encountered in row="
                                                + parmRow
                                                + "\n    For parmValStr="
                                                + parmValStr);
                                return;
                            }
                            numfl = 2;
                            /*
                             * Determine if Max or Min and overwrite previous
                             * value stored in array (in case 0) since there is
                             * a second value in a range. (The middle value is
                             * used in a range of values)
                             */
                            if (locHrs[idx + 1] > 14 && locHrs[idx + 1] < 23) {
                                hiMxTmpfAr[idx + 1] = avgMxTmpfAr[idx + 1];
                                avgMxTmpfAr[idx + 1] = flval;
                                hiMnTmpfAr[idx + 1] = RMISSD;
                                avgMnTmpfAr[idx + 1] = RMISSD;
                            } else {
                                hiMxTmpfAr[idx + 1] = RMISSD;
                                avgMxTmpfAr[idx + 1] = RMISSD;
                                hiMnTmpfAr[idx + 1] = avgMnTmpfAr[idx + 1];
                                avgMnTmpfAr[idx + 1] = flval;
                            }
                        }
                        break;
                    case 2:
                        if (parmValStr.equals("   ")
                                || parmValStr.trim().equals("MM")) {
                            numfl = 0;
                            /*
                             * Only two values in group, so use the second value
                             * that was already stored in case 1.
                             */
                            System.out
                                    .println("PAFM WARNING:  case 2 Invalid format encountered for index ="
                                            + idx
                                            + " value=>"
                                            + parmValStr
                                            + "<  bad row=" + parmRow);
                        } else {
                            numfl = 0;
                            try {
                                flval = Float.parseFloat(parmValStr.trim());
                            } catch (Exception e) {
                                e.printStackTrace();
                                System.out
                                        .println("PAFM WARNING:  Bad format encountered in row="
                                                + parmRow
                                                + "\n    For parmValStr="
                                                + parmValStr);
                                return;
                            }
                            if (locHrs[idx + 1] > 14 && locHrs[idx + 1] < 23) {
                                loMxTmpfAr[idx + 2] = flval;
                                loMnTmpfAr[idx + 2] = RMISSD;
                            } else {
                                loMxTmpfAr[idx + 2] = RMISSD;
                                loMnTmpfAr[idx + 2] = flval;
                            }
                        }
                        break;
                    }

                } else {
                    /*
                     * Bad value--could possibly be bad format for row. Set
                     * value to missing and print out warning message.
                     */
                    System.out
                            .println("PAFM WARNING:  No Match found; Invalid format encountered for index ="
                                    + idx
                                    + " value=>"
                                    + parmValStr
                                    + "<  bad row=" + parmRow);
                }
            } else {
                badLenFlag = true;
                System.out
                        .println("PAFM WARNING:  Row length does not agree with Date line: "
                                + parmRow);
            }
        }
    }

    /**
     * Parse the values in the row of the matrix having floating point values
     * and range snow data
     * 
     * @param parmRow
     *            The row of the matrix
     * @param snowRangePattern
     *            Regex pattern for a value of the field
     * @param ndxSt
     *            Starting index
     * @param ndxLast
     *            Ending index
     * @param locStNdx
     *            Array of the starting positions of the fields
     * @param locEnNdx
     *            Array of the ending positions of the fields
     * @param snowRangeBegAr
     *            Array of the beginning range snow values to be returned
     * @param snowRangeEndAr
     *            Array of the ending range snow values to be returned
     */
    public static void parseSnowRangeParmRow(String parmRow,
            Pattern snowRangePattern, int ndxSt, int ndxLast, int[] locStNdx,
            int[] locEnNdx, Float[] snowRangeBegAr, Float[] snowRangeEndAr) {
        int jx, kx, idxDash;

        Matcher snowRangeMatcher = snowRangePattern.matcher(parmRow + "\r\r\n");
        while (snowRangeMatcher.find()) {
            /*
             * Determine the row index to store the field based on the string
             * being right-justified.
             */
            jx = -1;
            for (kx = ndxSt; kx < ndxLast; kx++) {
                if (locEnNdx[kx] == snowRangeMatcher.end() - 1) {
                    /*
                     * Found index of the forecast hour
                     */
                    jx = kx;
                    break;
                }
            }
            if (jx != -1) {
                if (snowRangeMatcher.group().trim().equals("MM")) {
                    snowRangeBegAr[jx] = RMISSD;
                    snowRangeEndAr[jx] = RMISSD;
                } else if (snowRangeMatcher.group().trim().equals("T")) {
                    snowRangeBegAr[jx] = .001f;
                    snowRangeEndAr[jx] = .001f;
                } else if (snowRangeMatcher.group().trim().equals("00-00")) {
                    snowRangeBegAr[jx] = 0.f;
                    snowRangeEndAr[jx] = 0.f;
                } else {
                    idxDash = snowRangeMatcher.group().indexOf("-");
                    if (idxDash == -1) {
                        /*
                         * Single numeric value
                         */
                        try {
                            snowRangeBegAr[jx] = Float
                                    .parseFloat(snowRangeMatcher.group());
                            snowRangeEndAr[jx] = snowRangeBegAr[jx];
                        } catch (Exception e) {
                            e.printStackTrace();
                            System.out
                                    .println("PAFM WARNING:  Bad format encountered in row="
                                            + parmRow
                                            + "\n    For "
                                            + snowRangeMatcher.group());
                            return;
                        }

                    } else {
                        /*
                         * numeric range.
                         */
                        try {
                            snowRangeBegAr[jx] = Float
                                    .parseFloat(snowRangeMatcher.group()
                                            .substring(0, idxDash).trim());
                            snowRangeEndAr[jx] = Float
                                    .parseFloat(snowRangeMatcher
                                            .group()
                                            .substring(
                                                    idxDash + 1,
                                                    snowRangeMatcher.group()
                                                            .length()).trim());
                        } catch (Exception e) {
                            e.printStackTrace();
                            System.out
                                    .println("PAFM WARNING:  Bad format encountered in row="
                                            + parmRow
                                            + "\n    For "
                                            + snowRangeMatcher.group());
                            return;
                        }
                    }
                }
            } else {
                /*
                 * Problem with finding the forecast hour that it is
                 * right-justified to.
                 */
                System.out
                        .println("PAFM WARNING:  Problem finding the forecast "
                                + "hour in parseRangeStrParmRow for:" + parmRow);
            }
        }
    }

    /**
     * Parse the values in the row of the matrix having string type data.
     * 
     * @param parmRow
     *            The row of the matrix
     * @param flRangePattern
     *            Regex pattern for a value of the field
     * @param ndxSt
     *            Starting index
     * @param ndxLast
     *            Ending index
     * @param locStNdx
     *            Array of the starting positions of the fields
     * @param locEnNdx
     *            Array of the ending positions of the fields
     * @param strParmAr
     *            Array of the string data to be returned
     */
    public static void parseStrParmRow(String parmRow, int ndxSt, int ndxLast,
            int[] locStNdx, int[] locEnNdx, String[] strParmAr) {
        String parmValStr;
        boolean badLenFlag;
        int ndxEn = Math.min(ndxLast, maxNmHr);
        int mdxEn = Math.max(ndxLast, maxNmHr);
        badLenFlag = false;
        for (int idx = ndxSt; idx < ndxEn && !badLenFlag; idx++) {
            if (parmRow.length() >= locEnNdx[idx]) {
                parmValStr = parmRow.substring(locStNdx[idx], locEnNdx[idx]);
                strParmAr[idx] = parmValStr;
            } else {
                badLenFlag = true;
                System.out.println("PAFM WARNING in parseStrParmRow:  Row "
                        + "length does not agree with Date line: " + parmRow);
            }
        }
        if (mdxEn > ndxEn) {
            for (int idx = ndxEn; idx < mdxEn; idx++) {
                strParmAr[idx] = "   ";
            }
        }
    }

    /**
     * Decode the county FIPS, and find the production purge date.
     * 
     * @param ugc
     *            The UGC line which contains FIPS
     * @param lat
     *            The specific point latitude, if specified in (PFM) bulletin
     * @param lon
     *            The specific point longitude, if specified in (PFM) bulletin
     * @param elev
     *            The specific point elevation, if specified in (PFM) bulletin
     * @param UGC
     *            The NcPafmUgc table
     * @param mndTIme
     *            The calendar from MND remark
     */
    public static void processFips(String ugc, Float lat, Float lon,
            Float elev, NcPafmUgc UGC, Calendar mndTime) {

        Calendar purgeDate = null;
        String countyFips = null;
        String county = null;
        String fipsToken = null;

        final String delim = "-,\n";
        final String inclusiveDelim = ">";
        boolean first = true;

        /*
         * Here are many possible cases: 1. PAC055-057-140130- 2.
         * ILZ027>031-036>038-040>045-047>054-061-141015- 3.
         * LAC001-003-009-011-019-023-039-045-053-055-079-097-099-101-113-^M
         * 115-131500-^M 4. ILC129-221523- 5.
         * LCZ422-423-460-LEZ444-LHZ421-422-441-442-443-462-463-464-^M 191000-^M
         * 6. ILZ000-INZ000-MIZ000-LHZ000-LMZ000-190400-^M 7.
         * KYC001-003-005-009-015-017-021-023-027-029-031-037-041-045-049-^M
         * 053-057-061-067-073-077-079-081-085-087-093-097-099-103-111-113-^M
         * 117-123-137-141-151-155-161-163-167-169-171-179-181-185-187-191-^M
         * 201-207-209-211-213-215-217-223-227-229-239-190900-^M
         */

        StringTokenizer fipsTokens = new StringTokenizer(ugc, delim);

        while (fipsTokens.hasMoreTokens()) {

            fipsToken = fipsTokens.nextToken();

            // Get the county FIPS.
            if (first) {
                first = false;
                countyFips = fipsToken;
                county = fipsToken.substring(0, 3);

                if (fipsToken.length() == 6) {

                    // Format in NAMDDD
                    NcPafmFips currentFips = NcPafmFips.setFIPS(countyFips);
                    currentFips.setElev(elev);
                    // Search for latitude and longitude in zones.xml and set it
                    Station zone = zonesList.getStation(StationField.STID,
                            countyFips);
                    if (zone != null) {
                        currentFips.setLat(zone.getLatitude());
                        currentFips.setLon(zone.getLongitude());
                    }
                    // If meaningful lat/lon passed in -- specific to a point --
                    // use that to override more general area (FIPS) location
                    if (lat != null && lon != null && !lat.equals(RMISSD)
                            && !lon.equals(RMISSD)) {
                        currentFips.setLat(lat);
                        currentFips.setLon(lon);
                    }
                    UGC.addPafmFIPS(currentFips);
                } else if (fipsToken.length() == 10) {
                    String intervalToken = fipsToken.substring(3, 10);

                    // Format in NAMDDD1>DDD2
                    StringTokenizer twoTokens = new StringTokenizer(
                            intervalToken, inclusiveDelim);
                    String firstToken = twoTokens.nextToken();
                    String secondToken = twoTokens.nextToken();

                    Integer countyBegin = Integer.parseInt(firstToken);
                    Integer countyEnd = Integer.parseInt(secondToken);

                    for (int counter = countyBegin; counter <= countyEnd; counter++) {

                        String inclusiveToken = Integer.toString(counter);

                        // set "1" to "001" ...etc
                        if (counter < 10) {
                            inclusiveToken = "00".concat(inclusiveToken);
                        }

                        // set "10" to "010" ...etc
                        else if (counter < 100) {
                            inclusiveToken = "0".concat(inclusiveToken);
                        }
                        countyFips = county.concat(inclusiveToken);

                        NcPafmFips currentFips = NcPafmFips.setFIPS(countyFips);
                        currentFips.setElev(elev);
                        // Search for latitude and longitude in zones.xml and
                        // set it
                        Station zone = zonesList.getStation(StationField.STID,
                                countyFips);
                        if (zone != null) {
                            currentFips.setLat(zone.getLatitude());
                            currentFips.setLon(zone.getLongitude());
                        }
                        UGC.addPafmFIPS(currentFips);
                    }
                }

            } else if (fipsToken.length() == 3
                    && Character.isDigit(fipsToken.toCharArray()[0])) {

                // A continuation of previous county FIPS with format DDD
                countyFips = county.concat(fipsToken);

                NcPafmFips currentFips = NcPafmFips.setFIPS(countyFips);
                currentFips.setElev(elev);
                // Search for latitude and longitude in zones.xml and set it
                Station zone = zonesList.getStation(StationField.STID,
                        countyFips);
                if (zone != null) {
                    currentFips.setLat(zone.getLatitude());
                    currentFips.setLon(zone.getLongitude());
                }
                UGC.addPafmFIPS(currentFips);
            } else if (fipsToken.length() == 7
                    && Character.isDigit(fipsToken.toCharArray()[0])) {

                // A continuation of previous county FIPS with format DDD1>DDD2
                StringTokenizer twoTokens = new StringTokenizer(fipsToken,
                        inclusiveDelim);
                String firstToken = twoTokens.nextToken();
                String secondToken = twoTokens.nextToken();

                Integer countyBegin = Integer.parseInt(firstToken);
                Integer countyEnd = Integer.parseInt(secondToken);

                for (int counter = countyBegin; counter <= countyEnd; counter++) {

                    String inclusiveToken = Integer.toString(counter);

                    // set "1" to "001" ...etc
                    if (counter < 10) {
                        inclusiveToken = "00".concat(inclusiveToken);
                    }

                    // set "10" to "010" ...etc
                    else if (counter < 100) {
                        inclusiveToken = "0".concat(inclusiveToken);
                    }
                    countyFips = county.concat(inclusiveToken);

                    NcPafmFips currentFips = NcPafmFips.setFIPS(countyFips);
                    currentFips.setElev(elev);
                    // Search for latitude and longitude in zones.xml and set it
                    Station zone = zonesList.getStation(StationField.STID,
                            countyFips);
                    if (zone != null) {
                        currentFips.setLat(zone.getLatitude());
                        currentFips.setLon(zone.getLongitude());
                    }
                    UGC.addPafmFIPS(currentFips);
                }
            } else if (fipsToken.length() == 6
                    && Character.isLetter(fipsToken.toCharArray()[0])) {

                // A brand new county FIPS with format NAMDDD
                countyFips = fipsToken;
                county = fipsToken.substring(0, 3);

                NcPafmFips currentFips = NcPafmFips.setFIPS(countyFips);
                currentFips.setElev(elev);
                // Search for latitude and longitude in zones.xml and set it
                Station zone = zonesList.getStation(StationField.STID,
                        countyFips);
                if (zone != null) {
                    currentFips.setLat(zone.getLatitude());
                    currentFips.setLon(zone.getLongitude());
                }
                UGC.addPafmFIPS(currentFips);
            } else if (fipsToken.length() == 6
                    && Character.isDigit(fipsToken.toCharArray()[0])) {

                // The last item is the UGC product purge time
                purgeDate = UtilN.findDataTime(fipsToken, mndTime);
                UGC.setProdPurgeTime(purgeDate);
            }
        }
    }

    /**
     * processDateLineTime: Process the DATE and HRLY lines date/time groups and
     * determine the date and time for the forecast hours and store in database
     * 
     * @param dateLineMatcher
     *            Matcher for the DATE line
     * @param mndTime
     *            The Calendar date/time from the segment MND line
     * @param segMndTimeMatcher
     *            Matcher for the MND date/time line
     * @param ndxLast
     *            Last index
     * @param locHrs
     *            Array of local hours from the 6HRLY or 3HRLY line
     * @param locTimeZone
     *            Local Time Zone
     */
    private static void processDateLineTime(Matcher dateLineMatcher,
            Calendar mndTime, Matcher segMndTimeMatcher, int ndxLast,
            int[] locStNdx, int[] locHrs, String locTimeZone) {
        SimpleDateFormat sdf;
        String strNewDateSdf = " ";
        String strYear, strMonth, strDay;
        String locHrStr;
        int year;
        int diffHrs;
        strMonth = dateLineMatcher.group(10);
        strDay = dateLineMatcher.group(11);

        Calendar newDate = Calendar.getInstance();
        sdf = new SimpleDateFormat("HHmm zzz MM dd yyyy");
        /*
         * Check if there is a year given in the first date of the DATE line
         */
        if (dateLineMatcher.group(14) == null) {
            /*
             * No year given in the first date of DATE line. So, use the date in
             * the MND to determine the year. Check for end of year and
             * beginning of year to adjust year from the MND date line if
             * necessary.
             * 
             * Integer.parseInt(dateLineMatcher.group(10)) is the month.
             * Integer.parseInt(dateLineMatcher.group(11)) is the day.
             */
            if (dateLineMatcher.group(11).equals("31")
                    && dateLineMatcher.group(10).equals("12")) {
                if (segMndTimeMatcher.group(9).equals("JAN")
                        | segMndTimeMatcher.group(9).equals("Jan")) {
                    year = Integer.parseInt(segMndTimeMatcher.group(11)) - 1;
                } else {
                    year = Integer.parseInt(segMndTimeMatcher.group(11));
                }
            } else if (dateLineMatcher.group(11).equals("01")
                    && dateLineMatcher.group(10).equals("01")) {
                if (segMndTimeMatcher.group(9).equals("DEC")
                        | segMndTimeMatcher.group(9).equals("Dec")) {
                    year = Integer.parseInt(segMndTimeMatcher.group(11)) + 1;
                } else {
                    year = Integer.parseInt(segMndTimeMatcher.group(11));
                }
            } else {
                year = Integer.parseInt(segMndTimeMatcher.group(11));
            }

        } else {
            /*
             * Use the date in the first date of the DATE line. group(14) is the
             * 2 digit year.
             */
            year = Integer.parseInt(dateLineMatcher.group(14)) + 2000;
        }
        strYear = Integer.toString(year);

        /*
         * Set Calendar year, month, and day of month for temporary Calendar
         * newDate to be stored in array utcDateTm
         * 
         * Use the hour from the local hr (3HRLY or 6HRLY) line which is in
         * array locHrsl
         */
        locHrStr = Integer.toString(locHrs[0]);
        if (locHrs[0] < 10) {
            locHrStr = "0" + locHrStr;
        }
        strNewDateSdf = locHrStr + "00 " + locTimeZone + " " + strMonth + " "
                + strDay + " " + strYear;
        try {
            newDate.setTime(sdf.parse(strNewDateSdf));
        } catch (Exception e) {
            System.out
                    .println("PAFM Exception in processing date from DATE line");
        }

        utcDateTm[0].set(Calendar.YEAR, newDate.get(Calendar.YEAR));
        utcDateTm[0].set(Calendar.MONTH, newDate.get(Calendar.MONTH));
        utcDateTm[0].set(Calendar.DAY_OF_MONTH,
                newDate.get(Calendar.DAY_OF_MONTH));
        utcDateTm[0].set(Calendar.HOUR_OF_DAY,
                newDate.get(Calendar.HOUR_OF_DAY));
        utcDateTm[0].set(Calendar.MINUTE, newDate.get(Calendar.MINUTE));
        utcDateTm[0].set(Calendar.SECOND, 0);
        utcDateTm[0].set(Calendar.MILLISECOND, 0);

        /*
         * Check for case when the local hour is not directly under a a date in
         * the DATE line. This occurs when there is only one hour for a
         * particular day. If this is the case, adjust the day if necessary.
         */
        if ((locStNdx[1] - locStNdx[0]) > 3) {
            if (locHrs[0] != 0) {
                newDate.add(Calendar.DAY_OF_MONTH, -1);
            }
        }
        for (int nx = 1; nx < ndxLast; nx++) {
            diffHrs = locHrs[nx] - locHrs[nx - 1];
            if (diffHrs < 0)
                diffHrs = diffHrs + 24;

            newDate.add(Calendar.HOUR_OF_DAY, diffHrs);

            utcDateTm[nx].set(Calendar.YEAR, newDate.get(Calendar.YEAR));
            utcDateTm[nx].set(Calendar.MONTH, newDate.get(Calendar.MONTH));
            utcDateTm[nx].set(Calendar.DAY_OF_MONTH,
                    newDate.get(Calendar.DAY_OF_MONTH));
            utcDateTm[nx].set(Calendar.HOUR_OF_DAY,
                    newDate.get(Calendar.HOUR_OF_DAY));
            utcDateTm[nx].set(Calendar.MINUTE, newDate.get(Calendar.MINUTE));
            utcDateTm[nx].set(Calendar.SECOND, 0);
            utcDateTm[nx].set(Calendar.MILLISECOND, 0);
        }
    }

    /**
     * intParameters initializes the temporary parameter arrays used for storing
     * row data
     */
    public static void initParameters() {

        for (int ix = 0; ix < maxNmHr; ix++) {

            tmpfAr[ix] = RMISSD;
            dwpfAr[ix] = RMISSD;
            relhAr[ix] = RMISSD;
            pop12Ar[ix] = RMISSD;
            qpf12MnAr[ix] = RMISSD;
            qpf12MxAr[ix] = RMISSD;
            highestMaxQpfAr[ix] = RMISSD;
            lowestMaxQpfAr[ix] = RMISSD;
            snow12MnAr[ix] = RMISSD;
            snow12MxAr[ix] = RMISSD;
            maxHeatAr[ix] = RMISSD;
            minChillAr[ix] = RMISSD;
            heatIndexAr[ix] = RMISSD;
            windChillAr[ix] = RMISSD;
            avgMxTmpfAr[ix] = RMISSD;
            hiMxTmpfAr[ix] = RMISSD;
            loMxTmpfAr[ix] = RMISSD;
            avgMnTmpfAr[ix] = RMISSD;
            hiMnTmpfAr[ix] = RMISSD;
            loMnTmpfAr[ix] = RMISSD;

            locHrs[ix] = IMISSD;

            windDirAr[ix] = " ";
            windSmphAr[ix] = " ";
            gust_MphAr[ix] = " ";
            pwindDirAr[ix] = " ";
            windCharAr[ix] = " ";
            skyCoverAr[ix] = " ";
            avgSkyCoverAr[ix] = " ";

            obvisAr[ix] = " ";

            rainAr[ix] = " ";
            rainShwrsAr[ix] = " ";
            sprinklesAr[ix] = " ";
            tstmsAr[ix] = " ";
            drizzleAr[ix] = " ";
            snowAr[ix] = " ";
            snowShwrsAr[ix] = " ";
            flurriesAr[ix] = " ";
            sleetAr[ix] = " ";
            frzgRainAr[ix] = " ";
            frzgDrzlAr[ix] = " ";

            hazardsAr[ix] = " ";

            utcDateTm[ix] = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            utcDateTm[ix].set(Calendar.SECOND, 0);
            utcDateTm[ix].set(Calendar.MILLISECOND, 0);
        }
    }

    /**
     * addCurrParameters uses the storage arrays for the parameters and sets the
     * fields and add them to the NcPafmUgc (DB)
     * 
     * @param UGC
     */
    public static void addCurrParameters(NcPafmUgc UGC) {
        for (int nx = 0; nx < ndxLast; nx++) {
            NcPafmParameters currentParameters = new NcPafmParameters();

            currentParameters.setForecastTimeUtc(utcDateTm[nx]);
            currentParameters.setForecastTimeLocal(locHrs[nx]);
            currentParameters.setForecastTimeZone(locTimeZone);

            currentParameters.setAvgMxTmpf(avgMxTmpfAr[nx]);
            currentParameters.setHiMxTmpf(hiMxTmpfAr[nx]);
            currentParameters.setLoMxTmpf(loMxTmpfAr[nx]);
            currentParameters.setAvgMnTmpf(avgMnTmpfAr[nx]);
            currentParameters.setHiMnTmpf(hiMnTmpfAr[nx]);
            currentParameters.setLoMnTmpf(loMnTmpfAr[nx]);
            currentParameters.setTmpf(tmpfAr[nx]);
            currentParameters.setDwpf(dwpfAr[nx]);
            currentParameters.setRelh(relhAr[nx]);
            currentParameters.setMaxHeat(maxHeatAr[nx]);
            currentParameters.setMinChill(minChillAr[nx]);
            currentParameters.setHeatIndex(heatIndexAr[nx]);
            currentParameters.setWindChill(windChillAr[nx]);
            currentParameters.setPop12(pop12Ar[nx]);
            currentParameters.setQpf12Mn(qpf12MnAr[nx]);
            currentParameters.setQpf12Mx(qpf12MxAr[nx]);
            currentParameters.setHighestMaxQpf(highestMaxQpfAr[nx]);
            currentParameters.setLowestMaxQpf(lowestMaxQpfAr[nx]);
            currentParameters.setWindDir(windDirAr[nx]);
            currentParameters.setWindSmph(windSmphAr[nx]);
            currentParameters.setGust_Mph(gust_MphAr[nx]);
            currentParameters.setSkyCover(skyCoverAr[nx]);
            currentParameters.setPwindDir(pwindDirAr[nx]);
            currentParameters.setWindChar(windCharAr[nx]);
            currentParameters.setAvgSkyCover(avgSkyCoverAr[nx]);
            currentParameters.setRain(rainAr[nx]);
            currentParameters.setRainShwrs(rainShwrsAr[nx]);
            currentParameters.setSprinkles(sprinklesAr[nx]);
            currentParameters.setTstms(tstmsAr[nx]);
            currentParameters.setDrizzle(drizzleAr[nx]);
            currentParameters.setSnow(snowAr[nx]);
            currentParameters.setSnowShwrs(snowShwrsAr[nx]);
            currentParameters.setFlurries(flurriesAr[nx]);
            currentParameters.setSleet(sleetAr[nx]);
            currentParameters.setFrzgRain(frzgRainAr[nx]);
            currentParameters.setFrzgDrzl(frzgDrzlAr[nx]);
            currentParameters.setSnow12Mn(snow12MnAr[nx]);
            currentParameters.setSnow12Mx(snow12MxAr[nx]);
            currentParameters.setObvis(obvisAr[nx]);
            /*
             * Process the currently known Hazardous weather events when a valid
             * Watch (A), Warning (W), and Advisory (Y) appear in the PAFM.
             */
            currentParameters.setHazards(hazardsAr[nx]);
            /*
             * Add currentParameters
             */
            UGC.addPafmParms(currentParameters);
        }
    }

    /**
     * Concatenate hazard into hazardAr if given
     * 
     * @param parmRow
     *            The row of the matrix with a hazard
     * @parma hazStr The hazard name
     * @param ndxSt
     *            Starting index
     * @param ndxLast
     *            Ending index
     * @param locStNdx
     *            Array of the starting positions of the fields
     * @param locEnNdx
     *            Array of the ending positions of the fields
     */
    public static void parseHazardParmRow(String parmRow, String hazStr,
            int ndxSt, int ndxLast, int[] locStNdx, int[] locEnNdx) {
        String parmValStr;
        int ndxEn = Math.min(ndxLast, maxNmHr);
        for (int idx = ndxSt; idx < ndxEn; idx++) {
            parmValStr = parmRow.substring(locStNdx[idx], locEnNdx[idx]).trim();
            if ((parmValStr.equals("A") || parmValStr.equals("W") || parmValStr
                    .equals("Y"))
                    && (hazardsAr[idx].length() + hazStr.length() < 157)) {
                hazardsAr[idx] = hazardsAr[idx] + hazStr + ";" + parmValStr
                        + "|";
            }
        }
    }

    /**
     * readZoneLocs reads and stores the zones.xml fields creating StationTable
     * zonesList
     * 
     * @throws Exception
     */
    public static void readZoneLocs() throws Exception {

        // final String NCEP_DIR = "ncep";
        // final String stnsDir = "stns";
        // final String idftLocTableName = "zones.xml";

        final String ZONES_TABLE = "ncep" + File.separator + "stns"
                + File.separator + "zones.xml";

        IPathManager manager = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = null;

        // File baseDir = null;
        // String stnsFileName = null;
        baseContext = manager.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        // baseContext.setContextName(NCEP_DIR);
        LocalizationFile zonesFile = null;
        zonesFile = manager.getStaticLocalizationFile(ZONES_TABLE);
        // baseDir = manager.getFile(baseContext, "");
        // stnsFileName = baseDir + File.separator + stnsDir + File.separator
        // + idftLocTableName;

        if (zonesFile != null) {
            // zonesList = new StationTable(stnsFileName);
            zonesList = new StationTable(zonesFile.getFile().getAbsolutePath());
        }
    }
}
