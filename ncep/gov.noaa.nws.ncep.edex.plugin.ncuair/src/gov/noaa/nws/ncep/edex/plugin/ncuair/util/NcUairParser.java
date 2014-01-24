/**
 * NcUairParser
 * 
 * This java class is an utility to parse upper air sounding data.
 * 
 * HISTORY
 *
 * * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010     	 210				L. Lin     	Initial coding
 * 05/2010		 210                L. Lin      migration to TO11DR11
 * 02/2010		 210				T. Lee		Fixed TTCC tropopause
 * 09/2011                          Chin Chen   add batch parsing methods for better performance
 * 09/2011       457                S. Gurung   Renamed H5 to Nc and h5 to nc
 * 12/2013                          T. Lee      Fixed TTCC Wmax pressure off by factor of 10
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairLiftedIndex;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairMaxWind;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairObsLevels;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairTropopause;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class NcUairParser {
    // UU is data from ship; XX is data from mobile
    public static final int TTAA = 1;

    public static final int TTBB = 2;

    public static final int TTCC = 3;

    public static final int TTDD = 4;

    public static final int PPAA = 5;

    public static final int PPBB = 6;

    public static final int PPCC = 7;

    public static final int PPDD = 8;

    public static final int UUAA = 1;

    public static final int UUBB = 2;

    public static final int UUCC = 3;

    public static final int UUDD = 4;

    public static final int XXAA = 1;

    public static final int XXBB = 2;

    public static final int XXCC = 9;

    public static final int XXDD = 10;

    /**
     * Constructor
     */
    public NcUairParser() {
    }

    /**
     * Return the dataType with format "(TT|PP|XX|UU)(AA|BB|CC|DD)"
     * 
     * @param theReport
     *            The input upper air data report
     * @return
     */
    public static String getDataType(String theReport) {

        /** Regular expression for dataType */
        final String DATA_TYPE = "((TT|PP|XX|UU)(AA|BB|CC|DD)) ";
        Pattern datatypePattern = Pattern.compile(DATA_TYPE);
        Matcher datatypeMatcher = datatypePattern.matcher(theReport);
        String retType = null;

        if (datatypeMatcher.find()) {
            retType = datatypeMatcher.group(1);
        }
        return retType;
    }

    /**
     * Return the station Number
     * 
     * @param theReport
     *            The input upper air data report
     * @return
     */
    public static String getStationNumber(String theReport) {

        /** Regular expression for stationNumber */
        final String STATIONNUMBER = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{2})(\\d{2})(\\d{1}|/) (\\d{5})";
        Pattern stationNumberPattern = Pattern.compile(STATIONNUMBER);
        Matcher stationNumberMatcher = stationNumberPattern.matcher(theReport);
        String retStationNumber = null;

        if (stationNumberMatcher.find()) {
            retStationNumber = stationNumberMatcher.group(7);
        } else {
            /** Regular expression for stationNumber */
            final String STATIONNUMBER2 = "(TT|PP)(AA|BB|CC|DD) (\\d{5}) NIL";
            Pattern stationNumberPattern2 = Pattern.compile(STATIONNUMBER2);
            Matcher stationNumberMatcher2 = stationNumberPattern2
                    .matcher(theReport);
            if (stationNumberMatcher2.find()) {
                retStationNumber = stationNumberMatcher2.group(3);
            } else {
                /** Regular expression for stationNumber */
                final String STATIONNUMBER3 = "(TT|PP)(AA|BB|CC|DD) (/////) (\\d{5}) NIL";
                Pattern stationNumberPattern3 = Pattern.compile(STATIONNUMBER3);
                Matcher stationNumberMatcher3 = stationNumberPattern3
                        .matcher(theReport);
                if (stationNumberMatcher3.find()) {
                    retStationNumber = stationNumberMatcher3.group(4);
                }
            }
        }
        return retStationNumber;
    }

    /**
     * Get the correction indicator from WMO header.
     * 
     * @param theReport
     *            The input upper air data report
     * @return a String for corIndicator
     */
    public static String findCorIndicator(String theReport) {

        String corIndicator = null;

        /** Regular expression for corIndicator */
        final String corpat = "[A-Z]{4}\\d{0,2} [A-Z]{4} \\d{6}( )?([A-Z]{3})?\\r\\r\\n";
        Pattern CORPattern = Pattern.compile(corpat);
        Matcher corMatcher = CORPattern.matcher(theReport);

        if (corMatcher.find()) {
            corIndicator = corMatcher.group(2);
        }

        return corIndicator;
    }

    /**
     * From a given dataType returns an integer to represent that dataType.
     * 
     * @param dataType
     *            The input dataType
     * @return an integer for that dataType
     */
    public static Integer getUairType(String dataType) {
        HashMap<String, Integer> uairhm = new HashMap();

        uairhm.put("TTAA", new Integer(TTAA));
        uairhm.put("TTBB", new Integer(TTBB));
        uairhm.put("TTCC", new Integer(TTCC));
        uairhm.put("TTDD", new Integer(TTDD));

        uairhm.put("PPAA", new Integer(PPAA));
        uairhm.put("PPBB", new Integer(PPBB));
        uairhm.put("PPCC", new Integer(PPCC));
        uairhm.put("PPDD", new Integer(PPDD));

        uairhm.put("XXAA", new Integer(XXAA));
        uairhm.put("XXBB", new Integer(XXBB));
        uairhm.put("XXCC", new Integer(XXCC));
        uairhm.put("XXDD", new Integer(XXDD));

        uairhm.put("UUAA", new Integer(UUAA));
        uairhm.put("UUBB", new Integer(UUBB));
        uairhm.put("UUCC", new Integer(UUCC));
        uairhm.put("UUDD", new Integer(UUDD));

        return (Integer) uairhm.get(dataType);
    }

    /**
     * Parse the entire code message and interpret to observation level
     * information according its data type.
     * 
     * @param codeMessage
     *            The input upper air code groups
     * @param record
     *            The in and out Uair record
     * @return
     */
    static int exceptionCount = 0;

    public static void getLevels(String codeMessage, NcUairRecord record) {

        Boolean windKnot = false;

        String dataType = record.getDataType();

        int uairType = getUairType(dataType);
        // System.out.println("getLEvel uair datatype="+dataType);
        String[] codeGroupAr;// Chin, do not use fix array size, it will cause
                             // exception when size is over 500 = new
                             // String[500];
        List<String> codeGpList = new ArrayList<String>();
        // System.out.println("codeMessage...\n" + codeMessage);

        if (dataType.substring(0, 2).equals("XX")
                || dataType.substring(0, 2).equals("UU")) {
            windKnot = NcUairShipMobile.getWindKnot();
        } else {
            windKnot = NcUairTimeGroup.getWindKnot();
        }
        // Break the code message into segments by a " "
        Scanner sc = new Scanner(codeMessage).useDelimiter(" ");
        int cgSize = 0;

        while (sc.hasNext()) {
            String codeGroup = sc.next();
            codeGpList.add(codeGroup);
            // Chin codeGroupAr[cgSize] = codeGroup;
            cgSize++;
        }
        codeGroupAr = new String[cgSize];
        codeGpList.toArray(codeGroupAr);
        try {
            switch (uairType) {
            case TTAA: {
                processTTAACC(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            case TTBB: {
                processTTBBDD(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            case TTCC: {
                processTTAACC(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            case TTDD: {
                processTTBBDD(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            case PPAA: {
                processPPAACC(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            case PPBB: {
                processPPBBDD(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            case PPCC: {
                processPPAACC(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            case PPDD: {
                processPPBBDD(codeGroupAr, cgSize, windKnot, record);
                break;
            }
            default: {
                System.out.println("\n Invalid datatype!");
            }
            }
        } catch (Exception e) {
            exceptionCount++;
            System.out.println("NcUair parsing exception! #" + exceptionCount);
        }
    }

    /**
     * Decodes a Pres/height, temperature, and wind fields in the forms PPhhh
     * and TTTdd and DDDff for TTAA/CC.'
     * 
     * @param codeGroupAr
     *            The input uppair code group array
     * @param cgSize
     *            The input cgSize is the size of the code group array
     * @param windKnot
     *            The input windKnot is flag to indicate wind in knot or not.
     * @param record
     *            The in and out Uair record
     * @return
     */
    public static void processTTAACC(String[] codeGroupAr, int cgSize,
            Boolean windKnot, NcUairRecord record) {

        Boolean above = false;
        Boolean endrpt = false;
        Boolean topwindFlag = false;
        Boolean drop = false;

        int level = 0;
        int topwind = IDecoderConstantsN.UAIR_INTEGER_MISSING;

        String dataType = record.getDataType();
        String stationNumber = record.getStnum();

        if (dataType.substring(2, 4).equals("CC")) {
            above = true;
        }
        if (dataType.substring(0, 2).equals("XX")
                || dataType.substring(0, 2).equals("UU")) {
            topwind = NcUairShipMobile.getTopwind();
            if (dataType.substring(0, 2).equals("XX")) {
                drop = true;
            }
        } else {
            topwind = NcUairTimeGroup.getTopwind();
        }
        if (topwind == IDecoderConstantsN.UAIR_INTEGER_MISSING) {
            topwindFlag = true;
        }
        int topWind = getTopWind(topwind, above);

        int i = 0;
        while (i < cgSize && !endrpt) {

            float pres = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float height = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float temp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float dewpTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float wdir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float wspeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;

            if (codeGroupAr[i].length() < 3) {
                break;
            }
            if (codeGroupAr[i].equals("51515")) {
                endrpt = true;
                i++;
                // Decode lifted index and mean low level wind groups
                if ((i + 2) < cgSize) {
                    processLiftedIndex(codeGroupAr, i, windKnot, record);
                }
                break;
            }

            // This should be drop data
            if (codeGroupAr[i].equals("61616")) {
                endrpt = true;
                if (drop) {
                    // process drop data
                    // System.out.println("got drop 61616 in TT AACC - need to process...\n");
                }
                break;
            }

            String pp = codeGroupAr[i].substring(0, 2);
            if (pp.equals("^C")) {
                break;
            }

            if (pp.equals("88") || pp.equals("77") || pp.equals("66")) {
                // Decode supplemental data
                String hhh = codeGroupAr[i].substring(2, 5);
                if (pp.equals("88")) {
                    // Decode tropopause information
                    if (hhh.equals("999")) {
                        // NO tropopause data
                        i++;
                    } else {
                        processTropopause(codeGroupAr, i, topwindFlag,
                                windKnot, record, above);
                        if (topwindFlag) {
                            i = i + 2;
                        } else {
                            i = i + 3;
                        }
                    }
                } else if (pp.equals("77") || pp.equals("66")) {
                    // Decode maximum wind group
                    if (hhh.equals("999")) {
                        // NO maximum wind data
                        i++;
                    } else {
                        processMaximumWindShear(codeGroupAr, i, cgSize,
                                windKnot, above, record);
                        i = i + 3;
                    }
                }
            } else if (codeGroupAr[i].equals("31313")) {
                i = i + 3;
            } else if (codeGroupAr[i].equals("41414")) {
                i = i + 2;
            } else {
                int range = i + 2;
                if (range < cgSize) {
                    // Decode TTAA normal report -pressure and height
                    NcUairPressureHeightGroup.PressureHeightField(
                            codeGroupAr[i], above, level, stationNumber,
                            dataType, record);
                    pres = NcUairPressureHeightGroup.getPressure();
                    height = NcUairPressureHeightGroup.getHeight();

                    // Decode temperature group
                    NcUairTempGroup.TempField(codeGroupAr[i + 1]);
                    temp = NcUairTempGroup.getTemperature();
                    dewpTemp = NcUairTempGroup.getDewpointTemp();

                    if (pres >= topWind) {
                        // Decode wind group
                        NcUairWindGroup.WindField(codeGroupAr[i + 2], windKnot);
                        wspeed = NcUairWindGroup.getSped();
                        wdir = NcUairWindGroup.getDrct();
                        if (wdir > 360) {
                            wdir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                            endrpt = true;
                            break;
                        }
                    } else {
                        i--;
                    }

                    // Add level
                    addLevels(record, pres, height, temp, dewpTemp, wdir,
                            wspeed);
                }
                i = i + 3;
                level++;
            }
        }

    }

    /**
     * Decodes a pressure/height, temperature, and wind fields in the forms
     * PPhhh and TTTdd and DDDff for TTBB/DD.
     * 
     * @param codeGroupAr
     *            The input uppair code group array
     * @param cgSize
     *            The input cgSize is the size of the code group array
     * @param windKnot
     *            The input windKnot is flag to indicate wind in knot or not.
     * @param record
     *            The in and out Uair record
     * @return
     */
    public static void processTTBBDD(String[] codeGroupAr, int cgSize,
            Boolean windKnot, NcUairRecord record) {

        Boolean above = false;
        Boolean endrpt = false;
        Boolean wind = false;
        Boolean drop = false;

        String dataType = record.getDataType();

        if (dataType.substring(2, 4).equals("DD")) {
            above = true;
        }
        if (dataType.substring(0, 2).equals("XX")) {
            drop = true;
        }

        int i = 0;
        while (i < cgSize && !endrpt) {

            float pres = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float height = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float temp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float dewpTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float wdir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
            float wspeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;

            if (codeGroupAr[i].equals("51515")) {
                endrpt = true;
                i++;
                // Decode lifted index and mean low level wind groups
                if ((i + 2) < cgSize) {
                    processLiftedIndex(codeGroupAr, i, windKnot, record);
                }
                break;
            }

            if (codeGroupAr[i].equals("61616")) {
                endrpt = true;
                if (drop) {
                    // System.out.println("got drop 61616 in BBDD need to process.... \n");
                }
                break;
            }

            if (codeGroupAr[i].equals("21212")) {
                wind = true;
                i++;
                // System.out.println("got 21212 significant wind group need to process...\n");
                break;
            }

            if (codeGroupAr[i].equals("31313")) {
                i = i + 3;
            } else if (codeGroupAr[i].equals("41414")) {
                i = i + 2;
            } else {
                int range = i + 1;
                if (range < cgSize) {
                    // Decode Pres group
                    pres = getPressureFromTTBBDD(codeGroupAr[i], above);

                    if (!wind) {
                        // Decode temperature group
                        NcUairTempGroup.TempField(codeGroupAr[i + 1]);
                        temp = NcUairTempGroup.getTemperature();
                        dewpTemp = NcUairTempGroup.getDewpointTemp();
                    } else {
                        // Decode wind group
                        NcUairWindGroup.WindField(codeGroupAr[i + 1], windKnot);
                        wspeed = NcUairWindGroup.getSped();
                        wdir = NcUairWindGroup.getDrct();
                    }

                    // Add level
                    addLevels(record, pres, height, temp, dewpTemp, wdir,
                            wspeed);

                }
                i = i + 2;
            }
        }
    }

    /**
     * Decodes a pressure/height and temperature fields for PPAACC. These
     * reports contain wind data at mandatory levels below 100 mb.
     * 
     * @param codeGroupAr
     *            The input uppair code group array
     * @param cgSize
     *            The input cgSize is the size of the code group array
     * @param windKnot
     *            The input windKnot is flag to indicate wind in knot or not.
     * @param record
     *            The in and out Uair record
     * @return
     */
    public static void processPPAACC(String[] codeGroupAr, int cgSize,
            Boolean windKnot, NcUairRecord record) {

        Boolean above = false;
        Boolean endrpt = false;
        float height = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float temp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float dewpTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;

        float pold;

        String dataType = record.getDataType();

        if (dataType.substring(2, 4).equals("CC")) {
            above = true;
            pold = 100;
        } else {
            pold = 2000;
        }

        int i = 0;
        while (i < cgSize && !endrpt) {

            if (codeGroupAr[i].equals("51515")) {
                endrpt = true;
                break;
            }

            String ppIndicator = codeGroupAr[i].substring(0, 2);
            // Check that first two characters are 44 or 55.
            if (ppIndicator.equals("44") || ppIndicator.equals("55")) {
                String skip = codeGroupAr[i].substring(2, 5);
                if (skip.equals("///")) {
                    i++;
                } else {
                    // Decode number of pressures which is the third character.
                    /* chin, fix bug, when a non-number char is decoded */
                    int onetothree = 0;
                    try {
                        onetothree = Integer.parseInt(codeGroupAr[i].substring(
                                2, 3));
                    } catch (NumberFormatException e) {
                        onetothree = 0;

                    }
                    if (onetothree > 0 && onetothree <= 3) {
                        // Decode the pressure
                        float[] presArr = {
                                IDecoderConstantsN.UAIR_FLOAT_MISSING,
                                IDecoderConstantsN.UAIR_FLOAT_MISSING,
                                IDecoderConstantsN.UAIR_FLOAT_MISSING, };
                        presArr = getPressureFromPPAACC(codeGroupAr[i], above);

                        i++;
                        for (int index = 0; index < onetothree; index++) {
                            float wdir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                            float wspeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                            float pres = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                            float ppp = presArr[index];
                            if (ppp < pold) {
                                pres = presArr[index];
                                pold = ppp;
                            }

                            // Decode wind group
                            if (i < cgSize) {
                                NcUairWindGroup.WindField(codeGroupAr[i],
                                        windKnot);
                                wspeed = NcUairWindGroup.getSped();
                                wdir = NcUairWindGroup.getDrct();
                            }

                            // Add level
                            addLevels(record, pres, height, temp, dewpTemp,
                                    wdir, wspeed);

                            i++;
                        }
                    } else {
                        // no pressure group
                        i++;
                    }
                }
            } else if (ppIndicator.equals("77") || ppIndicator.equals("66")) {
                // Decode maximum wind group
                String hhh = codeGroupAr[i].substring(2, 5);
                if (hhh.equals("999")) {
                    // NO maximum wind and wind shear data
                    i++;
                } else {
                    processMaximumWindShear(codeGroupAr, i, cgSize, windKnot,
                            above, record);
                    i = i + 3;
                }
            } else {
                i++;
                break;
            }
        }
    }

    /**
     * Decodes a pressure/height and temperature fields for PPBBDD. These
     * reports contain significant wind data above 100 mb.
     * 
     * @param codeGroupAr
     *            The input uppair code group array
     * @param cgSize
     *            The input cgSize is the size of the code group array
     * @param windKnot
     *            The input windKnot is flag to indicate wind in knot or not.
     * @param record
     *            The in and out Uair record
     * @return
     */
    public static void processPPBBDD(String[] codeGroupAr, int cgSize,
            Boolean windKnot, NcUairRecord record) {

        Boolean endrpt = false;
        Boolean pressflag = false;
        Boolean above = false;

        String dataType = record.getDataType();

        if (dataType.substring(2, 4).equals("DD")) {
            above = true;
        }

        int i = 0;
        try {
            while (i < cgSize && !endrpt) {

                float pres = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                float height = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                float temp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                float dewpTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                float wdir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
                float wspeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;

                if (codeGroupAr[i].equals("21212")) {
                    pressflag = true;
                    i++;
                    if (i >= cgSize) {
                        break;
                    }
                    // System.out.println("got 21212 in PP BBDD - need to process...\n");
                    break;
                }

                if (codeGroupAr[i].equals("51515")
                        || codeGroupAr[i].equals("41414")) {
                    endrpt = true;
                    // System.out.println("got end group 51515 41414 in PP BBDD - BREAK \n");
                    break;
                }

                // Convert the unit digits to integers and compute height.
                Boolean missing = false;
                int noWind = 0;
                if (!pressflag) {
                    while (!missing && (noWind < 3)) {
                        int index = noWind + 3;
                        String unit = "/";
                        unit = codeGroupAr[i].substring(index - 1, index);

                        if (unit.equals(("/"))) {
                            missing = true;
                            break;
                        }

                        // Decode height; invalid height if returns
                        // FLOAT_MISSING.
                        height = getHeightFromPPBBDD(codeGroupAr[i], index);
                        if (height == IDecoderConstantsN.UAIR_FLOAT_MISSING) {
                            i++;
                            break;
                        }
                        int range = i + noWind + 1;
                        if (range < cgSize) {
                            // Decode wind group.
                            NcUairWindGroup.WindField(codeGroupAr[i + noWind
                                    + 1], windKnot);
                            wspeed = NcUairWindGroup.getSped();
                            wdir = NcUairWindGroup.getDrct();
                        }

                        // Add level
                        addLevels(record, pres, height, temp, dewpTemp, wdir,
                                wspeed);

                        noWind++;
                    }

                    i = i + noWind + 1;
                } else {
                    int range = i + 1;
                    if (range < cgSize) {
                        // Decode pressure group
                        pres = getPressureFromTTBBDD(codeGroupAr[i], above);
                        NcUairWindGroup.WindField(codeGroupAr[i + 1], windKnot);
                        wspeed = NcUairWindGroup.getSped();
                        wdir = NcUairWindGroup.getDrct();
                    }
                    i = i + 2;
                }
            }
        } catch (Exception e) {

        }
    }

    /**
     * Decodes a pressure field for mandatory wind reports (PPAA or PPCC). The
     * pressures reporting are returned.
     * 
     * @param presGroup
     *            The input pressure code group
     * @param above
     *            The input above is flag
     * @return pressure
     */
    public static float[] getPressureFromPPAACC(String presGroup, Boolean above) {

        float pressure[] = { IDecoderConstantsN.UAIR_FLOAT_MISSING,
                IDecoderConstantsN.UAIR_FLOAT_MISSING,
                IDecoderConstantsN.UAIR_FLOAT_MISSING };

        String bpres[] = { "00", "92", "85", "70", "50", "40", "30", "25",
                "20", "15", "10" };

        String apres[] = { "70", "50", "30", "20", "10", "07", "05", "03",
                "02", "01" };

        int rbprs[] = { 1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100 };

        int raprs[] = { 70, 50, 30, 20, 10, 7, 5, 3, 2, 1 };

        int level = -1;

        String hh = presGroup.substring(3, 5);
        // Check first pressure against list.
        int ifirst = -1;
        if (!above) {
            for (String plevel : bpres) {
                ifirst++;
                if (hh.equals(plevel)) {
                    level = ifirst;
                    break;
                }
            }
        } else {
            for (String plevel : apres) {
                ifirst++;
                if (hh.equals(plevel)) {
                    level = ifirst;
                    break;
                }
            }
        }

        if (!above) {
            if (ifirst < 11 && level >= 0) {
                pressure[0] = rbprs[level];
                if (level + 1 < 11) {
                    pressure[1] = rbprs[level + 1];
                }
                if (level + 2 < 11) {
                    pressure[2] = rbprs[level + 2];
                }
            }
        } else {
            if (ifirst < 10 && level >= 0) {
                pressure[0] = raprs[level];
                if (level + 1 < 10) {
                    pressure[1] = raprs[level + 1];
                }
                if (level + 2 < 10) {
                    pressure[2] = raprs[level + 2];
                }
            }
        }

        return pressure;
    }

    /**
     * Decodes the height from a significant wind field from reports (PPBB or
     * PPDD). The field is of the form atuuu where a is 1 if the height is above
     * 100000 feet and 9 or 8 otherwise. t is the ten-thousands digit of the
     * height and the u's are up to three thousands-of-feet fields. The heights
     * are converted from feet to meters.
     * 
     * @param heightGroup
     *            The input heightGroup contains the height code group.
     * @param index
     *            The input index is the current position in the code group.
     * @return pressure
     */
    public static float getHeightFromPPBBDD(String heightGroup, int index) {
        float height = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        final double feetToMeter = 0.3048;

        String hhIndicator = heightGroup.substring(0, 1);
        int iadd = 0;
        // Check that the first character of the field is '9' or '1'.
        if (hhIndicator.equals("9") || hhIndicator.equals("8")) {
            iadd = 0;
        } else {
            return height;
        }

        // Convert the tens digit to an integer.
        int iten;
        iten = Integer.parseInt(heightGroup.substring(1, 2)) * 10;
        int iunit = Integer.parseInt(heightGroup.substring(index - 1, index));
        height = (iten + iunit) * 1000 + iadd;
        height = (float) (height * feetToMeter);
        return height;
    }

    /**
     * Decodes a pressure field from a group reports (TTBB or TTDD). The
     * pressures reporting are returned.
     * 
     * @param presGroup
     *            The input pressure code group
     * @param above
     *            The input above is flag
     * @return pressure
     */
    public static float getPressureFromTTBBDD(String presGroup, Boolean above) {

        float pressure = IDecoderConstantsN.UAIR_FLOAT_MISSING;

        String clev[] = { "00", "11", "22", "33", "44", "55", "66", "77", "88",
                "99" };

        if (presGroup.length() == 5) {
            String pp = presGroup.substring(0, 2);
            String hhh = presGroup.substring(2, 5);

            int level = -1;
            if (!pp.equals("//") && !hhh.substring(2, 3).equals("/")) {
                for (String plevel : clev) {
                    level++;
                    if (pp.equals(plevel)) {
                        break;
                    }
                }
                // If a level was found, decode the pressure.
                if (level != -1 && level < 10) {
                    int ihhh = Integer.parseInt(hhh);
                    if (above) {
                        pressure = (float) ihhh / (float) 10;
                    } else {
                        if (ihhh < 100) {
                            pressure = ihhh + 1000;
                        } else {
                            pressure = ihhh;
                        }
                    }
                }
            }
        }
        return pressure;
    }

    /**
     * Decodes data for the tropopause level from the TTAA reports. The output
     * data are ordered PRES TEMP DWPT DRCT SPED.
     * 
     * @param codeGroupAr
     *            The input uppair code group array
     * @param index
     *            The input index is the current position of the code group
     *            array
     * @param topwindFlag
     *            the input index is the flag to indicate top wind
     * @param windKnot
     *            The input windKnot is flag to indicate wind in knot or not.
     * @param record
     *            The in and out Uair record
     * @return
     */
    public static void processTropopause(String[] codeGroupAr, int index,
            Boolean topwindFlag, Boolean windKnot, NcUairRecord record,
            Boolean above) {

        float pres = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float temp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float dewpTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wdir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wspeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;

        int i = index;

        String pp = codeGroupAr[i].substring(0, 2);
        String hhh = codeGroupAr[i].substring(2, 5);

        if (pp.equals("88")) {
            // Decode pressure and height
            pres = Integer.parseInt(hhh);
            if (above) {
                pres = pres / 10.f;
            }

            // Decode temperature group
            NcUairTempGroup.TempField(codeGroupAr[i + 1]);
            temp = NcUairTempGroup.getTemperature();
            dewpTemp = NcUairTempGroup.getDewpointTemp();

            if (!topwindFlag) {
                // Decode wind group
                NcUairWindGroup.WindField(codeGroupAr[i + 2], windKnot);
                wspeed = NcUairWindGroup.getSped();
                wdir = NcUairWindGroup.getDrct();
            }

            addTrop(record, pres, temp, dewpTemp, wdir, wspeed);
        }
    }

    /**
     * Decodes data for the maximum wind level(s) and wind shear from the TTAA
     * reports. The output data are ordered PRES DRCT SPED.
     * 
     * @param codeGroupAr
     *            The input uppair code group array
     * @param index
     *            The input index is the current position of the code group
     *            array
     * @param cgSize
     *            the input cgSize is the size of the codeGroupAr.
     * @param windKnot
     *            The input windKnot is flag to indicate wind in knot or not.
     * @param record
     *            The in and out Uair record
     * @return
     */
    public static void processMaximumWindShear(String[] codeGroupAr, int index,
            int cgSize, Boolean windKnot, Boolean above, NcUairRecord record) {

        float pres = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wdir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wspeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float loShear = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float hiShear = IDecoderConstantsN.UAIR_FLOAT_MISSING;

        int i = index;

        String ppIndicator = codeGroupAr[i].substring(0, 2);
        String hhh = codeGroupAr[i].substring(2, 5);

        if (ppIndicator.equals("77") || ppIndicator.equals("66")) {
            // Decode pressure and height
            if (!hhh.substring(1, 3).equals("//")) {
                if (above) {
                    pres = Integer.parseInt(hhh) / 10.f;
                } else {
                    pres = Integer.parseInt(hhh);
                }
            }

            // Decode wind group
            NcUairWindGroup.WindField(codeGroupAr[i + 1], windKnot);
            wspeed = NcUairWindGroup.getSped();
            wdir = NcUairWindGroup.getDrct();

            if (i + 2 < cgSize) {
                String windshearIndicator = codeGroupAr[i + 2].substring(0, 1);
                if (windshearIndicator.equals("4")) {
                    String below = codeGroupAr[i + 2].substring(1, 3);
                    String abov = codeGroupAr[i + 2].substring(3, 5);
                    if (!below.equals("//")) {
                        loShear = Integer.parseInt(below);
                    }
                    if (!abov.equals("//")) {
                        hiShear = Integer.parseInt(abov);
                    }
                }
            }

            NcUairMaxWind mwind = null;
            mwind = new NcUairMaxWind();
            mwind.setPres(pres);
            mwind.setDrct(wdir);
            mwind.setSped(wspeed);
            mwind.setLoShear(loShear);
            mwind.setHiShear(hiShear);

            record.addMaxWind(mwind);
        }
    }

    /**
     * Decodes data for the lifted index group from the TTAA reports. The output
     * data are ordered liTemp, wdirSurface, wspeedSurface, wdirAbove,
     * wspeedAbove
     * 
     * @param codeGroupAr
     *            The input uppair code group array
     * @param index
     *            The input index is the current position of the code group
     *            array
     * @param windKnot
     *            The input windKnot is flag to indicate wind in knot or not.
     * @param record
     *            The in and out Uair record
     * @return
     */
    public static void processLiftedIndex(String[] codeGroupAr, int index,
            Boolean windKnot, NcUairRecord record) {

        float liTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wdirSurface = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wspeedSurface = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wdirAbove = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        float wspeedAbove = IDecoderConstantsN.UAIR_FLOAT_MISSING;

        int i = index;
        if (codeGroupAr[i].equals("10164")) {
            int ittt = Integer.parseInt(codeGroupAr[i + 1]);
            if (ittt > 50) {
                // subtract 50 if lllll greater than 50
                ittt = ittt - 50;
            }
            int isign = ittt % 2;
            /*
             * If the integer is even, the liTemp is positive. Otherwise, the
             * liTemp is negative.
             */
            if (isign == 1) {
                liTemp = -ittt;
            } else {
                liTemp = ittt;
            }
            liTemp = (float) (liTemp / 10.);
            i = i + 2;
        }

        if (codeGroupAr[i].equals("10194")) {
            // Decode wind group from surface to 5000ft
            NcUairWindGroup.WindField(codeGroupAr[i + 1], windKnot);
            wspeedSurface = NcUairWindGroup.getSped();
            wdirSurface = NcUairWindGroup.getDrct();

            // Decode wind group from 5000ft to 10000ft
            NcUairWindGroup.WindField(codeGroupAr[i + 2], windKnot);
            wspeedAbove = NcUairWindGroup.getSped();
            wdirAbove = NcUairWindGroup.getDrct();

            NcUairLiftedIndex li = null;
            li = new NcUairLiftedIndex();
            li.setLiTemp(liTemp);
            li.setLoDrct(wdirSurface);
            li.setLoSped(wspeedSurface);
            li.setHiDrct(wdirAbove);
            li.setHiSped(wspeedAbove);
            record.addLiftedIndex(li);
        }
    }

    /**
     * Compute the top pressure reporting wind data. If topwind is missing, set
     * return top wind to a large value so that all the winds will be assumed to
     * be missing.
     * 
     * @param topwind
     *            The input topwind
     * @param above
     *            The input above is flag to indicate above 100mb.
     * @return an int for topWind.
     */
    public static int getTopWind(int topwind, Boolean above) {

        int retTopWind = 2000;

        if (topwind == IDecoderConstantsN.UAIR_INTEGER_MISSING) {
            retTopWind = 2000;
        } else if (topwind > 0) {
            if (above) {
                retTopWind = topwind * 10;
            } else {
                retTopWind = topwind * 100;
            }
        } else if (topwind == 0) {
            if (!above) {
                retTopWind = 1000;
            } else {
                retTopWind = 0;
            }
        } else {
            retTopWind = 2000;
        }
        return retTopWind;
    }

    /**
     * Add levels to record.
     * 
     * @param record
     *            The in and out Uair record
     * @param pres
     *            The input pressure
     * @param hght
     *            The input geopotential height
     * @param temp
     *            The input temperature
     * @param dwpt
     *            The input dew point temperature.
     * @param drct
     *            The input wind direction
     * @param sped
     *            The input wind speed
     * @return
     */
    public static void addLevels(NcUairRecord record, float pres,
            float geoHeight, float temp, float dwpt, float drct, float sped) {

        NcUairObsLevels level = null;
        level = new NcUairObsLevels();
        level.setPres(pres);
        level.setHght(geoHeight);
        level.setTemp(temp);
        level.setDwpt(dwpt);
        level.setDrct(drct);
        level.setSped(sped);

        record.addObsLevels(level);
    }

    /**
     * Add tropopause to record.
     * 
     * @param record
     *            The in and out Uair record
     * @param pres
     *            The input pressure
     * @param temp
     *            The input temperature
     * @param dwpt
     *            The input dew point temperature.
     * @param drct
     *            The input wind direction
     * @param sped
     *            The input wind speed
     * @return
     */
    public static void addTrop(NcUairRecord record, float pres, float temp,
            float dwpt, float drct, float sped) {

        NcUairTropopause trop = null;
        trop = new NcUairTropopause();
        trop.setPres(pres);
        trop.setTemp(temp);
        trop.setDwpt(dwpt);
        trop.setDrct(drct);
        trop.setSped(sped);

        record.addTropopause(trop);
    }

}
