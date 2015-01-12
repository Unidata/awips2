/**
 * International Significant Meteorological Information DecoderUtil
 * 
 * This java class intends to serve as a decoder utility for INTLSIGMET.
 * 
 * HISTORY
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 06/2009      113				L. Lin     	Initial coding
 * 07/2009		113				L. Lin		Migration to TO11
 * 09/2009      113             L. Lin      Convert station ID to lat/lon
 *                                          if any exists.
 * 11/2011      512             S. Gurung   Fixed NullPointerException bug while processing lat/lon (from vors)
 * May 14, 2014 2536            bclement    moved WMO Header to common, removed TimeTools usage
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.intlsigmet.util;

import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.tools.decoder.LatLonLocTbl;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

public class IntlSigmetParser {

    private static final String CONUS_EXP = "([A-Z]{4}) SIGMET (ALFA|BRAVO|CHARLIE|DELTA|ECHO|FOXTROT|GOLF|HOTEL|"
            + "INDIA|JULIETT|JULIET|KILO|LIMA|MIKE|NOVEMBER|OSCAR|PAPA|QUEBEC|ROMEO|SIERRA|TANGO|"
            + "UNIFORM|VICTOR|WHISKEY|XRAY|YANKEE|ZULU) ([0-9]{1}|[0-9]{2}) VALID";

    /**
     * Constructor
     */
    public IntlSigmetParser() {
    }

    /**
     * Parse the WMO line and store WMO header, OfficeID, issue time, ...
     * 
     * @param wmoline
     *            The bulletin message
     * 
     * @return an IntlSigmetRecord
     */
    public static IntlSigmetRecord processWMO(String wmoline, Headers headers) {

        IntlSigmetRecord record = null;
        // Regular expression for WMO/ICAO, station ID, and issue date.
        final String WMO_EXP = "([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})";

        // Pattern used for extracting WMO header, officeID, and issue date.
        final Pattern wmoPattern = Pattern.compile(WMO_EXP);
        Matcher theMatcher = wmoPattern.matcher(wmoline);

        if (theMatcher.find()) {
            record = new IntlSigmetRecord();

            record.setWmoHeader(theMatcher.group(1));
            record.setIssueOffice(theMatcher.group(2));

            // Decode the issue time.
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            Calendar issueTime = WMOTimeParser.findDataTime(
                    theMatcher.group(3), fileName);
            record.setIssueTime(issueTime);

            DataTime dataTime = new DataTime(issueTime);
            record.setDataTime(dataTime);
        }
        return record;
    }

    /**
     * Obtains hazardType as: TS (thunderstorm), TB (turbulence), HU
     * (hurricane), TR (tropical storm), TD (tropical depression), VA (volcanic
     * ash cloud), MW (marked mountain waves), TC (tropical cyclone), SQ (squall
     * line), CT (CAT), IC (icing), GR (hail), DS (duststorm), SS (sandstorm),
     * CB (cumulonimbus), WS (low level wind shear), TEST, or CN (cancel),
     * etc...
     * 
     * @param theReport
     *            Input report message.
     * @return a string for hazard type
     */
    public static String getHazardType(String theReport) {

        String retHazardType = " ";
        // Regular expression for hazardType
        final String HAZARDTYPE_EXP = "(HURRICANE|HURCN |TROPICAL STORM|TROPICAL DEPRESSION|"
                + " TD | SQL | TS | TS/|FRQ TS| TS| SH/TS |TURB| CB | VA | MTW |"
                + " ICE | TR | GR | TC | CT | CAT | DS | SS | WS | TSGR |VOLCANIC ASH|HVYSS|"
                + " ICG|ICING|LLWS|WATERSPOUTS|THUNDERSTORMS|WIND|TS OBS|CB/TS|TORNADO)";
        final String NIL_EXP = "(NIL)";
        final String MIS_EXP = "(TAF |METAR )";
        final String CNL_EXP = "(CANCEL| CNL | CNCL |:CNL| CNL|INVALID|DISCARDED|"
                + "CNL SIGMET| CLN SIGMET|CNCLD)";
        final String TEST_EXP = "(TEST |DISREGARD)";

        // Pattern used for extracting hazardType
        final Pattern hazardTypePattern = Pattern.compile(HAZARDTYPE_EXP);
        final Pattern nilPattern = Pattern.compile(NIL_EXP);
        final Pattern cnlPattern = Pattern.compile(CNL_EXP);
        final Pattern misPattern = Pattern.compile(MIS_EXP);
        final Pattern testPattern = Pattern.compile(TEST_EXP);

        Matcher theMatcher = hazardTypePattern.matcher(theReport);
        Matcher nilMatcher = nilPattern.matcher(theReport);
        Matcher cnlMatcher = cnlPattern.matcher(theReport);
        Matcher misMatcher = misPattern.matcher(theReport);
        Matcher testMatcher = testPattern.matcher(theReport);

        if (cnlMatcher.find()) {
            retHazardType = "CANCEL";
        } else if (testMatcher.find()) {
            retHazardType = "TEST";
        } else if (theMatcher.find()) {
            if (theMatcher.group(1).equals("HURRICANE")) {
                retHazardType = "HURRICANE";
            } else if (theMatcher.group(1).equals("HURCN ")) {
                retHazardType = "HURRICANE";
            } else if (theMatcher.group(1).equals(" TD ")) {
                retHazardType = "TROPICAL DEPRESSION";
            } else if (theMatcher.group(1).equals("TROPICAL STORM")) {
                retHazardType = "TROPICAL STORM";
            } else if (theMatcher.group(1).equals("TROPICAL DEPRESSION")) {
                retHazardType = "TROPICAL DEPRESSION";
            } else if (theMatcher.group(1).equals(" SQL ")) {
                retHazardType = "SQUALL LINE";
            } else if (theMatcher.group(1).equals("TURB")) {
                retHazardType = "TURBULENCE";
            } else if (theMatcher.group(1).equals(" ICE ")) {
                retHazardType = "ICING";
            } else if (theMatcher.group(1).equals(" ICG")) {
                retHazardType = "ICING";
            } else if (theMatcher.group(1).equals("ICING")) {
                retHazardType = "ICING";
            } else if (theMatcher.group(1).equals(" TS ")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals("FRQ TS")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals(" TS/")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals(" SH/TS ")) {
                retHazardType = "THUNDERSTORM WIND SHEAR";
            } else if (theMatcher.group(1).equals(" CB ")) {
                retHazardType = "CUMULONIMBUS";
            } else if (theMatcher.group(1).equals(" VA ")) {
                retHazardType = "VOLCANIC ASH CLOUD";
            } else if (theMatcher.group(1).equals(" MTW ")) {
                retHazardType = "MARKED MOUNTAIN WAVES";
            } else if (theMatcher.group(1).equals(" TR ")) {
                retHazardType = "TROPICAL STORM";
            } else if (theMatcher.group(1).equals(" GR ")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals(" TC ")) {
                retHazardType = "TROPICAL CYCLONE";
            } else if (theMatcher.group(1).equals(" CT ")) {
                retHazardType = "CAT";
            } else if (theMatcher.group(1).equals(" CAT ")) {
                retHazardType = "CAT";
            } else if (theMatcher.group(1).equals(" DS ")) {
                retHazardType = "DUSTSTORM";
            } else if (theMatcher.group(1).equals(" SS ")) {
                retHazardType = "SANDSTORM";
            } else if (theMatcher.group(1).equals(" WS ")) {
                retHazardType = "LOW LEVEL WIND SHEAR";
            } else if (theMatcher.group(1).equals("LLWS")) {
                retHazardType = "LOW LEVEL WIND SHEAR";
            } else if (theMatcher.group(1).equals(" TSGR ")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals("VOLCANIC ASH")) {
                retHazardType = "VOLCANIC ASH CLOUD";
            } else if (theMatcher.group(1).equals("TS ")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals("HVYSS ")) {
                retHazardType = "HEAVY SANDSTORM";
            } else if (theMatcher.group(1).equals("WATERSPOUTS")) {
                retHazardType = "WATERSPOUTS";
            } else if (theMatcher.group(1).equals("THUNDERSTORMS")) {
                retHazardType = "THUNDERSTORMS";
            } else if (theMatcher.group(1).equals("WIND")) {
                retHazardType = "WINDS";
            } else if (theMatcher.group(1).equals("TS OBS")) {
                retHazardType = "THUNDERSTORMS";
            } else if (theMatcher.group(1).equals(" TS")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals("CB/TS")) {
                retHazardType = getThunderStorm(theReport);
            } else if (theMatcher.group(1).equals("TORNADO")) {
                retHazardType = "TORNADO";
            }
        } else if (nilMatcher.find()) {
            retHazardType = "NIL";
        } else if (misMatcher.find()) {
            retHazardType = "NIL";
        } else {
            retHazardType = "UNKNOWN ???";
        }
        return retHazardType;
    }

    /**
     * Obtains the message ID.
     * 
     * @param theBulletin
     *            The input bulletin
     * @return a string for messageID
     */
    public static String getMessageID(String theBulletin) {

        String messageID = " ";

        // // Regular expression for CONUS message ID
        // final String CONUS_EXP =
        // "([A-Z]{4}) SIGMET (ALFA|BRAVO|CHARLIE|DELTA|ECHO|FOXTROT|" +
        // "GOLF|HOTEL|INDIA|JULIETT|JULIET|KILO|LIMA|MIKE|NOVEMBER|OSCAR|PAPA|QUEBEC|"
        // +
        // "ROMEO|SIERRA|TANGO|UNIFORM|VICTOR|WHISKEY|XRAY|YANKEE|ZULU) " +
        // "([0-9]{1}|[0-9]{2}) VALID";

        // Pattern used for extracting the message ID
        final Pattern conusPattern = Pattern.compile(CONUS_EXP);
        Matcher idconusMatcher = conusPattern.matcher(theBulletin);

        // Regular expression for CANADA message ID
        final String CANADA_EXP = "SIGMET ([A-Z]{1})([0-9]{1}|[0-9]{2}) (VALID "
                + "([0-9]{6})/|CANCELLED AT )([0-9]{6}) ([A-Z]{4})-";
        // Pattern used for extracting the message ID
        final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
        Matcher idcanadaMatcher = canadaPattern.matcher(theBulletin);

        // Regular expression for JAPAN and UK message ID
        final String JAPAN_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}) (VALID )?"
                + "([0-9]{6})/([0-9]{6})(UTC)? ([A-Z]{4})-";
        // Pattern used for extracting the message ID
        final Pattern japanPattern = Pattern.compile(JAPAN_EXP);
        Matcher idjapanMatcher = japanPattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER1_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID ([0-9]{6})/([0-9]{6})( UTC)? ([A-Z]{4})(-)?";
        // Pattern used for extracting the message ID
        final Pattern other1Pattern = Pattern.compile(OTHER1_EXP);
        Matcher idother1Matcher = other1Pattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER2_EXP = "(SIGMET) ([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}|[A-Z]{1}[0-9]{2}) "
                + "VALID ([0-9]{6})/([0-9]{6})";
        // Pattern used for extracting the message ID
        final Pattern other2Pattern = Pattern.compile(OTHER2_EXP);
        Matcher idother2Matcher = other2Pattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER3_EXP = "([A-Z]{4}) SIGMET ([A-Z]* )?([0-9]{1}|[0-9]{2}) VALID ([0-9]{6})/([0-9]{6}) "
                + "([A-Z]{4})(-)?";
        // Pattern used for extracting the message ID
        final Pattern other3Pattern = Pattern.compile(OTHER3_EXP);
        Matcher idother3Matcher = other3Pattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER4_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message ID
        final Pattern other4Pattern = Pattern.compile(OTHER4_EXP);
        Matcher idother4Matcher = other4Pattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER5_EXP = "([A-Z]{4}) SIGMET( )*VALID( )*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message ID
        final Pattern other5Pattern = Pattern.compile(OTHER5_EXP);
        Matcher idother5Matcher = other5Pattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER6_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{6})Z/([0-9]{6})Z( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message ID
        final Pattern other6Pattern = Pattern.compile(OTHER6_EXP);
        Matcher idother6Matcher = other6Pattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER7_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{4})/([0-9]{4})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message ID
        final Pattern other7Pattern = Pattern.compile(OTHER7_EXP);
        Matcher idother7Matcher = other7Pattern.matcher(theBulletin);

        // Regular expression for others' message ID
        final String OTHER8_EXP = "([A-Z]{4}) SIGMET( |\\W)*VALID( |\\W)*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message ID
        final Pattern other8Pattern = Pattern.compile(OTHER8_EXP);
        Matcher idother8Matcher = other8Pattern.matcher(theBulletin);

        // Get messageID
        if (idconusMatcher.find()) {
            messageID = idconusMatcher.group(2);
        } else if (idcanadaMatcher.find()) {
            messageID = idcanadaMatcher.group(6);
        } else if (idother1Matcher.find()) {
            messageID = idother1Matcher.group(6);
        } else if (idjapanMatcher.find()) {
            messageID = idjapanMatcher.group(7);
        } else if (idother2Matcher.find()) {
            messageID = idother2Matcher.group(1);
        } else if (idother3Matcher.find()) {
            messageID = idother3Matcher.group(6);
        } else if (idother4Matcher.find()) {
            messageID = idother4Matcher.group(8);
        } else if (idother5Matcher.find()) {
            messageID = idother5Matcher.group(7);
        } else if (idother6Matcher.find()) {
            messageID = idother6Matcher.group(8);
        } else if (idother7Matcher.find()) {
            messageID = idother7Matcher.group(8);
        } else if (idother8Matcher.find()) {
            messageID = idother8Matcher.group(7);
        }

        return messageID;
    }

    /**
     * Obtains the sequence number.
     * 
     * @param theBulletin
     *            The input bulletin
     * @return a string for sequenceNumber
     */
    public static String getSequenceNumber(String theBulletin) {

        String sequenceNumber = " ";

        // // Regular expression for CONUS sequence number
        // final String CONUS_EXP =
        // "([A-Z]{4}) SIGMET (ALFA|BRAVO|CHARLIE|DELTA|ECHO|FOXTROT|GOLF|HOTEL|"
        // +
        // "INDIA|JULIETT|JULIET|KILO|LIMA|MIKE|NOVEMBER|OSCAR|PAPA|QUEBEC|ROMEO|SIERRA|TANGO|"
        // +
        // "UNIFORM|VICTOR|WHISKEY|XRAY|YANKEE|ZULU) ([0-9]{1}|[0-9]{2}) VALID";

        // Pattern used for extracting the sequence number
        final Pattern conusPattern = Pattern.compile(CONUS_EXP);
        Matcher seqnoconusMatcher = conusPattern.matcher(theBulletin);

        // Regular expression for CANADA sequence number
        final String CANADA_EXP = "SIGMET ([A-Z]{1}[0-9]{1}|[A-Z]{1}[0-9]{2}) "
                + "(VALID ([0-9]{6})/|CANCELLED AT )([0-9]{6}) ([A-Z]{4})-";
        // Pattern used for extracting the sequence number
        final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
        Matcher seqnocanadaMatcher = canadaPattern.matcher(theBulletin);

        // Regular expression for JAPAN and UK sequence number
        final String JAPAN_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}) "
                + "(VALID )?([0-9]{6})/([0-9]{6})(UTC)? ([A-Z]{4})-";
        // Pattern used for extracting the sequence number
        final Pattern japanPattern = Pattern.compile(JAPAN_EXP);
        Matcher seqnojapanMatcher = japanPattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID ([0-9]{6})/([0-9]{6})( UTC)? ([A-Z]{4})(-)?";
        // Pattern used for extracting the sequence number
        final Pattern otherPattern = Pattern.compile(OTHER_EXP);
        Matcher seqnootherMatcher = otherPattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER2_EXP = "SIGMET ([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}|A-Z]{1}[0-9]{2}) "
                + "VALID ([0-9]{6})/([0-9]{6})";
        // Pattern used for extracting the sequence number
        final Pattern other2Pattern = Pattern.compile(OTHER2_EXP);
        Matcher seqnoother2Matcher = other2Pattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER3_EXP = "([A-Z]{4}) SIGMET (([A-Z]* )?([0-9]{1}|[0-9]{2})) "
                + "VALID ([0-9]{6})/([0-9]{6}) ([A-Z]{4})(-)?";
        // Pattern used for extracting the sequence number
        final Pattern other3Pattern = Pattern.compile(OTHER3_EXP);
        Matcher seqnoother3Matcher = other3Pattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER4_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the sequence number
        final Pattern other4Pattern = Pattern.compile(OTHER4_EXP);
        Matcher seqnoother4Matcher = other4Pattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER5_EXP = "([A-Z]{4}) SIGMET( )*VALID( )*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the sequence number
        final Pattern other5Pattern = Pattern.compile(OTHER5_EXP);
        Matcher seqnoother5Matcher = other5Pattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER6_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{6})Z/([0-9]{6})Z( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message sequence number
        final Pattern other6Pattern = Pattern.compile(OTHER6_EXP);
        Matcher seqnoother6Matcher = other6Pattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER7_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{4})/([0-9]{4})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the sequence number
        final Pattern other7Pattern = Pattern.compile(OTHER7_EXP);
        Matcher seqnoother7Matcher = other7Pattern.matcher(theBulletin);

        // Regular expression for others' sequence number
        final String OTHER8_EXP = "([A-Z]{4}) SIGMET( |\\W)*([0-9]{1})( |\\W)*VALID";
        // Pattern used for extracting the sequence number
        final Pattern other8Pattern = Pattern.compile(OTHER8_EXP);
        Matcher seqnoother8Matcher = other8Pattern.matcher(theBulletin);

        // Get sequenceNumber
        if (seqnoconusMatcher.find()) {
            sequenceNumber = seqnoconusMatcher.group(3);
        } else if (seqnocanadaMatcher.find()) {
            sequenceNumber = seqnocanadaMatcher.group(1);
        } else if (seqnootherMatcher.find()) {
            sequenceNumber = seqnootherMatcher.group(2);
        } else if (seqnojapanMatcher.find()) {
            sequenceNumber = seqnojapanMatcher.group(2);
        } else if (seqnoother2Matcher.find()) {
            sequenceNumber = seqnoother2Matcher.group(1);
        } else if (seqnoother3Matcher.find()) {
            sequenceNumber = seqnoother3Matcher.group(2);
        } else if (seqnoother4Matcher.find()) {
            sequenceNumber = seqnoother4Matcher.group(3);
        } else if (seqnoother5Matcher.find()) {
            // lacking sequence number; default 0
            sequenceNumber = "0";
        } else if (seqnoother6Matcher.find()) {
            sequenceNumber = seqnoother6Matcher.group(3);
        } else if (seqnoother7Matcher.find()) {
            sequenceNumber = seqnoother7Matcher.group(3);
        } else if (seqnoother8Matcher.find()) {
            sequenceNumber = seqnoother8Matcher.group(3);
        }

        return sequenceNumber;
    }

    /**
     * Obtains the Air Traffic Service Unit.
     * 
     * @param theBulletin
     *            The input bulletin
     * @return a string for atsu.
     */
    public static String getAtsu(String theBulletin) {

        String atsu = null;

        // // Regular expression for CONUS ATSU
        // final String CONUS_EXP =
        // "([A-Z]{4}) SIGMET (ALFA|BRAVO|CHARLIE|DELTA|ECHO|FOXTROT|GOLF|" +
        // "HOTEL|INDIA|JULIETT|JULIET|KILO|LIMA|MIKE|NOVEMBER|OSCAR|PAPA|QUEBEC|ROMEO|SIERRA|"
        // +
        // "TANGO|UNIFORM|VICTOR|WHISKEY|XRAY|YANKEE|ZULU) ([0-9]{1}|[0-9]{2}) VALID";

        // Pattern used for extracting the ATSU
        final Pattern conusPattern = Pattern.compile(CONUS_EXP);
        Matcher atsuconusMatcher = conusPattern.matcher(theBulletin);

        // Regular expression for CANADA ATSU
        final String CANADA_EXP = "SIGMET ([A-Z]{1}[0-9]{1}|[A-Z]{1}[0-9]{2}) "
                + "(VALID ([0-9]{6})/|CANCELLED AT )([0-9]{6}) ([A-Z]{4})-";
        // Pattern used for extracting the ATSU
        final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
        Matcher atsucanadaMatcher = canadaPattern.matcher(theBulletin);

        // Regular expression for JAPAN and UK ATSU
        final String JAPAN_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}) "
                + "(VALID )?([0-9]{6})/([0-9]{6})(UTC)? ([A-Z]{4})-";
        // Pattern used for extracting the ATSU
        final Pattern japanPattern = Pattern.compile(JAPAN_EXP);
        Matcher atsujapanMatcher = japanPattern.matcher(theBulletin);

        // Regular expression for others' ATSU
        final String OTHER1_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2})"
                + "( )*VALID";
        // Pattern used for extracting the ATSU
        final Pattern other1Pattern = Pattern.compile(OTHER1_EXP);
        Matcher atsuother1Matcher = other1Pattern.matcher(theBulletin);

        // Regular expression for CONUS ATSU
        final String CONUS2_EXP = "([A-Z]{4} ([A-Z]{4} )*)SIGMET (ALFA|BRAVO|CHARLIE|DELTA|ECHO|FOXTROT|"
                + "GOLF|HOTEL|INDIA|JULIETT|JULIET|KILO|LIMA|MIKE|NOVEMBER|OSCAR|PAPA|QUEBEC|ROMEO|SIERRA|"
                + "TANGO|UNIFORM|VICTOR|WHISKEY|XRAY|YANKEE|ZULU) ([0-9]{1}|[0-9]{2}) VALID";
        // Pattern used for extracting the ATSU
        final Pattern conus2Pattern = Pattern.compile(CONUS2_EXP);
        Matcher atsuconus2Matcher = conus2Pattern.matcher(theBulletin);

        final String OTHER2_EXP = "([A-Z]{4}) SIGMET( )*VALID( )*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the ATSU
        final Pattern other2Pattern = Pattern.compile(OTHER2_EXP);
        Matcher atsuother2Matcher = other2Pattern.matcher(theBulletin);

        // Regular expression for others' ATSU
        final String OTHER3_EXP = "([A-Z]{4}) SIGMET ([0-9]{1})( |\\W)*VALID( |\\W)*"
                + "([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the ATSU
        final Pattern other3Pattern = Pattern.compile(OTHER3_EXP);
        Matcher atsuother3Matcher = other3Pattern.matcher(theBulletin);

        // Get atsu
        if (atsuconus2Matcher.find()) {
            atsu = atsuconus2Matcher.group(1);
        } else if (atsuconusMatcher.find()) {
            atsu = atsuconusMatcher.group(1);
        } else if (atsucanadaMatcher.find()) {
            atsu = atsucanadaMatcher.group(5);
        } else if (atsuother1Matcher.find()) {
            atsu = atsuother1Matcher.group(1);
        } else if (atsujapanMatcher.find()) {
            atsu = atsujapanMatcher.group(1);
        } else if (atsuother2Matcher.find()) {
            atsu = atsuother2Matcher.group(1);
        } else if (atsuother3Matcher.find()) {
            atsu = atsuother3Matcher.group(1);
        }

        return atsu;
    }

    /**
     * Obtains the location indicator of the meteorological watch office
     * originating the message.
     * 
     * @param theBulletin
     *            The input bulletin
     * @return a string for omwo.
     */
    public static String getOmwo(String theBulletin) {

        String omwo = " ";

        // Regular expression for CONUS OMWO
        final String CONUS_EXP = "([A-Z]{4}) ([A-Z]{4} )?SIGMET ([A-Z])* ([0-9]{1}|[0-9]{2}) "
                + "VALID ([0-9]{6})/([0-9]{6})( UTC)? ([A-Z]{4})-";
        // Pattern used for extracting the OMWO
        final Pattern conusPattern = Pattern.compile(CONUS_EXP);
        Matcher omwoconusMatcher = conusPattern.matcher(theBulletin);

        // Regular expression for CANADA OMWO
        final String CANADA_EXP = "SIGMET ([A-Z]{1}[0-9]{1}|[A-Z]{1}[0-9]{2}) "
                + "(VALID ([0-9]{6})/|CANCELLED AT )([0-9]{6}) ([A-Z]{4})-";
        // Pattern used for extracting the OMWO
        final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
        Matcher omwocanadaMatcher = canadaPattern.matcher(theBulletin);

        // Regular expression for JAPAN and UK OMWO
        final String JAPAN_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}) "
                + "(VALID )?([0-9]{6})/([0-9]{6})(UTC)? ([A-Z]{4})-";
        // Pattern used for extracting the OMWO
        final Pattern japanPattern = Pattern.compile(JAPAN_EXP);
        Matcher omwojapanMatcher = japanPattern.matcher(theBulletin);

        // Regular expression for other OMWO
        final String OTHER_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID ([0-9]{6})/([0-9]{6})( UTC)? ([A-Z]{4})(-)?";
        // Pattern used for extracting the OMWO
        final Pattern otherPattern = Pattern.compile(OTHER_EXP);
        Matcher omwootherMatcher = otherPattern.matcher(theBulletin);

        // Regular expression for others' OMWO
        final String OTHER2_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|"
                + "[A-Z]{2}[0-9]{2}|[A-Z]{1}[0-9]{2}) "
                + "VALID( )*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message OMWO
        final Pattern other2Pattern = Pattern.compile(OTHER2_EXP);
        Matcher omwoother2Matcher = other2Pattern.matcher(theBulletin);

        // Regular expression for others' OMWO
        final String OTHER3_EXP = "([A-Z]{4}) SIGMET( )*VALID( )*([0-9]{6})/([0-9]{6})"
                + "( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message OMWO
        final Pattern other3Pattern = Pattern.compile(OTHER3_EXP);
        Matcher omwoother3Matcher = other3Pattern.matcher(theBulletin);

        // Regular expression for others' OMWO
        final String OTHER4_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{4})/([0-9]{4})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message OMWO
        final Pattern other4Pattern = Pattern.compile(OTHER4_EXP);
        Matcher omwoother4Matcher = other4Pattern.matcher(theBulletin);

        // Regular expression for others' OMWO
        final String OTHER5_EXP = "([A-Z]{4}) SIGMET( )*([0-9]{1}|[0-9]{2}|[A-Z]{1}[0-9]{1}|[A-Z]{2}[0-9]{2}) "
                + "VALID( )*([0-9]{6})Z/([0-9]{6})Z( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the message OMWO
        final Pattern other5Pattern = Pattern.compile(OTHER5_EXP);
        Matcher omwoother5Matcher = other5Pattern.matcher(theBulletin);

        // Regular expression for others' OMWO
        final String OTHER6_EXP = "([A-Z]{4}) SIGMET ([0-9]{1})( |\\W)*"
                + "VALID( |\\W)*([0-9]{6})/([0-9]{6})( )*([A-Z]{4})(-)?";
        // Pattern used for extracting the OMWO
        final Pattern other6Pattern = Pattern.compile(OTHER6_EXP);
        Matcher omwoother6Matcher = other6Pattern.matcher(theBulletin);

        // Get omwo
        if (omwoconusMatcher.find()) {
            omwo = omwoconusMatcher.group(8);
        } else if (omwocanadaMatcher.find()) {
            omwo = omwocanadaMatcher.group(5);
        } else if (omwootherMatcher.find()) {
            omwo = omwootherMatcher.group(6);
        } else if (omwojapanMatcher.find()) {
            omwo = omwojapanMatcher.group(7);
        } else if (omwoother2Matcher.find()) {
            omwo = omwoother2Matcher.group(8);
        } else if (omwoother3Matcher.find()) {
            omwo = omwoother3Matcher.group(7);
        } else if (omwoother4Matcher.find()) {
            omwo = omwoother4Matcher.group(8);
        } else if (omwoother5Matcher.find()) {
            omwo = omwoother5Matcher.group(8);
        } else if (omwoother6Matcher.find()) {
            omwo = omwoother6Matcher.group(8);
        }

        return omwo;
    }

    /**
     * Obtains start time from input bulletin.
     * 
     * @param theReport
     *            The bulletin message
     * @return a calendar for start time
     */
    public static Calendar getStartTime(String theBulletin, Headers headers) {

        String time = "???";

        // Regular expression for general start time group
        final String START_EXP = " VALID( )*([0-9]{6})/([0-9]{6})";
        // Pattern used for extracting the start time group
        final Pattern startPattern = Pattern.compile(START_EXP);
        Matcher timestartMatcher = startPattern.matcher(theBulletin);

        // Regular expression for CANADA start time group
        final String CANADA_EXP = "SIGMET ([A-Z]{1}[0-9]{1}) VALID ([0-9]{6})/([0-9]{6}) ([A-Z]{4})-";
        // Pattern used for extracting the start time group
        final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
        Matcher timecanadaMatcher = canadaPattern.matcher(theBulletin);

        // Regular expression for JAPAN and UK start time group
        final String JAPAN_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}) "
                + "(VALID )?([0-9]{6})/([0-9]{6})(UTC)? ([A-Z]{4})-";
        // Pattern used for extracting the start time group
        final Pattern japanPattern = Pattern.compile(JAPAN_EXP);
        Matcher timejapanMatcher = japanPattern.matcher(theBulletin);

        // Regular expression for others' start time
        final String OTHER1_EXP = " VALID( )*([0-9]{6})(Z)?/([0-9]{6})(Z)?";
        // Pattern used for extracting the start time
        final Pattern other1Pattern = Pattern.compile(OTHER1_EXP);
        Matcher time1Matcher = other1Pattern.matcher(theBulletin);

        // Regular expression for others' start time
        final String OTHER2_EXP = " VALID( )*([0-9]{4})/([0-9]{4})";
        // Pattern used for extracting the start time
        final Pattern other2Pattern = Pattern.compile(OTHER2_EXP);
        Matcher time2Matcher = other2Pattern.matcher(theBulletin);

        // Get time group
        if (timestartMatcher.find()) {
            time = timestartMatcher.group(2);
        } else if (timejapanMatcher.find()) {
            time = timejapanMatcher.group(4);
        } else if (time1Matcher.find()) {
            time = time1Matcher.group(2);
        } else if (timecanadaMatcher.find()) {
            time = timecanadaMatcher.group(2);
        } else if (time2Matcher.find()) {
            // Handle start time in special case without day
            final String WMO_EXP = "([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{2})([0-9]{4})";
            // Pattern used for extracting WMO header, officeID, and issue date
            final Pattern wmoPattern = Pattern.compile(WMO_EXP);
            Matcher theMatcher = wmoPattern.matcher(theBulletin);
            if (theMatcher.find()) {
                time = time2Matcher.group(2).concat(theMatcher.group(3));
            }
        }
        if (time != "???") {
            // Get start time
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            return WMOTimeParser.findDataTime(time, fileName);
        } else {
            return null;
        }
    }

    /**
     * Get the end time
     * 
     * @param theBulletin
     *            The bulletin which contains end time
     * @return a calendar for end time
     */
    public static Calendar getEndTime(String theBulletin, Headers headers) {

        String time = "???";

        // Regular expression for CONUS end time group
        final String ENDTIME_EXP = " VALID( )*([0-9]{6})/([0-9]{6})";
        // Pattern used for extracting the end time group
        final Pattern endtimePattern = Pattern.compile(ENDTIME_EXP);
        Matcher endtimeMatcher = endtimePattern.matcher(theBulletin);

        // Regular expression for CANADA end time group
        final String CANADA_EXP = "SIGMET ([A-Z]{1}[0-9]{1}) "
                + "(VALID ([0-9]{6})/|CANCELLED AT )([0-9]{6}) ([A-Z]{4})-";
        // Pattern used for extracting the end time group
        final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
        Matcher timecanadaMatcher = canadaPattern.matcher(theBulletin);

        // Regular expression for JAPAN and UK end time group
        final String JAPAN_EXP = "([A-Z]{4}) SIGMET ([0-9]{1}|[0-9]{2}) "
                + "(VALID )?([0-9]{6})/([0-9]{6})(UTC)? ([A-Z]{4})-";
        // Pattern used for extracting the end time group
        final Pattern japanPattern = Pattern.compile(JAPAN_EXP);
        Matcher timejapanMatcher = japanPattern.matcher(theBulletin);

        // Regular expression for others' end time
        final String OTHER1_EXP = " VALID( )*([0-9]{6})(Z)?/([0-9]{6})(Z)?";
        // Pattern used for extracting the end time
        final Pattern other1Pattern = Pattern.compile(OTHER1_EXP);
        Matcher time1Matcher = other1Pattern.matcher(theBulletin);

        // Regular expression for others' end time
        final String OTHER2_EXP = " VALID( )*([0-9]{4})/([0-9]{4})";
        // Pattern used for extracting the end time
        final Pattern other2Pattern = Pattern.compile(OTHER2_EXP);
        Matcher time2Matcher = other2Pattern.matcher(theBulletin);

        // Get time group
        if (endtimeMatcher.find()) {
            time = endtimeMatcher.group(3);
        } else if (timejapanMatcher.find()) {
            time = timejapanMatcher.group(4);
        } else if (timecanadaMatcher.find()) {
            time = timecanadaMatcher.group(4);
        } else if (timecanadaMatcher.find()) {
            time = timecanadaMatcher.group(6);
        } else if (time1Matcher.find()) {
            time = time1Matcher.group(4);
        } else if (time2Matcher.find()) {
            // Handle end time special case without day
            final String WMO_EXP = "([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{2})([0-9]{4})";
            // Pattern used for extracting WMO header, officeID and issue time
            final Pattern wmoPattern = Pattern.compile(WMO_EXP);
            Matcher theMatcher = wmoPattern.matcher(theBulletin);
            if (theMatcher.find()) {
                time = time2Matcher.group(3).concat(theMatcher.group(3));
            }
        }

        if (time != "???") {
            // Get end time
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            return WMOTimeParser.findDataTime(time, fileName);
        } else {
            return null;
        }
    }

    /**
     * Parse the bulletin and store flight level 1 or level 2 if any.
     * 
     * @param theBulletin
     *            The bulletin message
     * @param record
     *            The main table
     */
    public static void processFlightLevels(String theBulletin,
            IntlSigmetRecord record) {

        String flevel1 = " ";
        String flevel2 = " ";

        // Regular expression for general one flight level
        final String FLEVEL1_EXP = "(TOP|TOPS|TOPS TO) FL( )?([0-9]{3})";
        // Pattern used for extracting the flight level
        final Pattern flevel1Pattern = Pattern.compile(FLEVEL1_EXP);
        Matcher flevel1Matcher = flevel1Pattern.matcher(theBulletin);

        // Regular expression for flight levels in format "BETWEEN ... AND ...."
        final String BETWEEN_EXP = "(BETWEEN|BTN) (FL)?([0-9]{3}|[0-9]{2}) AND "
                + "(FL)?([0-9]{3}|[0-9]{2})";
        // Pattern used for extracting the flight levels
        final Pattern betweenPattern = Pattern.compile(BETWEEN_EXP);
        Matcher betweenMatcher = betweenPattern.matcher(theBulletin);

        // Regular expression for flight levels in format
        // "TOPS ... MAX TOPS ...." for Canada
        final String MAXTOPS_EXP = "TOPS( |\\x0d\\x0d\\x0a)([0-9]{3}|[0-9]{2}) "
                + "MAX TOPS ([0-9]{3}|[0-9]{2})";
        // Pattern used for extracting the flight levels
        final Pattern maxtopsPattern = Pattern.compile(MAXTOPS_EXP);
        Matcher maxtopsMatcher = maxtopsPattern.matcher(theBulletin);

        // Regular expression for general two flight levels in "360-440" or
        // "360/440" format
        final String FLEVEL2_EXP = "FL([0-9]{3}|[0-9]{2})(-|/)([0-9]{3}|[0-9]{2})";
        // Pattern used for extracting the flight levels
        final Pattern flevel2Pattern = Pattern.compile(FLEVEL2_EXP);
        Matcher flevel2Matcher = flevel2Pattern.matcher(theBulletin);

        // Regular expression for general one flight level
        final String FLEVEL3_EXP = "(TOP|TOPS|TOPS TO) (FL )?([0-9]{3})";
        // Pattern used for extracting the flight level
        final Pattern flevel3Pattern = Pattern.compile(FLEVEL3_EXP);
        Matcher flevel3Matcher = flevel3Pattern.matcher(theBulletin);

        final String FLEVEL4_EXP = "FL([0-9]{3})";
        // Pattern used for extracting the flight level
        final Pattern flevel4Pattern = Pattern.compile(FLEVEL4_EXP);
        Matcher flevel4Matcher = flevel4Pattern.matcher(theBulletin);

        // Get flight levels group
        if (maxtopsMatcher.find()) {
            flevel1 = maxtopsMatcher.group(3);
        } else if (flevel2Matcher.find()) {
            flevel1 = flevel2Matcher.group(1);
            flevel2 = flevel2Matcher.group(3);
        } else if (betweenMatcher.find()) {
            flevel1 = betweenMatcher.group(3);
            flevel2 = betweenMatcher.group(5);
        } else if (flevel1Matcher.find()) {
            flevel1 = flevel1Matcher.group(3);
        } else if (flevel3Matcher.find()) {
            flevel1 = flevel3Matcher.group(3);
        } else if (flevel4Matcher.find()) {
            flevel1 = flevel4Matcher.group(1);
        }

        // Set flight levels to database
        if (flevel1 != " ") {
            record.setFlightlevel1(Integer.parseInt(flevel1));
        }
        if (flevel2 != " ") {
            record.setFlightlevel2(Integer.parseInt(flevel2));
        }

        // Parse the locations
        processLocation(theBulletin, record);

    }

    /**
     * Obtains remarks from a bulletin
     * 
     * @param bullMessage
     *            The bulletin message
     * @return a string for remarks
     */
    public static String getRemarks(String bullMessage) {

        String retRemarks = " ";

        // Regular expression correction
        final String CORR_EXP = "(COR | (CC[A-Z]{1}))";

        // Pattern used for extracting correction
        final Pattern corrPattern = Pattern.compile(CORR_EXP);
        Matcher corrMatcher = corrPattern.matcher(bullMessage);

        // Regular expression amendment
        final String AMD_EXP = "(AMD  )";
        // Pattern used for extracting amendment
        final Pattern amdPattern = Pattern.compile(AMD_EXP);
        Matcher amdMatcher = amdPattern.matcher(bullMessage);

        // Regular expression for TEST
        final String TEST_EXP = "TEST ";
        // Pattern used for extracting test
        final Pattern testPattern = Pattern.compile(TEST_EXP);
        Matcher testMatcher = testPattern.matcher(bullMessage);

        if (corrMatcher.find()) {
            retRemarks = corrMatcher.group(1);
        } else if (amdMatcher.find()) {
            retRemarks = amdMatcher.group();
        } else if (testMatcher.find()) {
            retRemarks = "TEST";
        }

        return retRemarks;
    }

    /**
     * Obtains speed from a bulletin
     * 
     * @param bullMessage
     *            The bulletin message
     * @return an integer for speed
     */
    public static Integer getSpeed(String bullMessage) {

        Integer retSpeed = IDecoderConstantsN.INTEGER_MISSING;

        final String SPEED_EXP = "(MOVE|MOV|MOVG) (|E|N|S|W|\006|\012)* "
                + "([0-9]{1}|[0-9]{2})(-[0-9]{2})?( )?(KT)";
        // Pattern used for extracting speed for general
        final Pattern speedPattern = Pattern.compile(SPEED_EXP);
        Matcher theMatcher = speedPattern.matcher(bullMessage);

        final String CANADA1_EXP = "(MOVE |MOV |MOVG )?([ENSW]{1})WD "
                + "([0-9]{2}|[0-9]{1})( |\\x0d\\x0d\\x0a)?KT";
        // Pattern used for extracting speed for Canada
        final Pattern canada1Pattern = Pattern.compile(CANADA1_EXP);
        Matcher canada1Matcher = canada1Pattern.matcher(bullMessage);

        final String CANADA2_EXP = "(MOVE |MOV |MOVG )?([ENSW]{2})WD "
                + "([0-9]{2}|[0-9]{1})(\\x0d\\x0d\\x0a| )?KT";
        // Pattern used for extracting speed for Canada
        final Pattern canada2Pattern = Pattern.compile(CANADA2_EXP);
        Matcher canada2Matcher = canada2Pattern.matcher(bullMessage);

        final String KMH_EXP = "([0-9]{2}|[0-9]{3})(( )*|( )?\\x0d\\x0d\\x0a)KMH";
        // Pattern used for extracting speed for KMH
        final Pattern kmhPattern = Pattern.compile(KMH_EXP);
        Matcher kmhMatcher = kmhPattern.matcher(bullMessage);

        if (theMatcher.find()) {
            retSpeed = Integer.parseInt(theMatcher.group(3));
        } else if (canada1Matcher.find()) {
            retSpeed = Integer.parseInt(canada1Matcher.group(3));
        } else if (canada2Matcher.find()) {
            retSpeed = Integer.parseInt(canada2Matcher.group(3));
        } else if (kmhMatcher.find()) {
            Integer kmhSpeed = Integer.parseInt(kmhMatcher.group(1));
            // Convert KMH to Knots
            float speed = (float) (((double) kmhSpeed * 1000.0 / 3600.0) * 1.9425);
            retSpeed = (int) speed;
        }

        return retSpeed;
    }

    /**
     * Obtains intensity from a bulletin
     * 
     * @param bullMessage
     *            The bulletin message
     * @return a string for intensity
     */
    public static String getIntensity(String bullMessage) {

        String intensity = null;

        // Regular expression for intensity
        final String INTENSITY_EXP = "( NC | INTSF| WKN | NC=|WKN=|NC.|WKNG )";
        // Pattern used for extracting intensityFlag
        final Pattern intensityPattern = Pattern.compile(INTENSITY_EXP);
        Matcher theMatcher = intensityPattern.matcher(bullMessage);

        if (theMatcher.find()) {
            intensity = theMatcher.group(1);
            intensity = UtilN.removeLeadingWhiteSpaces(intensity);
            if (intensity.substring(0, 2).equals("NC")) {
                intensity = "NC";
            } else if (intensity.substring(0, 3).equals("WKN")) {
                intensity = "WKN";
            }
        }

        return intensity;
    }

    /**
     * Obtains direction from a bulletin
     * 
     * @param bullMessage
     *            The bulletin message
     * @return a string for direction
     */
    public static String getDirection(String bullMessage) {

        String direction = null;

        final String CANADA1_EXP = "(MOVE |MOV |MOVG )?([ENSW]{1})WD "
                + "([0-9]{2}|[0-9]{1})( |\\x0d\\x0d\\x0a)?KT";
        // Pattern used for extracting direction for Canada
        final Pattern canada1Pattern = Pattern.compile(CANADA1_EXP);
        Matcher canada1Matcher = canada1Pattern.matcher(bullMessage);

        final String CANADA2_EXP = "(MOVE |MOV |MOVG )?([ENSW]{2})WD "
                + "([0-9]{2}|[0-9]{1})( |\\x0d\\x0d\\x0a)?KT";
        // Pattern used for extracting direction for Canada
        final Pattern canada2Pattern = Pattern.compile(CANADA2_EXP);
        Matcher canada2Matcher = canada2Pattern.matcher(bullMessage);

        // Regular expression for direction
        final String DIRECTION_EXP = "(MOVE|MOV|MOVG) ([ENSW]{3}|[ENSW]{2}|[ENSW]{1})";
        // Pattern used for extracting direction for general
        final Pattern directionPattern = Pattern.compile(DIRECTION_EXP);
        Matcher theMatcher = directionPattern.matcher(bullMessage);

        final String DIR2_EXP = "(MOVE|MOV|MOVG)(\\D)*([ENSW]{3}|[ENSW]{2}|[ENSW]{1}) ";
        // Pattern used for extracting direction for Canada
        final Pattern dir2Pattern = Pattern.compile(DIR2_EXP);
        Matcher dir2Matcher = dir2Pattern.matcher(bullMessage);

        final String DIR3_EXP = "(MOVE|MOV|MOVG) (SOUTH|NORTH|WEST|EAST) ";
        // Pattern used for extracting direction for Canada
        final Pattern dir3Pattern = Pattern.compile(DIR3_EXP);
        Matcher dir3Matcher = dir3Pattern.matcher(bullMessage);

        if (canada2Matcher.find()) {
            direction = canada2Matcher.group(2);
        } else if (canada1Matcher.find()) {
            direction = canada1Matcher.group(2);
        } else if (theMatcher.find()) {
            direction = theMatcher.group(2);
        } else if (dir2Matcher.find()) {
            direction = dir2Matcher.group(3);
        } else if (dir3Matcher.find()) {
            direction = dir3Matcher.group(2);
            if (direction.equals("SOUTH")) {
                direction = "S";
            } else if (direction.equals("NORTH")) {
                direction = "N";
            } else if (direction.equals("WEST")) {
                direction = "W";
            } else if (direction.equals("EAST")) {
                direction = "E";
            }
        }
        return direction;
    }

    /**
     * Parse the location to set Lat/Lon or location name
     * 
     * @param theLocation
     *            The location with lat/lon or location name
     * @param locTb
     *            The location table
     */
    public static void processLatLon(String theLocation,
            IntlSigmetLocation locTb, Integer index, IntlSigmetRecord record) {

    	double flat, flon;

        final String LATLON1_EXP = "(N|S)([0-9]{2})([0-9]{2}) (E|W)([0-9]{3})([0-9]{2})";
        // Pattern used for extracting latlon - CONUS
        final Pattern latlon1Pattern = Pattern.compile(LATLON1_EXP);
        Matcher latlon1Matcher = latlon1Pattern.matcher(theLocation);

        final String LATLON2_EXP = "/([0-9]{2})([0-9]{2})(N|S)([0-9]{3})"
                + "([0-9]{2})(E|W)/((\\S|\\s|\\D)*)";
        // Pattern used for extracting latlon - Canada
        final Pattern latlon2Pattern = Pattern.compile(LATLON2_EXP);
        Matcher latlon2Matcher = latlon2Pattern.matcher(theLocation);

        final String LATLON3_EXP = "(N|S)([0-9]{2})([0-9]{2})(E|W)([0-9]{3})([0-9]{2})";
        // Pattern used for extracting latlon
        final Pattern latlon3Pattern = Pattern.compile(LATLON3_EXP);
        Matcher latlon3Matcher = latlon3Pattern.matcher(theLocation);

        final String LATLON4_EXP = "(N|S)([0-9]{2})(E|W)([0-9]{3}|[0-9]{2})";
        // Pattern used for extracting latlon
        final Pattern latlon4Pattern = Pattern.compile(LATLON4_EXP);
        Matcher latlon4Matcher = latlon4Pattern.matcher(theLocation);

        final String LATLON5_EXP = "([0-9]{2})(N|S)([0-9]{3}|[0-9]{2})(E|W)";
        // Pattern used for extracting latlon - Japan
        final Pattern latlon5Pattern = Pattern.compile(LATLON5_EXP);
        Matcher latlon5Matcher = latlon5Pattern.matcher(theLocation);

        final String LATLON6_EXP = "([0-9]{2})([0-9]{2})(N|S)([0-9]{3})([0-9]{2})(E|W)";
        // Pattern used for extracting latlon
        final Pattern latlon6Pattern = Pattern.compile(LATLON6_EXP);
        Matcher latlon6Matcher = latlon6Pattern.matcher(theLocation);

        final String LATLON7_EXP = "(N|S)([0-9]{2})([0-9]{2})[\\W| ]*(E|W)"
                + "([0-9]{3})([0-9]{2})";
        // Pattern used for extracting latlon
        final Pattern latlon7Pattern = Pattern.compile(LATLON7_EXP);
        Matcher latlon7Matcher = latlon7Pattern.matcher(theLocation);

        final String LATLON8_EXP = "(N|S)([0-9]{2})( )*(E|W)([0-9]{3}|[0-9]{2})";
        // Pattern used for extracting latlon
        final Pattern latlon8Pattern = Pattern.compile(LATLON8_EXP);
        Matcher latlon8Matcher = latlon8Pattern.matcher(theLocation);

        double NSFlag = 1.0;
        double EWFlag = 1.0;
        double seconds = 60.0;
        LatLonPoint point = null;

        if (record.getWmoHeader().equals("WSMC31")) {
            // WSMC31 issues lat/lon in decimals.
            seconds = 100.0;
        }

        if (latlon1Matcher.find()) {
            // latlon format for CONUS
            if (latlon1Matcher.group(1).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag
                    * (Integer.parseInt(latlon1Matcher.group(2)) + (Integer
                            .parseInt(latlon1Matcher.group(3)) / seconds));
            if (latlon1Matcher.group(4).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag
                    * (Integer.parseInt(latlon1Matcher.group(5)) + (Integer
                            .parseInt(latlon1Matcher.group(6)) / seconds));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon1Matcher.group());
        } else if (latlon2Matcher.find()) {
            // latlon format for Canada
            if (latlon2Matcher.group(3).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag
                    * (Integer.parseInt(latlon2Matcher.group(1)) + (Integer
                            .parseInt(latlon2Matcher.group(2)) / seconds));
            if (latlon2Matcher.group(6).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag
                    * (Integer.parseInt(latlon2Matcher.group(4)) + (Integer
                            .parseInt(latlon2Matcher.group(5)) / seconds));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon2Matcher.group(7));
        } else if (latlon3Matcher.find()) {
            if (latlon3Matcher.group(1).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag
                    * (Integer.parseInt(latlon3Matcher.group(2)) + (Integer
                            .parseInt(latlon3Matcher.group(3)) / seconds));
            if (latlon3Matcher.group(4).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag
                    * (Integer.parseInt(latlon3Matcher.group(5)) + (Integer
                            .parseInt(latlon3Matcher.group(6)) / seconds));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon3Matcher.group());
        } else if (latlon4Matcher.find()) {
            if (latlon4Matcher.group(1).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag * (Integer.parseInt(latlon4Matcher.group(2)));
            if (latlon4Matcher.group(3).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag * (Integer.parseInt(latlon4Matcher.group(4)));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon4Matcher.group());
        } else if (latlon5Matcher.find()) {
            if (latlon5Matcher.group(2).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag * (Integer.parseInt(latlon5Matcher.group(1)));
            if (latlon5Matcher.group(4).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag * (Integer.parseInt(latlon5Matcher.group(3)));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon5Matcher.group());
        } else if (latlon6Matcher.find()) {
            // latlon6 format
            if (latlon6Matcher.group(3).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag
                    * (Integer.parseInt(latlon6Matcher.group(1)) + (Integer
                            .parseInt(latlon6Matcher.group(2)) / seconds));
            if (latlon6Matcher.group(6).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag
                    * (Integer.parseInt(latlon6Matcher.group(4)) + (Integer
                            .parseInt(latlon6Matcher.group(5)) / seconds));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon6Matcher.group());
        } else if (latlon7Matcher.find()) {
            // latlon format
            if (latlon7Matcher.group(1).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag
                    * (Integer.parseInt(latlon7Matcher.group(2)) + (Integer
                            .parseInt(latlon7Matcher.group(3)) / seconds));
            if (latlon7Matcher.group(4).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag
                    * (Integer.parseInt(latlon7Matcher.group(5)) + (Integer
                            .parseInt(latlon7Matcher.group(6)) / seconds));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon7Matcher.group());
        } else if (latlon8Matcher.find()) {
            if (latlon8Matcher.group(1).equals("S")) {
                NSFlag = -1.0;
            }
            flat = NSFlag * (Integer.parseInt(latlon8Matcher.group(2)));
            if (latlon8Matcher.group(4).equals("W")) {
                EWFlag = -1.0;
            }
            flon = EWFlag * (Integer.parseInt(latlon8Matcher.group(5)));
            locTb.setLatitude(flat);
            locTb.setLongitude(flon);
            locTb.setLocationName(latlon8Matcher.group());
        } else {
            System.out.println("theLocation=" + theLocation);            
            if ((index == 0)
                    && ((theLocation.substring(0, 3).equals("IN ")) || theLocation
                            .substring(0, 3).equals("OF "))) {
                // handle special case with "IN " or "OF " and location
                String retLoc = theLocation.substring(3);
                locTb.setLocationName(retLoc);
                // Get a latLonPoint for this station ID from "vors" location
                // table
                point = LatLonLocTbl.getLatLonPoint(retLoc, "vors");
                if (point != null) {
	                locTb.setLatitude(point.getLatitude(LatLonPoint.INDEGREES));
	                locTb.setLongitude(point.getLongitude(LatLonPoint.INDEGREES));
                }
            } else {
                locTb.setLocationName(theLocation);
                // Get a latLonPoint for this station ID from "vors" location
                // table
                point = LatLonLocTbl.getLatLonPoint(theLocation, "vors");
                if (point != null) {
	                locTb.setLatitude(point.getLatitude(LatLonPoint.INDEGREES));
	                locTb.setLongitude(point.getLongitude(LatLonPoint.INDEGREES));
                }

            }
        }
    }

    /**
     * Obtains distance or radius from a bulletin for a "LINE" type
     * 
     * @param bullMessage
     *            The bulletin message
     * @return an integer of distance
     */
    public static Integer getDistance(String bullMessage) {

        Integer retDistance = IDecoderConstantsN.INTEGER_MISSING;

        // Regular expression for distance
        final String DISTANCE_EXP = "(WI )?([0-9]{2}|[0-9]{3})( )?NM ";
        // Pattern used for extracting distance
        final Pattern distancePattern = Pattern.compile(DISTANCE_EXP);
        Matcher theMatcher = distancePattern.matcher(bullMessage);

        // Regular expression distance for Canada
        final String CANADA_EXP = "WTN ([0-9]{2}|[0-9]{3}) NM OF LN ";
        // Pattern used for extracting distance for CANADA sigmet
        final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
        Matcher canadaMatcher = canadaPattern.matcher(bullMessage);

        if (theMatcher.find()) {
            retDistance = Integer.parseInt(theMatcher.group(2));
        } else if (canadaMatcher.find()) {
            retDistance = Integer.parseInt(canadaMatcher.group(1));
        }
        return retDistance;
    }

    /**
     * Get name location for the VA, TC, or HU....
     * 
     * @param bullMessage
     *            The bulletin message
     * @return a string for nameLocation
     */
    public static String getNameLocation(String bullMessage) {

        String retLocation = " ";

        final String VA_EXP = "MT (\\S)*-PEAK LOC ([NS][0-9]{4} [EW][0-9]{5}) ";
        // Pattern used for extracting VA nameLocation
        final Pattern vaPattern = Pattern.compile(VA_EXP);
        Matcher vaMatcher = vaPattern.matcher(bullMessage);

        final String TC_EXP = " TC ((\\S)* FCST (\\S)* (\\S)*) ";
        // Pattern used for extracting TC nameLocation
        final Pattern tcPattern = Pattern.compile(TC_EXP);
        Matcher tcMatcher = tcPattern.matcher(bullMessage);

        // final String HUR1_EXP = "(HURRICANE IRENE LOCATED AT )";
        final String HUR1_EXP = "(HURRICANE ([A-Z])* LOCATED AT ([\\w|.])* ([\\w|.])*) MOVG";
        // Pattern used for extracting Hurricane nameLocation
        final Pattern hur1Pattern = Pattern.compile(HUR1_EXP);
        Matcher hur1Matcher = hur1Pattern.matcher(bullMessage);

        final String HUR2_EXP = "(HURCN ([A-Z])* NEAR ([\\w|.])* ([\\w|.])*) ";
        // Pattern used for extracting Hurricane nameLocation
        final Pattern hur2Pattern = Pattern.compile(HUR2_EXP);
        Matcher hur2Matcher = hur2Pattern.matcher(bullMessage);

        final String TS_EXP = "(TROPICAL STORM ([A-Z])* (\\w)*)";
        // Pattern used for extracting Tropical Storm nameLocation
        final Pattern tsPattern = Pattern.compile(TS_EXP);
        Matcher tsMatcher = tsPattern.matcher(bullMessage);

        final String TD_EXP = "(TROPICAL (DEPRESSION )?([A-Z])* (\\w)*)";
        // Pattern used for extracting Tropical Depression nameLocation
        final Pattern tdPattern = Pattern.compile(TD_EXP);
        Matcher tdMatcher = tdPattern.matcher(bullMessage);

        if (vaMatcher.find()) {
            retLocation = vaMatcher.group();
        } else if (tcMatcher.find()) {
            retLocation = tcMatcher.group(1);
        } else if (hur1Matcher.find()) {
            retLocation = hur1Matcher.group(1);
        } else if (hur2Matcher.find()) {
            retLocation = hur2Matcher.group(1);
        } else if (tsMatcher.find()) {
            retLocation = tsMatcher.group(1);
        } else if (tdMatcher.find()) {
            retLocation = tdMatcher.group(1);
        }

        return retLocation;
    }

	/**
	 * Obtains polygon extent from a bulletin for a "LINE" type
	 * or a polygon type
	 * 
	 *  @param bullMessage The bulletin message
	 *  @return a string  
	 */
	public static String getPolygonExtent(String bullMessage) {
		
		String retStr = "";
		
		// Regular expression for distance
	    final String DISTANCE_EXP = "(WI )?([0-9]{2}|[0-9]{3})( )?NM ";
	    // Pattern used for extracting distance
		final Pattern distancePattern = Pattern.compile(DISTANCE_EXP);
		Matcher theMatcher = distancePattern.matcher( bullMessage );
		
		// Regular expression for line
	    final String LINE_EXP = " ([0-9]{2}|[0-9]{3})( )?NM (EITHER SIDE OF)";
	    // Pattern used for extracting line information
		final Pattern linePattern = Pattern.compile(LINE_EXP);
		Matcher lineMatcher = linePattern.matcher( bullMessage );
		
		// Regular expression distance for Canada
	    final String CANADA_EXP = "(WTN|WITHIN) ([0-9]{2}|[0-9]{3})( )?NM OF (LN|LINE)";
	    // Pattern used for extracting distance for CANADA sigmet
		final Pattern canadaPattern = Pattern.compile(CANADA_EXP);
		Matcher canadaMatcher = canadaPattern.matcher( bullMessage );

		// Regular expression for POLYGON
	    final String POLYGON_EXP = " ([0-9]{2}|[0-9]{3})( )?NM ([EWSN]|[EWSN]{2}|[EWSN]{3}) OF";
	    // Pattern used for extracting POLYGON information
		final Pattern polygonPattern = Pattern.compile(POLYGON_EXP);
		Matcher polygonMatcher = polygonPattern.matcher( bullMessage );
		
		if (canadaMatcher.find()) {
			retStr = canadaMatcher.group(1);
		} else if (lineMatcher.find()) {
			retStr = lineMatcher.group(3);
		} else if (polygonMatcher.find()) {
			retStr = polygonMatcher.group(3).concat(" OF");
		} else if (theMatcher.find()) {
			retStr = theMatcher.group(1);
		}

		return retStr;
	}
	
    /**
     * Parse the location lines and add location table to the main record table
     * if any
     * 
     * @param theBulletin
     *            The report from bulletin message
     * @param record
     *            The main table
     * @return a boolean true if any location exists.
     */
    public static boolean processLocation(String theBulletin,
            IntlSigmetRecord record) {

        boolean ret = false;

        String locationDelimiter = "-";
        String retLocation = "?";
        int locPositionIndex = -1;

        ArrayList<String> terminationList = new ArrayList<String>();
        // Locations ends with these key words
        terminationList.addAll(Arrays
                .asList(new String[] { "TOP", "SEV", "BKN", "STNR", "ISOL",
                        "MOV", "SOLID", "TOPS", "AREA", "BTN", "STG", "SEVERE",
                        "INTSF=", "MAINLY", "SIGMET", "OCNL", "WKN", "PL",
                        "FZRA", "LLWS", "TURB", "STR", "FCST", "VA", "MDT",
                        "STRONG", "LN", "AT", "BASED", "OF", "SVR", "OVER",
                        "TS", "AND", "OBL", "TURBUL", "BLW", "TOP", "TORNADO",
                        "CORRECTION" }));

        final String LATLON1_EXP = "(N|S)([0-9]{4}) (E|W)([0-9]{5}) -";
        // Pattern used for extracting latlon
        final Pattern latlon1Pattern = Pattern.compile(LATLON1_EXP);
        Matcher latlon1Matcher = latlon1Pattern.matcher(theBulletin);

        final String LATLON2_EXP = "/([0-9]{4})(N|S)([0-9]{5})(E|W)/";
        // Pattern used for extracting latlon
        final Pattern latlon2Pattern = Pattern.compile(LATLON2_EXP);
        Matcher latlon2Matcher = latlon2Pattern.matcher(theBulletin);

        final String LATLON3_EXP = "(N|S)([0-9]{4})(E|W)([0-9]{5})-";
        // Pattern used for extracting latlon
        final Pattern latlon3Pattern = Pattern.compile(LATLON3_EXP);
        Matcher latlon3Matcher = latlon3Pattern.matcher(theBulletin);

        final String LATLON4_EXP = "(N|S)([0-9]{2})(E|W)([0-9]{3})";
        // Pattern used for extracting latlon
        final Pattern latlon4Pattern = Pattern.compile(LATLON4_EXP);
        Matcher latlon4Matcher = latlon4Pattern.matcher(theBulletin);

        final String LATLON5_EXP = "([0-9]{2})(N|S)([0-9]{3}|[0-9]{2})(E|W)";
        // Pattern used for extracting latlon
        final Pattern latlon5Pattern = Pattern.compile(LATLON5_EXP);
        Matcher latlon5Matcher = latlon5Pattern.matcher(theBulletin);

        final String LATLON6_EXP = "([0-9]{4})(N|S)([0-9]{5})(E|W)";
        // Pattern used for extracting latlon
        final Pattern latlon6Pattern = Pattern.compile(LATLON6_EXP);
        Matcher latlon6Matcher = latlon6Pattern.matcher(theBulletin);

        final String LATLON7_EXP = " [A-Z]*( )?-( )?[A-Z]*( )?-";
        // Pattern used for extracting latlon
        final Pattern latlon7Pattern = Pattern.compile(LATLON7_EXP);
        Matcher latlon7Matcher = latlon7Pattern.matcher(theBulletin);

        final String LATLON9_EXP = " [A-Z]* [A-Z]*/(\\w| )*/";
        // Pattern used for extracting latlon
        final Pattern latlon9Pattern = Pattern.compile(LATLON9_EXP);
        Matcher latlon9Matcher = latlon9Pattern.matcher(theBulletin);

        // Find the position of first location
        if (latlon7Matcher.find()) {
            retLocation = latlon7Matcher.group();
        } else if (latlon1Matcher.find()) {
            retLocation = latlon1Matcher.group();
        } else if (latlon2Matcher.find()) {
            retLocation = latlon2Matcher.group();
        } else if (latlon3Matcher.find()) {
            retLocation = latlon3Matcher.group();
        } else if (latlon4Matcher.find()) {
            retLocation = latlon4Matcher.group();
        } else if (latlon5Matcher.find()) {
            retLocation = latlon5Matcher.group();
        } else if (latlon6Matcher.find()) {
            retLocation = latlon6Matcher.group();
        } else if (latlon9Matcher.find()) {
            retLocation = latlon9Matcher.group();
            int sigmetIndex = retLocation.indexOf("SIGMET");
            int fcstIndex = retLocation.indexOf("FCST ");
            int innIndex = retLocation.indexOf("IN N/");
            int sfcIndex = retLocation.indexOf(" SFC");
            int obsIndex = retLocation.indexOf(" OBS");
            // Handle special cases and throw them away
            if (sigmetIndex != -1) {
                retLocation = "?";
            } else if (fcstIndex != -1) {
                retLocation = "?";
            } else if (innIndex != -1) {
                retLocation = "?";
            } else if (sfcIndex != -1) {
                retLocation = "?";
            } else if (obsIndex != -1) {
                retLocation = "?";
            }

        }

        locPositionIndex = theBulletin.indexOf(retLocation);

        if (locPositionIndex > 0) {
            String secondHalf = theBulletin.substring(locPositionIndex);
            Scanner scLocationLine = new Scanner(secondHalf)
                    .useDelimiter("\\x0d\\x0d\\x0a");
            String lines = " ";
            String curLine = null;
            ArrayList<String> locationList = new ArrayList<String>();
            locationList.clear();
            Boolean notBreak = true;

            while (scLocationLine.hasNext() && notBreak) {
                // Get next location line
                curLine = scLocationLine.next();

                Scanner scLocationToken = new Scanner(curLine);
                while (scLocationToken.hasNext() && notBreak) {
                    // Check the token from each line
                    String token = scLocationToken.next();
                    if (terminationList.contains(token)) {
                        // terminate and get the locations in this line
                        int token_pos = curLine.indexOf(token);
                        lines = lines.concat(" ").concat(
                                curLine.substring(0, token_pos));
                        notBreak = false;
                        break;
                    }
                }
                if (notBreak) {
                    lines = lines.concat(" ").concat(curLine);
                }
            }

            // Clean up the leading space
            lines = UtilN.removeLeadingWhiteSpaces(lines);
            lines = removeChar(lines, '\r');
            lines = removeChar(lines, '\n');

            // Decide the location delimiter.
            Boolean whiteDel = false;
            int dashPos = lines.indexOf("-");
            int slashPos = lines.indexOf("/");
            if (dashPos != -1) {
                locationDelimiter = "-";
            } else if (slashPos != -1) {
                locationDelimiter = "/";
            } else {
                locationDelimiter = " ";
                whiteDel = true;
            }
            // Parse the location lines by a "-", "/", or " ".
            Scanner scLocation = new Scanner(lines)
                    .useDelimiter(locationDelimiter);

            locationList.clear();
            // Get all locations
            while (scLocation.hasNext()) {
                // Clean up the leading space
                String location = UtilN.removeLeadingWhiteSpaces(scLocation
                        .next());
                if (whiteDel) {
                    // white space as delimiter
                    int lengthOfLocation = location.length();
                    if (lengthOfLocation > 2) {
                        if (Character.isDigit(location.toCharArray()[0])
                                && (lengthOfLocation > 2)) {
                            locationList.add(location);
                        } else if (Character.isDigit(location.toCharArray()[1])
                                && (lengthOfLocation > 2)) {
                            locationList.add(location);
                        }
                    }
                } else if ((location.length() > 0)
                        && (!location.substring(0, 2).equals("FL"))) {
                    // exception handle for a "FL" false location
                    locationList.add(location);
                }
            }

            // set locations to data base
            Integer idxLocation = 0;
            if (locationList.size() > 1) {
                for (String Location : locationList) {
                    IntlSigmetLocation currentLocation = new IntlSigmetLocation();
                    currentLocation.setLocationLine(lines);
                    // currentLocation.setLocationName(Location);
                    processLatLon(Location, currentLocation, idxLocation,
                            record);
                    currentLocation.setIndex(idxLocation + 1);
                    idxLocation++;

                    record.addIntlSigmetLocation(currentLocation);
                }
                ret = true;
            }
        }
        return ret;
    }

    /**
     * Get detail information for Thunderstorm.
     * 
     * @param theReport
     *            the input bulletin
     * @return a string for Thunderstorm
     */
    public static String getThunderStorm(String theReport) {

        String retTS = "THUNDERSTORM";
        // Regular expression for tsType
        final String TSTYPE_EXP = "(SQL EMBD TS |ISOL EMBD TS |EMBD TS GR |ISOL TS |EMBD TS |"
                + "FRQ TS|SQL TS |TS GR |OBSC TS|FRQ TSGR |OCNL TS |EMBD TSGR | TSGR | GR |"
                + "TS/CB|CB/TS)";
        // Pattern used for extracting hazardType
        final Pattern tsTypePattern = Pattern.compile(TSTYPE_EXP);
        Matcher tsMatcher = tsTypePattern.matcher(theReport);

        if (tsMatcher.find()) {
            if (tsMatcher.group(1).equals("EMBD TS GR ")) {
                retTS = "EMBEDED THUNDERSTORMS HAIL";
            } else if (tsMatcher.group(1).equals("SQL EMBD TS ")) {
                retTS = "SQUALL EMBEDED THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("ISOL EMBD TS ")) {
                retTS = "ISOLATED EMBEDED THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("ISOL TS ")) {
                retTS = "ISOLATED THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("EMBD TS ")) {
                retTS = "EMBEDED THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("FRQ TS")) {
                retTS = "FREQUENT THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("SQL TS ")) {
                retTS = "SQUALL THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("TS GR ")) {
                retTS = "THUNDERSTORMS HAIL";
            } else if (tsMatcher.group(1).equals("OBSC TS")) {
                retTS = "OBSCURE THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("FRQ TSGR ")) {
                retTS = "FREQUENT THUNDERSTORMS HAIL";
            } else if (tsMatcher.group(1).equals("OCNL TS ")) {
                retTS = "OCCASIONAL THUNDERSTORMS";
            } else if (tsMatcher.group(1).equals("EMBD TSGR ")) {
                retTS = "EMBEDED THUNDERSTORMS HAIL";
            } else if (tsMatcher.group(1).equals(" TSGR ")) {
                retTS = "THUNDERSTORMS HAIL";
            } else if (tsMatcher.group(1).equals(" GR ")) {
                retTS = "HAIL";
            } else if (tsMatcher.group(1).equals("TS/CB")) {
                retTS = "THUNDERSTORMS CUMULONIMBUS";
            } else if (tsMatcher.group(1).equals("CB/TS")) {
                retTS = "THUNDERSTORMS CUMULONIMBUS";
            }
        }

        return retTS;
    }

    /**
     * Remove a character from a string and return the result.
     * 
     * @param s
     *            the string
     * @param c
     *            the character to be removed
     * @return a string.
     */
    public static String removeChar(String s, char c) {

        String ret = "";

        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != c)
                ret += s.charAt(i);
        }

        return ret;
    }

}
