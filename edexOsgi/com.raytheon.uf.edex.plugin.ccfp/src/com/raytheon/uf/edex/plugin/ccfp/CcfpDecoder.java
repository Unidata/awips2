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

package com.raytheon.uf.edex.plugin.ccfp;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ccfp.CcfpLocation;
import com.raytheon.uf.common.dataplugin.ccfp.CcfpRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.vividsolutions.jts.io.WKTReader;

/**
 * 
 * Collaborative Convective Forecast Product (CCFP) Decoder
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 03, 2007 908         bwoodle     initial creation
 * Dec 03, 2008             chammack    Camel refactor
 * Sep 15, 2009 3027        njensen     Patterns constants
 * Sep 21, 2009 3072        bsteffen    Fixed Decoding of Line Records
 * Jan 02, 2013 DCS 135     tk          handle coverage value Line records
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Apr 16, 2014 3001        bgonzale    Use UfStatus for logging.
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed TimeTools usage
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1
 */

public class CcfpDecoder extends AbstractDecoder {

    private static final IUFStatusHandler theLogger = UFStatus
            .getHandler(CcfpDecoder.class);

    /** Match the product returned from separator */
    private static final String PARSE_STRING = "[A-Z]{4}[0-9]{1,2} [A-Z]{4} [0-9]{6}(?: [A-Z]{3})?\n"
            + "CFP[\\p{Alnum} ]{3}\n" // awips header
            + "CCFP (\\d{4})(\\d{2})(\\d{2})_(\\d{2})\\d{2} (\\d{4})(\\d{2})(\\d{2})_(\\d{2})\\d{2}\n" // start/valid
            // times
            + "(AREA|LINE).*\n" // SKIP AREA and LINE parsing
            + "(CANADA ON|CANADA OFF)";

    /** Parse an AREA line */
    private static final String PARSE_AREA = "AREA (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) \\d+ (.*) (\\d+) (\\d+)";

    /** Parse a LINE line */
    private static final String PARSE_LINE = "LINE (\\d+) \\d+ (.*)";

    private static final Pattern STRING_PATTERN = Pattern.compile(PARSE_STRING);

    private static final Pattern AREA_PATTERN = Pattern.compile(PARSE_AREA);

    private static final Pattern LINE_PATTERN = Pattern.compile(PARSE_LINE);

    private static final Pattern PAIR_PATTERN = Pattern
            .compile("(\\d+) (\\d+)");

    private static final String COMMA = ",";

    private static final String SPACE = " ";

    private static final PluginDataObject[] EMPTY_PDO = new PluginDataObject[0];

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public CcfpDecoder() throws DecoderException {
    }

    public PluginDataObject[] decode(String msg, Headers headers)
            throws PluginException {

        PluginDataObject[] data = null;

        Calendar baseTime = null;
        WMOHeader wmoHdr = new WMOHeader(msg.getBytes());
        if (wmoHdr.isValid()) {
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            baseTime = WMOTimeParser.findDataTime(wmoHdr.getYYGGgg(), fileName);
        } else {
            baseTime = TimeUtil.newGmtCalendar();
        }

        CcfpRecord record = new CcfpRecord();
        record.setMessageData(msg);
        CcfpLocation location = new CcfpLocation();

        Matcher matcher = STRING_PATTERN.matcher(msg);

        try {
            WKTReader wktReader = new WKTReader();
            if (matcher.find()) {
                Calendar start = TimeUtil.newGmtCalendar(
                        Integer.parseInt(matcher.group(1)),
                        Integer.parseInt(matcher.group(2)),
                        Integer.parseInt(matcher.group(3)));
                start.set(Calendar.HOUR_OF_DAY,
                        Integer.parseInt(matcher.group(4)));
                Calendar valid = TimeUtil.newGmtCalendar(
                        Integer.parseInt(matcher.group(5)),
                        Integer.parseInt(matcher.group(6)),
                        Integer.parseInt(matcher.group(7)));
                valid.set(Calendar.HOUR_OF_DAY,
                        Integer.parseInt(matcher.group(8)));
                TimeRange range = new TimeRange(start.getTime(),
                        valid.getTime());
                record.setDataTime(new DataTime(start.getTime().getTime(),
                        range));
                record.setProducttype(matcher.group(9));
                if (matcher.group(10).equals("CANADA ON")) {
                    record.setCanadaflag(Boolean.TRUE);
                } else {
                    record.setCanadaflag(Boolean.FALSE);
                }
                record.setInsertTime(baseTime);
            }
            if (record.getProducttype().equals("AREA")) {
                matcher = AREA_PATTERN.matcher(msg);
                if (matcher.find()) {
                    record.setCoverage(Integer.parseInt(matcher.group(1)));
                    record.setConf(Integer.parseInt(matcher.group(2)));
                    record.setGrowth(Integer.parseInt(matcher.group(3)));
                    record.setTops(Integer.parseInt(matcher.group(4)));
                    record.setSpeed(Integer.parseInt(matcher.group(5)));
                    record.setDirection(Integer.parseInt(matcher.group(6)));
                    location.setBoxLat(Double.parseDouble(matcher.group(8)) * 0.1);
                    location.setBoxLong(Double.parseDouble(matcher.group(9))
                            * -0.1);
                    String templatlonpairs = matcher.group(7);
                    matcher = PAIR_PATTERN.matcher(templatlonpairs);
                    StringBuffer wtk = new StringBuffer();
                    wtk.append("POLYGON((");
                    if (matcher.find()) {
                        wtk.append(Double.toString(Integer.parseInt(matcher
                                .group(2)) * -0.1));
                        wtk.append(SPACE);
                        wtk.append(Double.toString(Integer.parseInt(matcher
                                .group(1)) * 0.1));
                    }
                    while (matcher.find()) {
                        wtk.append(COMMA);
                        wtk.append(SPACE);
                        wtk.append(Double.toString(Integer.parseInt(matcher
                                .group(2)) * -0.1));
                        wtk.append(SPACE);
                        wtk.append(Double.toString(Integer.parseInt(matcher
                                .group(1)) * 0.1));
                    }
                    wtk.append("))");
                    location.setGeometry(wktReader.read(wtk.toString()));
                    record.setLocation(location);
                }
            } else if (record.getProducttype().equals("LINE")) {
                matcher = LINE_PATTERN.matcher(msg);
                if (matcher.find()) {
                    // change to group 1
                    record.setCoverage(Integer.parseInt(matcher.group(1)));
                    record.setConf(null);
                    record.setGrowth(null);
                    record.setTops(null);
                    record.setSpeed(null);
                    record.setDirection(null);
                    // change to group 2
                    String templatlonpairs = matcher.group(2);
                    matcher = PAIR_PATTERN.matcher(templatlonpairs);

                    StringBuffer wtk = new StringBuffer();
                    wtk.append("LINESTRING(");
                    if (matcher.find()) {

                        Double lon = Integer.parseInt(matcher.group(2)) * -0.1;
                        String lonStr = Double.toString(lon);

                        Double lat = Integer.parseInt(matcher.group(1)) * 0.1;
                        String latStr = Double.toString(lat);

                        location.setBoxLong(lon);
                        location.setBoxLat(lat);

                        wtk.append(lonStr);
                        wtk.append(SPACE);
                        wtk.append(latStr);
                    }
                    while (matcher.find()) {
                        wtk.append(COMMA);
                        wtk.append(SPACE);
                        wtk.append(Double.toString(Integer.parseInt(matcher
                                .group(2)) * -0.1));
                        wtk.append(SPACE);
                        wtk.append(Double.toString(Integer.parseInt(matcher
                                .group(1)) * 0.1));
                    }
                    wtk.append(")");
                    location.setGeometry(wktReader.read(wtk.toString()));
                    record.setLocation(location);
                }
            }
        } catch (Exception e) {
            record = null;
            theLogger.error("Unable to decode CCFP", e);
        }
        data = EMPTY_PDO;
        if (record != null) {
            record.setInsertTime(baseTime);
            data = new PluginDataObject[] { record };
        }
        return data;
    }
}
