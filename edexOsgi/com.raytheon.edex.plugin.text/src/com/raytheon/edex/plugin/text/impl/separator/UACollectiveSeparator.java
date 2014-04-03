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
package com.raytheon.edex.plugin.text.impl.separator;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.text.impl.TextSeparatorFactory;
import com.raytheon.edex.textdb.dbapi.impl.TextDBStaticData;
import com.raytheon.edex.textdb.dbapi.impl.WMOReportData;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Upper Air Collective text Separator.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2008             jkorman     Initial creation
 * Jul 10, 2009 2191       rjpeter     Reimplemented.
 * Mar 13, 2014 2652       skorolev    Fixed calculation of message end.
 * Apr 01, 2014 2915       dgilling    Support re-factored TextDBStaticData.
 * Apr 02, 2014 2652       skorolev    Corrected a removing of excess control characters.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class UACollectiveSeparator extends WMOMessageSeparator {
    private static final String UA_DATA = "..(AA|BB|CC|DD) ";

    private static final String UA_SHORT = UA_DATA + "{1,2}\\d{5} \\d{5}";

    Pattern ua_short = Pattern.compile(UA_SHORT);

    private static final String UA_LONG = "\\d{5} " + UA_DATA
            + "{1,2}\\d{5} \\d{5}";

    Pattern ua_long = Pattern.compile(UA_LONG);

    private static final String UA_NIL_A = "..(AA|BB|CC|DD) {1,2}\\d{5} NIL";

    private static final String UA_NIL_B = "..(AA|BB|CC|DD) {1,2} \\d{4}[0-9/] \\d{5} NIL";

    private static final String UA_NIL_C = "\\d{5} ..(AA|BB|CC|DD) {1,2} \\d{4}[0-9/] \\d{5} NIL";

    private final Log logger = LogFactory.getLog(getClass());

    ArrayList<Pattern> nilPatterns = new ArrayList<Pattern>();
    {
        nilPatterns.add(Pattern.compile(UA_NIL_A));
        nilPatterns.add(Pattern.compile(UA_NIL_B));
        nilPatterns.add(Pattern.compile(UA_NIL_C));
    }

    /**
     * 
     */
    public UACollectiveSeparator(String traceId, String siteId,
            WMOHeader wmoHeader) {
        super(traceId, siteId, wmoHeader);
    }

    @Override
    protected void createProductId() {
        WMOHeader wmoHeader = getWmoHeader();
        String hdr = wmoHeader.getWmoHeader();
        String afosId = TextDBStaticData
                .matchUACollective(createDataDes(wmoHeader));

        if (afosId != null) {
            productId = new AFOSProductId(afosId);
        } else {
            String tt = hdr.substring(0, 2);
            if ("UM".equals(tt) || "UI".equals(tt)) {
                productId = new AFOSProductId("CCC", "SGL", "XXX");
            } else if ("US".equals(tt)) {
                productId = new AFOSProductId("CCC", "MAN", "XXX");
            } else {
                productId = new AFOSProductId("NOA", "FOS", "PIL");
            }
        }
    }

    @Override
    protected void identifyReports(byte[] rawData, Headers headers) {
        AFOSProductId afos_id = getProductId();
        if (afos_id == null) {
            afos_id = NOAFOSPIL;
        }

        WMOHeader wmoHdr = getWmoHeader();
        int startIndex = wmoHdr.getMessageDataStart();
        int endIndex = TextSeparatorFactory.findDataEnd(rawData);
        if (endIndex <= startIndex) {
            endIndex = rawData.length - 1;
        }
        String rawMsg = new String(rawData, startIndex, endIndex - startIndex);
        Matcher nnnxxxMatcher = NNNXXX.matcher(rawMsg);
        if (nnnxxxMatcher.find() && nnnxxxMatcher.start() == 0) {
            rawMsg = rawMsg.substring(nnnxxxMatcher.end());
        }
        StringBuilder buffer = new StringBuilder(rawMsg);
        String hdrStr = wmoHdr.getWmoHeader();
        String dataDes = createDataDes(wmoHdr);
        String origin = wmoHdr.getCccc();
        boolean flag = true;
        logger.trace("Decoding Upper Air Message.");
        StringBuilder stationNum = new StringBuilder();
        StringBuilder parsedMsg = new StringBuilder();
        String XXX_id = null;
        String station_id = null;

        // Loop through the Msg until and EOM is reached.
        while (flag && buffer.length() > 0) {
            // Check to make sure that there is a message.
            if ((buffer.length() > MIN_COLL_DATA_LEN)
                    && (buffer.charAt(0) != EOM)) {
                parsedMsg.setLength(0);

                // Get the station number and the rest of the message until a
                // record separator is reached.
                parseUpairMsg(buffer, stationNum, parsedMsg, dataDes);
                AFOSProductId newProductId = new AFOSProductId();

                // couldn't determine stationNum
                if (stationNum.length() == 0) {
                    // deal with special irregular MAN products
                    if (dataDes == "USMF01" || dataDes == "USDR01"
                            || dataDes == "USTD01") {
                        if (buffer.length() < 20) {
                            flag = false;
                        }
                    } else {
                        if (buffer.length() < MIN_COLL_DATA_LEN) {
                            flag = false;
                        }
                    }
                    continue;
                }
                // Get the XXX from the station number, using station_table.dat
                else {
                    XXX_id = TextDBStaticData.mapWMOToICAO(stationNum
                            .toString());
                    if (XXX_id == null) {
                        logger.debug("NCF_FAIL to map station number to XXX: "
                                + stationNum);
                        continue;
                    }

                    // If the product is supposed to have an AFOS ID, create it.
                    // Otherwise, store products to the database using TTAAii
                    // CCCC,
                    // and give afos_id a dummy value during the AFOS to
                    // AWIPS transition so that products can still be stored.
                    if (!makeCollId(newProductId, XXX_id, afos_id, origin)) {
                        if (NOAFOSPIL.equals(afos_id)) {
                            logger.debug("No ID found; use NOAFOSPIL to store");
                            newProductId = NOAFOSPIL;
                        } else {
                            numSkipped++;

                            /*
                             * if (moveBadTxt) { logger.error(
                             * "Invalid product id. Move undecodable data into badText"
                             * ); // moveToBad ((byte)parsedMsg.stringPtr(), //
                             * parsedMsg.length(), //
                             * "Error from decodeUpairMsg"); } else logger
                             * .error("Invalid product id, product not stored");
                             */

                            // trash data if the remaining is less than 8 bytes
                            // First, deal with special irregular MAN products
                            if (dataDes.equals("USMF01")
                                    || dataDes.equals("USDR01")
                                    || dataDes.equals("USTD01")) {
                                if (buffer.length() < 20) {
                                    flag = false;
                                }
                            } else {
                                if (buffer.length() < MIN_COLL_DATA_LEN) {
                                    flag = false;
                                }
                            }

                            // filter out junk characters
                            while (buffer.length() > 0
                                    && !checkCharNum(buffer.charAt(0))) {
                                buffer.deleteCharAt(0);
                            }

                            // again, trash data if it is less than 20 bytes
                            if (buffer.length() < MIN_COLL_DATA_LEN) {
                                flag = false;
                            }

                            continue;
                        }
                    }
                }

                // logEvent << "WMO header = " << WMOheader << std::endl;

                // If the origin is determinable, take the origin of an
                // individual
                // product (and not the one in the header) to be the CCCC that
                // will be stored.

                if (XXX_id.length() == 4) {
                    station_id = XXX_id;
                } else if (XXX_id.length() == 3) {
                    station_id = origin.charAt(0) + XXX_id;
                }

                // logEvent << "CCCC = " << station_id << std::endl;
                // logEvent << "parsedMsg = " << parsedMsg << std::endl;
                // logEvent << "WMOheader.mid(4,3) = " << WMOheader.mid(4,3) <<
                // std::endl;

                StringBuilder newHdr = new StringBuilder();
                // hdrStr
                if (hdrStr.charAt(4) == ' ') {
                    newHdr.append(hdrStr.substring(0, 5));
                    newHdr.append(station_id);
                    newHdr.append(hdrStr.substring(hdrStr.length() - 9,
                            hdrStr.length()));
                } else if (hdrStr.charAt(5) == ' ') {
                    newHdr.append(hdrStr.substring(0, 6));
                    newHdr.append(station_id);
                    newHdr.append(hdrStr.substring(hdrStr.length() - 10,
                            hdrStr.length()));
                } else if (hdrStr.charAt(6) == ' ') {
                    newHdr.append(hdrStr.substring(0, 7));
                    newHdr.append(station_id);
                    newHdr.append(hdrStr.substring(hdrStr.length() - 11,
                            hdrStr.length()));
                } else {
                    newHdr.append(hdrStr);
                }
                newHdr.append("\r\n");

                // logEvent << "WMO new header = " << WMOnewHeader << std::endl;
                WMOHeader newWmoHdr = new WMOHeader(newHdr.toString()
                        .getBytes(), headers);

                // skip possible partial AWIPS id
                if (parsedMsg.length() > MAX_SECND_LINE_LEN) {
                    WMOReportData rptData = new WMOReportData(newWmoHdr,
                            newProductId, parsedMsg.toString());
                    reports.add(rptData);
                }

                // deal with special irregular MAN products
                if (dataDes == "USMF01" || dataDes == "USDR01"
                        || dataDes == "USTD01") {
                    if (buffer.length() < 20) {
                        flag = false;
                    }
                } else {
                    if (buffer.length() < MIN_COLL_DATA_LEN) {
                        flag = false;
                    }
                }
            } else {
                flag = false;
            }
        }
    }

    /**
     * Iterate through the reports collection and determine the actual productId
     * for each report. We will also get rid of any that might not be real data
     * i.e. an empty string, or nil reports.
     */
    void identifyReports(List<WMOReportData> reports, AFOSProductId productId) {

        for (int i = 0; i < reports.size();) {

            WMOReportData data = reports.get(i);

            // get the 'base' product identifier for the message.
            data.setAfosProdId(productId);

            String rpt = data.getReportData();

            Matcher m = null;
            for (Pattern p : nilPatterns) {
                m = p.matcher(rpt);
                if (m.find()) {
                    reports.remove(i);
                    continue;
                }
            }

            String wmoId = null;

            String tt = null;

            m = ua_short.matcher(rpt);
            if (m.find()) {
                wmoId = rpt.substring(m.end() - 5, m.end());
                tt = rpt.substring(m.start() + 2, m.start() + 4);
            } else {
                m = ua_long.matcher(rpt);
                if (m.find()) {
                    wmoId = rpt.substring(m.end() - 5, m.end());
                    tt = rpt.substring(m.start() + 6, m.start() + 8);
                } else {
                    // complain for now, remove the report and continue
                    reports.remove(i);
                    continue;
                }
            }
            String uaNNN = null;
            if ("AA".equals(tt) || "CC".equals(tt)) {
                uaNNN = "MAN";
            } else if ("BB".equals(tt) || "DD".equals(tt)) {
                uaNNN = "SGL";
            }

            String icao = TextDBStaticData.mapWMOToICAO(wmoId);
            if (icao != null) {

                // get the associated CCC
                String cccId = TextDBStaticData.mapICAOToCCC(icao);

                AFOSProductId prodId = data.getAfosProdId();
                if (prodId != null) {
                    if (uaNNN != null) {
                        prodId.setNnn(uaNNN);
                    }

                    String xxxId = icao;
                    prodId.setCcc(cccId);
                    if (xxxId.length() == 4) {
                        xxxId = xxxId.substring(1, 4);
                    }
                    if (xxxId.length() == 3) {
                        prodId.setXxx(xxxId);
                        i++;
                    } else {
                        // this is an error in the table data.
                        // logger.error("Invalid icao[" + icao +
                        // "] mapped from "
                        // + wmoId + " in stationTable.dat");
                        reports.remove(i);
                    }
                } else {
                    // This is an error condition that shouldn't happen!
                    // logger.error("The productId for this report is missing");
                    reports.remove(i);
                }
            } else {
                // no mapping for the wmoId
                // logger.error("The wmo identifier [" + wmoId +
                // "] is unknown");
                reports.remove(i);
            }
        }
    }

    // -- fileScope
    // --------------------------------------------------------------
    // parseUpairMsg()
    //
    // Parses an upper air collective text message by retrieving the station
    // number and matching that number with an XXX. Function also reads the
    // message to the next record separator.
    //
    // -- implementation
    // ---------------------------------------------------------
    // Called by the decodeUpAirMsg function.
    // ---------------------------------------------------------------------------
    private void parseUpairMsg(StringBuilder buffer, StringBuilder stationNum,
            StringBuilder parsedMsg, String dataDes) {
        stationNum.setLength(0);

        // Check each message for the \036 record separator and increment past
        // it.
        if (!checkCharNum(buffer.charAt(0))) {
            buffer.deleteCharAt(0);
        }

        // Check for UEXX or UJXX formatted messages and decode
        if (dataDes.endsWith("XX") || dataDes.endsWith("81")
                || dataDes.endsWith("82")) {
            stationNum.append(assignTextSegment(buffer.toString(), CSPC));
            getTextSegment(buffer, parsedMsg, MARKERANDEOM);
        }
        // Check for USUS80 or 90 formatted messages and decode
        else if (dataDes.endsWith("80") || dataDes.endsWith("90")) {
            if (checkCharNum(buffer.charAt(0))) {
                buffer.deleteCharAt(0);
            } else {
                stationNum.append(assignTextSegment(buffer.toString(), CSPC));
            }

            getTextSegment(buffer, parsedMsg, CSEP);
        } else {
            // Otherwise it's standard format so decode
            if (!checkCharNum(buffer.charAt(0))) {
                buffer.deleteCharAt(0);
            } else {

                // Move to the third field of the message to get the station
                // number.
                // The format of some products had been causing the second field
                // to be used as the station number instead of the third, so the
                // length of a string between spaces is now used to determine a
                // valid field.
                int x = 0;
                while (x < 2 && buffer.length() > 0) {
                    int len = buffer.length();
                    if (safeStrpbrk(buffer, CSPC)) {
                        if (len - buffer.length() >= 4) {
                            x++;
                        }
                        buffer.deleteCharAt(0);
                    }
                }

                stationNum.append(assignTextSegment(buffer.toString(), CSPC));
            }

            getTextSegment(buffer, parsedMsg, CSEP);
        }

        // Remove excess control characters
        if (buffer.length() == 0) {
            if (parsedMsg.length() < MIN_COLL_DATA_LEN) {
                stationNum.setLength(0);
            } else {
                trim_message(parsedMsg);
            }
        } else if (buffer.charAt(0) == '=') {
            buffer.deleteCharAt(0);
            while (buffer.length() > 0) {
                char c = buffer.charAt(0);
                if ((c == '\n') || (c == '\r')) {
                    buffer.deleteCharAt(0);
                } else {
                    break;
                }
            }
        } else if ((buffer.charAt(0) == EOM)
                && (parsedMsg.length() > (MIN_COLL_DATA_LEN - 1))) {
            trim_message(parsedMsg);
        } else if (parsedMsg.length() < MIN_COLL_DATA_LEN) {
            stationNum.setLength(0);
        }
    }

    // -- fileScope
    // --------------------------------------------------------------
    // makeCollId()
    //
    // Gets the CCC from XXX map from national table and combines this with the
    // NNN that was gotten from the collective table to create the 9 character
    // AFOS id.
    //
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    boolean makeCollId(AFOSProductId product_id, String XXX_id,
            AFOSProductId afos_id, String origin) {
        String CCC_id, newId;

        logger.trace("Getting CCC from XXX.");

        // If this is a national bit product, just pass "AAA" through as CCC.
        if (afos_id.getCcc().equals("AAA")) {
            CCC_id = "AAA";
        }
        // Otherwise, use the national category table to get the CCC from the
        // XXX
        else {
            CCC_id = TextDBStaticData.mapICAOToCCC(XXX_id);
            if (CCC_id == null) {
                // We failed to get a CCC from the national_category_table...

                // If the XXX is 3 characters, and the origin starts with K, try
                // prepending K or P (the latter for AK, HI products)
                if ((XXX_id.length() == 3) && (origin.charAt(0) == 'K')) {
                    newId = "K" + XXX_id;
                    CCC_id = TextDBStaticData.mapICAOToCCC(newId);
                    if (CCC_id == null) {
                        newId = "P" + XXX_id;
                        CCC_id = TextDBStaticData.mapICAOToCCC(newId);
                        if (CCC_id == null) {
                            // logger.error("NCF_FAIL to map XXX to CCC: "
                            // + XXX_id);
                            return false;
                        }
                    }
                }
                // Otherwise, if the XXX is 3 characters, try prepending the
                // first
                // character of the origin.
                else if (XXX_id.length() == 3) {
                    newId = origin.charAt(0) + XXX_id;
                    CCC_id = TextDBStaticData.mapICAOToCCC(newId);
                    if (CCC_id == null) {
                        // logger.error("NCF_FAIL to map XXX to CCC: " +
                        // XXX_id);
                        return false;
                    }
                } else {
                    // logger.error("NCF_FAIL to map XXX to CCC: " + XXX_id);
                    if (!checkCharNum(XXX_id.charAt(0))) {
                        // logger.error("bad XXX id");
                        return false;
                    } else {
                        return false;
                    }
                }
            }
        }

        // Get the NNN from the afos_id "CCCNNNXXX" that was assigned from the
        // collectives table.
        product_id.setCcc(CCC_id);
        product_id.setNnn(afos_id.getNnn());
        if (XXX_id.length() == 3) {
            product_id.setXxx(XXX_id);
        } else {
            product_id.setXxx(XXX_id.substring(1));
        }

        return true;
    }
}
