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
package com.raytheon.uf.edex.plugin.text.impl.separator;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.regex.Matcher;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.text.impl.TextDBStaticData;
import com.raytheon.uf.edex.plugin.text.impl.TextSeparatorFactory;
import com.raytheon.uf.edex.plugin.text.impl.WMOReportData;

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
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * Dec 03, 2014 ASM #16859 D. Friedman Use CharBuffer instead of StringBuilder.
 * Feb 17, 2014 ASM #17125 D. Friedman Fix parsing of type and date fields.
 * Dec 09, 2015 5166       kbisanz     Update logging to use SLF4J.
 * May 09, 2018 7308       njensen     Fixed newHdr in identifyReports()
 *                                     Removed dead code
 * 
 * </pre>
 * 
 * @author jkorman
 */

public class UACollectiveSeparator extends WMOMessageSeparator {

    private final Logger logger = LoggerFactory.getLogger(getClass());

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
        CharBuffer rawMsg = Charset.forName("ISO-8859-1").decode(
                ByteBuffer.wrap(rawData, startIndex, endIndex - startIndex));
        Matcher nnnxxxMatcher = NNNXXX.matcher(rawMsg);
        if (nnnxxxMatcher.find() && nnnxxxMatcher.start() == 0) {
            rawMsg.position(rawMsg.position() + nnnxxxMatcher.end());
        }
        CharBuffer buffer = rawMsg;
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
                    if (isIrregularMANProduct(dataDes)) {
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
                    XXX_id = TextDBStaticData
                            .mapWMOToICAO(stationNum.toString());
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
                            if (isIrregularMANProduct(dataDes)) {
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
                                buffer.get();
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
                    newHdr.append(hdrStr.substring(9, hdrStr.length()));
                } else if (hdrStr.charAt(5) == ' ') {
                    newHdr.append(hdrStr.substring(0, 6));
                    newHdr.append(station_id);
                    newHdr.append(hdrStr.substring(10, hdrStr.length()));
                } else if (hdrStr.charAt(6) == ' ') {
                    newHdr.append(hdrStr.substring(0, 7));
                    newHdr.append(station_id);
                    newHdr.append(hdrStr.substring(11, hdrStr.length()));
                } else {
                    newHdr.append(hdrStr);
                }
                newHdr.append("\r\n");

                // logEvent << "WMO new header = " << WMOnewHeader << std::endl;
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                WMOHeader newWmoHdr = new WMOHeader(
                        newHdr.toString().getBytes(), fileName);

                // skip possible partial AWIPS id
                if (parsedMsg.length() > MAX_SECND_LINE_LEN) {
                    WMOReportData rptData = new WMOReportData(newWmoHdr,
                            newProductId, parsedMsg.toString());
                    reports.add(rptData);
                }

                // deal with special irregular MAN products
                if (isIrregularMANProduct(dataDes)) {
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
    private void parseUpairMsg(CharBuffer buffer, StringBuilder stationNum,
            StringBuilder parsedMsg, String dataDes) {
        stationNum.setLength(0);

        // Check each message for the \036 record separator and increment past
        // it.
        if (!checkCharNum(buffer.charAt(0))) {
            buffer.get();
        }

        // Check for UEXX or UJXX formatted messages and decode
        if (dataDes.endsWith("XX") || dataDes.endsWith("81")
                || dataDes.endsWith("82")) {
            stationNum.append(assignTextSegment(buffer, CSPC));
            getTextSegment(buffer, parsedMsg, MARKERANDEOM);
        }
        // Check for USUS80 or 90 formatted messages and decode
        else if (dataDes.endsWith("80") || dataDes.endsWith("90")) {
            /*
             * // A1 logic. It is not clear what this was for. if
             * (checkCharNum(buffer.charAt(0))) { buffer.get(); } else {
             * stationNum.append(assignTextSegment(buffer, CSPC)); }
             */

            if (!checkCharNum(buffer.charAt(0))) {
                buffer.get();
            }
            stationNum.append(assignTextSegment(buffer, CSPC));
            getTextSegment(buffer, parsedMsg, CSEP);
        } else {
            // Otherwise it's standard format so decode
            if (!checkCharNum(buffer.charAt(0))) {
                buffer.get();
            } else {

                // Move to the third field of the message to get the station
                // number.
                // The format of some products had been causing the second field
                // to be used as the station number instead of the third, so the
                // length of a string between spaces is now used to determine a
                // valid field.
                CharBuffer stationSearchBuffer = buffer.duplicate();
                int x = 0;
                while (x < 2 && stationSearchBuffer.length() > 0) {
                    int len = stationSearchBuffer.length();
                    if (safeStrpbrk(stationSearchBuffer, CSPC)) {
                        if (len - stationSearchBuffer.length() >= 4) {
                            x++;
                        }
                        stationSearchBuffer.get();
                    }
                }

                stationNum.append(assignTextSegment(stationSearchBuffer, CSPC));
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
            buffer.get();
            while (buffer.length() > 0) {
                char c = buffer.charAt(0);
                if ((c == '\n') || (c == '\r')) {
                    buffer.get();
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
        if ("AAA".equals(afos_id.getCcc())) {
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

    private boolean isIrregularMANProduct(String dataDes) {
        return "USFM01".equals(dataDes) || "USDR01".equals(dataDes)
                || "USTD01".equals(dataDes);
    }

}
