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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.text.impl.TextSeparatorFactory;
import com.raytheon.edex.textdb.dbapi.impl.WMOReportData;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Standard Message Collective Separator.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------       ---------- ----------- --------------------------
 * Sep 3, 2008             jkorman     Initial creation
 * Jul 10, 2009 2191       rjpeter     Reimplemented.
 * Sep 22, 2010 6932       cjeanbap    Added METAR/SPECI to product.
 * Feb 18, 2014 2652       skorolev    Fixed error in the makeCollId.
 * Mar 06, 2014 2652       skorolev    Corrected rawMsg extraction.
 * Mar 14, 2014 2652       skorolev    Changed logging for skipped headers.
 *                                     Fixed calculation of message end.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class StdCollectiveSeparator extends WMOMessageSeparator {
    private final Log logger = LogFactory.getLog(getClass());

    private static final Pattern P_TAF = Pattern
            .compile("(TAF +AMD)|(TAF +COR)|(TAF...[\r\n])|(TAF ?)");

    private StringBuilder fouHeader = new StringBuilder();

    private boolean checkFouHeader = true;

    private boolean fouFlag = false;

    private boolean pirFlag = false;

    private String reportType = null;

    private static final String METAR = "METAR";

    private static final String SPECI = "SPECI";

    /**
     * 
     * @param traceId
     * @param siteId
     * @param wmoHeader
     */
    public StdCollectiveSeparator(String traceId, String siteId,
            WMOHeader wmoHeader) {
        super(traceId, siteId, wmoHeader);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.text.impl.separator.WMOMessageSeparator#
     * createProductId()
     */
    @Override
    protected void createProductId() {
        WMOHeader wmoHeader = getWmoHeader();
        String hdr = wmoHeader.getWmoHeader();
        String afosId = staticData.matchStdCollective(createDataDes(wmoHeader));

        if (afosId != null) {
            // String ccc = SiteMap.getInstance().getCCCFromXXXCode(siteId);
            productId = new AFOSProductId(afosId);
        } else {
            String tt = hdr.substring(0, 2);
            if ("SA".equals(tt) || "SP".equals(tt)) {
                productId = new AFOSProductId("CCC", "MTR", "XXX");
            } else if ("FR".equals(tt)) {
                productId = new AFOSProductId("CCC", "TWB", "XXX");
            } else if (("FT".equals(tt)) || ("FC".equals(tt))) {
                productId = new AFOSProductId("CCC", "TAF", "XXX");
            } else {
                productId = NOAFOSPIL;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.text.impl.separator.WMOMessageSeparator#
     * identifyReports(byte[], com.raytheon.edex.esb.Headers)
     */
    @Override
    protected void identifyReports(byte[] rawData, Headers headers) {
        AFOSProductId afos_id = getProductId();
        if (afos_id == null) {
            afos_id = NOAFOSPIL;
        }

        WMOHeader wmoHdr = getWmoHeader();
        WMOHeader newWmoHdr = null;
        String hdrStr = wmoHdr.getWmoHeader();
        String dataDes = createDataDes(wmoHdr);
        pirFlag = ("UBUS31".equals(dataDes) || "UBUS35".equals(dataDes)
                || "SDUS8 ".equals(dataDes) || "SDUS08".equals(dataDes) || "SDUS40"
                .equals(dataDes));
        fouFlag = dataDes.startsWith("FOX");
        String productType = null;
        int startIndex = wmoHdr.getMessageDataStart();
        int endIndex = TextSeparatorFactory.findDataEnd(rawData);
        if (endIndex <= startIndex) {
            endIndex = rawData.length - 1;
        }
        String rawMsg = new String(rawData, startIndex, endIndex - startIndex);
        StringBuilder sb = null;
        // This is a fake line "METXXX" to permit skipping of NNNXXX pattern
        // line.
        if ((rawMsg.indexOf(METAR) == 0) || (rawMsg.indexOf(SPECI) == 0)) {
            productType = (rawMsg.indexOf(METAR) == 0 ? METAR : SPECI);
            sb = new StringBuilder(rawMsg);
            sb.insert(0, "METXXX\n");
            rawMsg = sb.toString();
        }

        if ("TAF".equals(afos_id.getNnn())) {
            sb = new StringBuilder(rawMsg);
            Matcher m = P_TAF.matcher(sb);
            while (m.find()) {
                sb.delete(m.start(), m.end());
                m = P_TAF.matcher(sb);
            }
            sb.insert(0, "\n");
            sb.insert(0, "TAFXXX\nTAF");
            rawMsg = sb.toString();
        }
        Matcher nnnxxxMatcher = NNNXXX.matcher(rawMsg);
        if (nnnxxxMatcher.find()) {
            rawMsg = rawMsg.substring(nnnxxxMatcher.end());
        }
        StringBuilder buffer = new StringBuilder(rawMsg);
        boolean parsing = true;

        while (parsing) {
            if (buffer.length() < MIN_COLL_DATA_LEN || buffer.charAt(0) == EOM) {
                parsing = false;
            } else {
                StringBuilder XXX_id = new StringBuilder();
                StringBuilder parsedMsg = new StringBuilder();
                AFOSProductId product_id = AFOSProductId.copy(afos_id);

                // Get the XXX id and the rest of the message until a record
                // separator is reached
                parseCollMsg(buffer, XXX_id, parsedMsg);

                // check for product type being set and not currently in parsed
                // message.
                if ((productType != null)
                        && (parsedMsg.indexOf(productType) < 0)) {
                    parsedMsg.insert(0, productType + " ");
                }

                // Check to see if the line parsed properly
                if (XXX_id.length() == 0) {
                    if (buffer.length() < MIN_COLL_DATA_LEN
                            || buffer.charAt(0) == '\0') {
                        break;
                    } else {
                        continue;
                    }
                }

                // skip possible partial AWIPS id
                if (parsedMsg.length() > MAX_SECND_LINE_LEN) {
                    // Take the origin of an individual product (and not
                    // the one in the header) to be the CCCC that will be
                    // stored.
                    String parsedMsgStr = parsedMsg.substring(0, 10);
                    String station_id;
                    if (parsedMsgStr.startsWith("METAR")
                            || parsedMsgStr.startsWith("SPECI")
                            || parsedMsgStr.startsWith("TESTM")
                            || parsedMsgStr.startsWith("TESTS")) {
                        station_id = parsedMsgStr.substring(6, 10);
                    } else {
                        station_id = parsedMsgStr.substring(0, 4);
                    }

                    // logEvent << "CCCC = " << station_id << std::endl;
                    // logEvent << "parsedMsg = " << parsedMsg << std::endl;
                    StringBuilder newHdr = new StringBuilder();

                    if (!parsedMsgStr.startsWith("NIL")
                            && (parsedMsgStr.length() >= 3)
                            && (parsedMsgStr.substring(2, 4) != "XX")
                            && (parsedMsgStr.substring(1, 2) != ".")
                            && (!parsedMsgStr.startsWith("MRF"))
                            && (!parsedMsgStr.startsWith("AVN"))
                            && (!parsedMsgStr.startsWith("EXT"))) {
                        int spaceIndex = hdrStr.indexOf(' ');

                        if (station_id.charAt(station_id.length() - 1) == ' ') {
                            station_id = hdrStr.charAt(spaceIndex + 1)
                                    + station_id.substring(0, 3);
                        }

                        newHdr.append(hdrStr.substring(0, spaceIndex + 1));
                        newHdr.append(station_id);
                        newHdr.append(hdrStr.substring(spaceIndex + 5));
                    } else {
                        newHdr.append(hdrStr);
                    }

                    newHdr.append("\r\n");
                    newWmoHdr = new WMOHeader(newHdr.toString().getBytes(),
                            headers);

                    // If so, take the afos id (if it exists) and the XXX id and
                    // create the full AFOS id of the product. If no AFOS ID
                    // exists,
                    // use TTAAii CCCC to store, and give afos_id a dummy value
                    // during the AFOS to AWIPS transition so that products can
                    // still be stored.
                    if (!makeCollId(product_id, XXX_id, afos_id,
                            wmoHdr.getCccc(), newWmoHdr)) {
                        if (NOAFOSPIL.equals(afos_id)) {
                            logger.info("No AFOS ID found; use TTAAii CCCC to store: "
                                    + dataDes + wmoHdr.getCccc());
                            product_id = NOAFOSPIL;
                        } else {
                            numSkipped++;

                            /*
                             * if (moveBadTxt) { logger.error(
                             * "Invalid product id. Move undecodable data into badText"
                             * ); // moveToBad ((byte)parsedMsg.stringPtr(), //
                             * parsedMsg.length(), //
                             * "Error from decodeCollMsg"); } else { logger
                             * .error("Invalid product id, product not stored");
                             * }
                             */

                            // trash data if the remaining is less than 8 bytes
                            if ((buffer.length() < MIN_COLL_DATA_LEN)
                                    || buffer.charAt(0) == '\0') {
                                break;
                            }

                            // filter out junk characters
                            while (buffer.length() > 0
                                    && !checkCharNum(buffer.charAt(0)))
                                buffer.deleteCharAt(0);

                            // again, trash data if it is less than 20 bytes
                            if (buffer.length() < MIN_COLL_DATA_LEN) {
                                break;
                            } else {
                                continue;
                            }
                        }
                    }

                    WMOReportData rptData = new WMOReportData(newWmoHdr,
                            product_id, parsedMsg.toString());
                    reports.add(rptData);
                } else {
                    numSkipped++;
                }

                /*
                 * if ("MTR".equals(prodId.getNnn())) {
                 * 
                 * Matcher m = metarPattern.matcher(temp); if (m.find()) {
                 * String icao = m.group(ICAO_GROUP); String ccc =
                 * staticData.mapICAOToCCC(icao); if (ccc != null) {
                 * prodId.setCcc(ccc); } else { logger.debug(traceId +
                 * " - Could not find CCC for ICAO:" + icao);
                 * prodId.setCcc("CCC"); } if (icao.length() == 4) { icao =
                 * icao.substring(1); } prodId.setXxx(icao); } else { prodId =
                 * REJECT_PIL; } } else if ("TAF".equals(prodId.getNnn())) {
                 * prodId = REJECT_PIL; } else if
                 * ("PIR".equals(prodId.getNnn())) { prodId = REJECT_PIL; }
                 * rpt.setAfosProdId(prodId);
                 */
            }
        }
    }

    /**
     * Parses collective message.
     * 
     * @param buffer
     * @param XXX_id
     * @param parsedMsg
     */
    private void parseCollMsg(StringBuilder buffer, StringBuilder XXX_id,
            StringBuilder parsedMsg) {
        String msgId = null;

        // Check the status of the special case flags and if necessary,
        // skip the special case characters.
        // Do the FWC's first.
        if (checkFouHeader && fouFlag) {
            // Get the length of the FWC header section and save section
            // to store at the beginning of each product.
            if (buffer.charAt(0) == (char) 0x1e) {
                buffer.deleteCharAt(0);
            }

            if (!getTextSegment(buffer, fouHeader, OCSEP)) {
                // if did not encounter a 0x1e or EOM then skip this
                // message
                return;
            }

            checkFouHeader = false;
        }

        String blank = buffer.substring(0,
                buffer.length() < 5 ? buffer.length() : 5);

        if (blank.equals("METAR") || blank.equals("SPECI")
                || blank.equals("TESTM") || blank.equals("TESTS")) {
            if (!safeStrpbrk(buffer, CSPL)) {
                return;
            }

            buffer.deleteCharAt(0);
            if (buffer.charAt(0) == ' ') {
                buffer.deleteCharAt(0);
            }

            reportType = blank;
        } else if (blank.startsWith("TAF")) {
            // Delete "TAF" that starts the data
            safeStrpbrk(buffer, rnl);
            // then any remaining leading carriage control.
            while (buffer.length() > 0) {
                char c = buffer.charAt(0);
                if ((c == '\n') || (c == '\r')) {
                    buffer.deleteCharAt(0);
                } else {
                    break;
                }
            }

            // The next test on blank uses at most three characters
            blank = buffer.substring(0, buffer.length() < 3 ? buffer.length()
                    : 3);
        } else if (pirFlag) {
            if (buffer != null) {
                for (int i = 0; i < buffer.length(); i++) {
                    if (buffer.charAt(i) == '\r') {
                        buffer.setCharAt(i, '\n');
                    }
                }

                // If the pirflag is set, skip the first line of the message, as
                // it
                // is not an id or part of the first collective.
                // safeStrpbrk(buffer, CSPL);

                while (buffer.length() > 0) {
                    char c = buffer.charAt(0);
                    if ((c == ' ') || (c == '\n')) {
                        buffer.deleteCharAt(0);
                    } else {
                        break;
                    }
                }

            }
            pirFlag = false;
        }
        blank = buffer.toString();
        if (blank.startsWith("AMD") || blank.startsWith("COR")) {
            if (safeStrpbrk(buffer, CSPC)) {
                buffer.deleteCharAt(0);
            }
        }

        // Skip junk characters
        while (buffer.length() > 0
                && !(checkCharNum(buffer.charAt(0)) && (buffer.charAt(0) != EOM))) {
            buffer.deleteCharAt(0);
        }

        // Grab the first word of each line to act as the XXX of the afos id.
        if (buffer.length() == 0 || buffer.charAt(0) == EOM) {
            return;
        } else if ((buffer.charAt(0) != (char) 0x1e)
                && (buffer.charAt(0) != EOM)) {
            msgId = assignTextSegment(buffer.toString(), CSPC);
        }

        // Check the length of the XXX, if 3 add a space, if 4 okay.
        if (msgId != null) {
            XXX_id.append(msgId);

            while (XXX_id.length() < 4) {
                XXX_id.append(" ");
            }
        }

        // Get the length of each product by finding the length until the first
        // record separator. Assign to the parsedMsg.
        if (fouFlag) {
            getTextSegment(buffer, parsedMsg, OCSEP);
            // For FWCs, prepend the fouHeader.
            parsedMsg.insert(0, fouHeader);
        } else {
            // Check the length of the message to either the old record
            // separator or the new one and append to the parsedMsg variable.
            getTextSegment(buffer, parsedMsg, MARKERANDEOM);
        }

        if (buffer.length() == 0) {
            if (parsedMsg.length() < MIN_COLL_DATA_LEN) {
                XXX_id.setLength(0);
            } else {
                trim_message(parsedMsg);
            }
        } else if (buffer.charAt(0) == (char) 0x1e) {
            parsedMsg.setLength(parsedMsg.length() - 3);
        } else if (buffer.charAt(0) == '=') {
            if (safeStrpbrk(buffer, CSPL))
                buffer.deleteCharAt(0);
        } else if ((buffer.charAt(0) == EOM)
                && (parsedMsg.length() > (MIN_COLL_DATA_LEN - 1))) {
            checkFouHeader = true;
            trim_message(parsedMsg);
        } else if (parsedMsg.length() < MIN_COLL_DATA_LEN) {
            XXX_id.setLength(0);
        }

        // Prepend the parsed report with the appropriate type, if needed.
        if ((reportType != null)
                && (reportType.equals("METAR") || reportType.equals("SPECI")
                        || reportType.equals("TESTM") || reportType
                            .equals("TESTS")))
            parsedMsg.insert(0, reportType + " ");
    }

    /**
     * Gets the CCC from XXX map from national table and combines this with the
     * NNN that was gotten from the collective table to create the 9 character
     * AFOS id.
     * 
     * @param product_id
     * @param XXX_id
     * @param afos_id
     * @param origin
     * @param newWmoHdr
     * @return
     */
    private boolean makeCollId(AFOSProductId product_id, StringBuilder XXX_id,
            AFOSProductId afos_id, String origin, WMOHeader newWmoHdr) {
        // /TextString CCC_id, newId;
        String CCC_id;
        String newId;
        String trimmedXXX = XXX_id.toString().trim();

        // If this is a national bit product, then CCC = CCC of the current
        // site.
        if ("AAA".equals(afos_id.getCcc()))
            CCC_id = SiteMap.getInstance().getCCCFromXXXCode(siteId);
        // Otherwise, use the national category table to get the CCC from the
        // XXX
        else if ((CCC_id = staticData.mapICAOToCCC(XXX_id.toString())) == null) {
            // We failed to get a CCC from the national_category_table...

            // If the XXX is 3 characters, and the origin starts with K, try
            // prepending K or P (the latter for AK, HI products)
            if (trimmedXXX.length() == 3 && origin.startsWith("K")) {
                newId = "K" + trimmedXXX;
                if ((CCC_id = staticData.mapICAOToCCC(newId)) == null) {
                    newId = "P" + trimmedXXX;
                    if ((CCC_id = staticData.mapICAOToCCC(newId)) == null) {
                        // logger.error("NCF_FAIL to map XXX to CCC: " +
                        // XXX_id);
                        subHeadersSkipped
                                .put(newWmoHdr,
                                        "Product "
                                                + afos_id.toString()
                                                + " is excluded from storage due to "
                                                + newId
                                                + " not present in national_category_table.template");
                        return false;
                    }
                }
            }
            // Otherwise, if the XXX is 3 characters, try prepending the first
            // character of the origin.
            else if (trimmedXXX.length() == 3) {
                newId = origin.charAt(0) + trimmedXXX;
                if ((CCC_id = staticData.mapICAOToCCC(newId)) == null) {
                    subHeadersSkipped
                            .put(newWmoHdr,
                                    "Product "
                                            + afos_id.toString()
                                            + " is excluded from storage due to "
                                            + newId
                                            + " not present in national_category_table.template");
                    return false;
                }
            } else {
                if (!checkCharNum(XXX_id.charAt(0))) {
                    subHeadersSkipped
                            .put(newWmoHdr,
                                    "Product "
                                            + afos_id.toString()
                                            + " is excluded from storage due to incorrect xxxid"
                                            + XXX_id);
                    return false;
                } else
                    // If trimmedXXX has 4 characters and not found in
                    // national_category_table.template.
                    subHeadersSkipped
                            .put(newWmoHdr,
                                    "Product "
                                            + afos_id.toString()
                                            + " is excluded from storage due to "
                                            + trimmedXXX
                                            + " not present in national_category_table.template");
                return false;
            }
        }

        product_id.setCcc(CCC_id);

        // Get the NNN from the afos_id "CCCNNNXXX" that was assigned from the
        // collectives table.
        product_id.setNnn(afos_id.getNnn());

        // Put all three of the id pieces together.
        if (trimmedXXX.length() == 3)
            product_id.setXxx(trimmedXXX);
        else
            product_id.setXxx(XXX_id.substring(1, 4));
        return true;
    }
}
