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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.text.dao.StdTextProductDao;
import com.raytheon.uf.edex.plugin.text.impl.TextDBStaticData;
import com.raytheon.uf.edex.plugin.text.impl.TextSeparatorFactory;
import com.raytheon.uf.edex.plugin.text.impl.WMOReportData;

/**
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer        Description
 * ------------- -------- --------------- --------------------------------------
 * Sep 03, 2008           jkorman         Initial creation
 * Jul 10, 2009  2191     rjpeter         Reimplemented.
 * Jul 26, 2011  10043    rferrel         Modified identifyReports to have
 *                                        checks like A1.
 * Mar 13, 2014  2652     skorolev        Fixed calculation of message end.
 * Apr 01, 2014  2915     dgilling        Support re-factored TextDBStaticData.
 * Dec 09, 2015  5166     kbisanz         Update logging to use SLF4J.
 * Jun 29, 2016  14687    mgamazaychikov  Add special case of FOUS12 KWNO.
 * Jan 13, 2021  7864     randerso        Fix makeStdId to check cccid for empty
 *                                        instead of null. Also added code to
 *                                        try multiple first letters when
 *                                        looking up by xxxid.
 *
 * </pre>
 *
 * @author jkorman
 */

public class StdTextSeparator extends WMOMessageSeparator {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private boolean stdFlg = false;

    /**
     *
     * @param traceId
     * @param siteId
     * @param wmoHeader
     * @param mode
     */
    public StdTextSeparator(String traceId, String siteId, WMOHeader wmoHeader,
            TextDecoderMode mode) {
        super(traceId, siteId, wmoHeader, mode);
    }

    @Override
    protected void createProductId() {
        String ispanId = createIspanId(getWmoHeader());
        String afosId = TextDBStaticData.getProductId(ispanId);

        if (afosId != null) {
            productId = new AFOSProductId(afosId);
        } else {
            productId = NOAFOSPIL;
        }
    }

    /**
     *
     */
    @Override
    protected void identifyReports(byte[] rawData, Headers headers) {
        // AFOSProductId productId = getProductId();
        WMOHeader wmoHeader = getWmoHeader();
        String ispanId = createIspanId(wmoHeader);

        String product_id = TextDBStaticData.getProductId(ispanId);

        // check whether to exclude from decoding for storage
        if ((product_id != null) && TextDBStaticData.isExcluded(product_id)) {
            logger.debug("NCF_ENTRY " + product_id + " is skipped");
            return;
        }

        if ((ispanId.startsWith("FOU") && ispanId.endsWith("KWNO"))
                || ispanId.startsWith("CSCN03") || ispanId.startsWith("FQAC40")
                || ispanId.startsWith("FQGX40") || ispanId.startsWith("FDU")
                || ispanId.startsWith("UAXX") || ispanId.startsWith("FPCN")
                || ispanId.startsWith("WWPN20") || ispanId.startsWith("FAUS2")
                || ispanId.startsWith("FAUS30")
                || (ispanId.startsWith("TC") && !ispanId.startsWith("TCUS"))) {
            stdFlg = true;
        } else {
            stdFlg = false;
        }
        // Set stdFlg for the special case for product FOUS12 KWNO
        if (ispanId.startsWith("FOUS12") && ispanId.endsWith("KWNO")) {
            stdFlg = false;
        }
        int startIndex = wmoHeader.getMessageDataStart();
        int endIndex = TextSeparatorFactory.findDataEnd(rawData);
        if (endIndex <= startIndex) {
            endIndex = rawData.length - 1;
        }
        StringBuilder buffer = new StringBuilder(
                new String(rawData, startIndex, endIndex - startIndex));
        if (!decodeStdMsg(buffer, ispanId, wmoHeader)) {
            numSkipped++;
            // logger.error("NCF_FAIL - " + ispanId + "|"
            // + wmoHeader.getYYGGgg() + " - product not stored");
        }
        // Take a look at the first line. Since we've removed the WMO header
        // this should be the AFOSPIL if it exists!
        /*
         * String pil = getLine(s.getBytes());
         *
         * logger.debug("doSeparate_xxx pil = " + pil);
         *
         * if (pil != null) { if (pil.startsWith("^")) { prodId =
         * pil.substring(1).trim(); logger.debug("doSeparate_xx1 prodId = " +
         * prodId); rpt.setAfosProdId(new AFOSProductId(prodId, getSiteId())); }
         * else if (pil.startsWith("NMC")) { prodId = pil.trim();
         * logger.debug("doSeparate_xx2 prodId = " + prodId);
         * rpt.setAfosProdId(new AFOSProductId(prodId, getSiteId())); } else {
         * if (pil.length() > 6) { prodId = "NONAWIPS"; } else { prodId = pil; }
         * logger.debug("doSeparate_xx3 pil = " + prodId); prodId =
         * makeStdId(ispanId, prodId); logger.debug("doSeparate_xx3 prodId = " +
         * prodId); if (prodId == null) { prodId = "NOAFOSPIL"; }
         * rpt.setAfosProdId(new AFOSProductId(prodId, getSiteId())); } } else {
         * // null data so remove the entry. removeReport = true; }
         */
    }

    private boolean decodeStdMsg(StringBuilder buffer, String ispanId,
            WMOHeader wmoHeader) {
        StringBuilder parsedMsg = new StringBuilder(buffer.length());
        String product_id = null;

        parseStdMsg(buffer, parsedMsg);

        logger.debug("After parsing: " + parsedMsg);

        // Check the nnnxxx for a ^, the sign of a national product which
        // contains the entire 9 char product id after the ^. Or check for a NNN
        // of NMC, also an indicator of a national bit product. For the national
        // bit products, truncate any trailing blanks (similar to what is done
        // in makeStdId) so that the asynchronous product scheduler works
        // correctly. If necessary, get the id from the parsed message,
        // otherwise, make a standard id.
        if (parsedMsg.charAt(0) == '^') {
            product_id = CSPC.split(parsedMsg.substring(1, 10), 2)[0];
        } else if ("NMC".equals(parsedMsg.substring(0, 3))) {
            product_id = CSPC.split(parsedMsg.substring(0, 9), 2)[0];
        } else {
            String nnnxxx;

            // If the second line of parsedMsg is longer than 6 characters,
            // that line does not contain a correct AWIPS ID, and the
            // ispan_table must be used to build the AFOS PIL. Otherwise,
            // the NNNXXX from the second line can be used. For new products,
            // if no product_id is found, use ispanId to store and assign
            // a dummy to product_id so that such products can still be stored
            // during the AFOS to AWIPS transition.
            if ((parsedMsg.length() > 6) && (parsedMsg.charAt(6) != '\r')
                    && (parsedMsg.charAt(6) != '\n')) {
                nnnxxx = "NONAWIPS";
            } else {
                nnnxxx = parsedMsg.substring(0,
                        (parsedMsg.length() > 6 ? 6 : parsedMsg.length()));
            }

            StringBuilder newProductId = new StringBuilder();

            if (!makeStdId(newProductId, nnnxxx, ispanId)) {
                logger.debug("No AFOS ID found; use TTAAii CCCC to store: "
                        + ispanId);
                if (ispanId.startsWith("SXUS70")
                        || ispanId.startsWith("SFPA41")) {
                    return false;
                }

                product_id = "NOAFOSPIL";
            } else {
                product_id = newProductId.toString();
            }
        }

        // check whether incoming product is excluded from storage
        if (TextDBStaticData.isExcluded(product_id)) {
            logger.debug("NCF_ENTRY " + product_id + " is skipped");
            return true;
        }

        /*
         * Moved duplicate checking to the database
         *
         * // If this is a warning product, check number of products with same
         * // wmo_id, // cccc, header_time. If there are more than one, compare
         * all with // incoming // product to see if it is a duplicate.
         * List<String> prodList = null;
         *
         * if (TextDecoderMode.WARN.equals(getMode()) &&
         * wmoHeader.getWmoHeader().charAt(0) == 'W') { if
         * (product_id.startsWith("AAA")) { product_id = siteId +
         * product_id.substring(product_id.length() - 3); }
         *
         * prodList = getProdInSameMin(wmoHeader, product_id); }
         *
         * // Check for duplicate messages. If the incoming message is not a //
         * duplicate, write the WMO header and parsed message to the database.
         * // If it is a duplicate, do not write to the database. boolean
         * dupProduct = false; if (prodList != null && prodList.size() > 1) {
         * logger
         * .info("More than one products with same WMO header exit in database"
         * ); dupProduct = duplicateCheck(wmoHeader, product_id, prodList,
         * parsedMsg); } else { dupProduct = duplicateCheck(wmoHeader,
         * product_id, parsedMsg, siteId); }
         *
         * if (!dupProduct) { // add to reports WMOReportData rptData = new
         * WMOReportData(wmoHeader, new AFOSProductId(product_id),
         * parsedMsg.toString()); reports.add(rptData); } else {
         * logger.info(product_id + " is a duplicate, not stored."); }
         */

        // add to reports
        if (product_id.startsWith("AAA")) {
            product_id = SiteMap.getInstance().getCCCFromXXXCode(siteId)
                    + product_id.substring(3);
        }
        WMOReportData rptData = new WMOReportData(wmoHeader,
                new AFOSProductId(product_id), parsedMsg.toString());
        reports.add(rptData);

        return true;
    }

    private void parseStdMsg(StringBuilder buffer, StringBuilder parsedMsg) {
        logger.debug("Parsing message.");

        // messages are supposed to end with 6 spaces and a \3
        int endOfMessage = buffer.indexOf("\3");

        // Check for a proper message by verifying the EOM character.
        if (endOfMessage > 6) {
            // drop six spaces and EOM char
            parsedMsg.append(buffer.substring(0, endOfMessage - 6));
        } else {
            parsedMsg.append(buffer.toString());
        }

        // chop off any trailing control characters
        while (Character
                .isISOControl(parsedMsg.charAt(parsedMsg.length() - 1))) {
            parsedMsg.setLength(parsedMsg.length() - 1);
        }
    }

    private boolean makeStdId(StringBuilder product_id, String nnnxxx,
            String ispanId) {
        String newId;
        String cccId;
        String nnnId = "";
        String origin = ispanId.substring(ispanId.length() - 4,
                ispanId.length());

        boolean badflag = false;

        // If the second line of the product is an AWIPS ID (6 characters),
        // use it to get nnnId.
        if (!"NONAWIPS".equals(nnnxxx)) {
            logger.trace("Making Id. NNNXXX: " + nnnxxx);
            nnnId = nnnxxx.substring(0, 3);
        }

        logger.trace("Origin in makeStdId: " + origin);
        // Check the status of the standard flag(s) to see if any special cases
        // apply otherwise, try to map the origin to the CCC with the afos
        // table.
        if (stdFlg) {
            logger.trace("Mapping from stdflag.");
            newId = TextDBStaticData.getProductId(ispanId);
            if (newId == null) {
                // logger.error("Unable to create AFOS id - ispan table.");
                return false;
            } else {
                product_id.append(newId);
                logger.trace("stdflag id: " + newId);
                return true;
            }
        } else if (nnnxxx.startsWith("METAR")) {
            logger.trace("Mapping with METAR.");

            // Check to see if the product is a non-collective METAR (out of
            // country), and map against the ISPAN table to get the id.
            newId = TextDBStaticData.getProductId(ispanId);
            if (newId == null) {
                // logger.error("Unable to create AFOS id - ispan table");
                return false;
            } else {
                product_id.append(newId);
                logger.trace("Metar Id: " + newId);
                return true;
            }
        }

        // Try to map the nnnId with the bit table for a national bit product;
        // otherwise, check the AFOS table for the ccc value of the origin.
        newId = TextDBStaticData.getSiteIdFromNNN(nnnId, siteId);

        if (newId == null) {
            cccId = TextDBStaticData.getAFOSTableMap(origin);

            /* if not found, try looking up by xxx */
            if (cccId.isEmpty()) {
                String xxxId = null;
                if (nnnxxx.length() >= 6) {
                    xxxId = nnnxxx.substring(3, 6);
                } else if (nnnxxx.length() > 3) {
                    xxxId = nnnxxx.substring(3);
                }

                if (xxxId != null) {
                    /* Try axxx where a is one of the afos first letters */
                    for (char c : StdTextProductDao
                            .getPreferredafosfirstletter()) {
                        cccId = TextDBStaticData.getAFOSTableMap(c + xxxId);
                        if (!cccId.isEmpty()) {
                            break;
                        }
                    }
                }
                if (cccId.isEmpty()) {
                    newId = "";
                } else {
                    logger.trace("ccc mapped from xxx");
                    // Create the ID
                    newId = cccId + nnnxxx;
                }
            } else {
                logger.trace("Mapping from AFOS.");
                // Create the id.
                newId = cccId + nnnxxx;
            }
        } else {
            logger.trace("Mapping from bitTable.");
            newId += nnnxxx;
            logger.trace("Product is national bit: " + newId);
        }

        // Check the last 3 char of the id for spaces. If there is one,
        // truncate the id to exclude the spaces.
        newId = newId.trim();

        // Check for ids less than 7 (no xxx) or > 9 (bad id).
        logger.trace("Checking Id: " + newId);
        if ((newId.length() < 7) || (newId.length() > 9)) {
            // If the length is bad, try mapping to the ispan table with the
            // ispan id.
            newId = TextDBStaticData.getProductId(ispanId);
            if (newId == null) {
                // logger.error("Unable to create AFOS id - ispan table");
                return false;
            } else {
                logger.trace("Bad length, mapping from ispan.");
                product_id.append(newId);
                logger.trace("Length id: " + newId);
                return true;
            }
        } else {
            // Check the product_id for all numbers and spaces in the middle.
            logger.debug("newId: " + newId);
            logger.debug("ispanId: " + ispanId);

            if (checkCharNum(newId.charAt(0))) {
                for (int i = 1; i < newId.length(); i++) {
                    logger.trace("Checking letters and numbers.");
                    if (!checkCharNum(newId.charAt(i))) {
                        badflag = true;
                        break;
                    }
                }
                if (badflag) {
                    newId = TextDBStaticData.getProductId(ispanId);
                    if (newId == null) {
                        // logger.error("Invalid product - ispan table.");
                        return false;
                    } else {
                        logger.trace(
                                "Numbers and letters bad mapping from ispan."
                                        + ispanId);
                        product_id.append(newId);
                        logger.trace("Character Id: " + newId);
                        return true;
                    }
                }
            } else {
                newId = TextDBStaticData.getProductId(ispanId);
                if (newId == null) {
                    // logger.error("Invalid product - ispan table.");
                    return false;
                } else {
                    logger.trace("Id is all numbers mapping from ispan.");
                    product_id.append(newId);
                    logger.trace("All num id: " + newId);
                    return true;
                }
            }
        }

        product_id.append(newId);
        logger.trace("All ok id: " + newId);
        return true;
    }
}
