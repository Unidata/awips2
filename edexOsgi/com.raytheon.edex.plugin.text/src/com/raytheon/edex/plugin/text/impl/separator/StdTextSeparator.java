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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.text.impl.TextSeparatorFactory;
import com.raytheon.edex.textdb.dbapi.impl.TextDBStaticData;
import com.raytheon.edex.textdb.dbapi.impl.WMOReportData;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2008             jkorman     Initial creation
 * Jul 10, 2009 2191       rjpeter     Reimplemented.
 * Jul 26, 2011 10043      rferrel     Modified identifyReports to
 *                                     have checks like A1.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class StdTextSeparator extends WMOMessageSeparator {

    private final Log logger = LogFactory.getLog(getClass());

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
        String afosId = staticData.getProductId(ispanId);

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

        String product_id = TextDBStaticData.instance(siteId).getProductId(
                ispanId);

        // check whether to exclude from decoding for storage
        if (product_id != null && staticData.isExcluded(product_id.toString())) {
            logger.debug("NCF_ENTRY " + product_id.toString() + " is skipped");
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
        int startIndex = wmoHeader.getMessageDataStart();
        int endIndex = TextSeparatorFactory.findDataEnd(rawData);
        StringBuilder buffer = new StringBuilder(new String(rawData,
                startIndex, endIndex - startIndex));
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
            if (parsedMsg.length() > 6 && parsedMsg.charAt(6) != '\r'
                    && parsedMsg.charAt(6) != '\n') {
                nnnxxx = "NONAWIPS";
            } else {
                nnnxxx = parsedMsg.substring(0, (parsedMsg.length() > 6 ? 6
                        : parsedMsg.length()));
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
        if (staticData.isExcluded(product_id.toString())) {
            logger.debug("NCF_ENTRY " + product_id.toString() + " is skipped");
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
        WMOReportData rptData = new WMOReportData(wmoHeader, new AFOSProductId(
                product_id), parsedMsg.toString());
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
        while (Character.isISOControl(parsedMsg.charAt(parsedMsg.length() - 1))) {
            parsedMsg.setLength(parsedMsg.length() - 1);
        }
    }

    /**
     * 
     * @param rpt
     * @return
     */
    /*
     * private boolean doSeparate(WMOReportData rpt) {
     * 
     * boolean standard = false;
     * 
     * boolean removeReport = false;
     * 
     * String s = rpt.getReportData();
     * 
     * String prodId = null;
     * 
     * prodId = staticData.getProductId(ispanId); if (s.length() < 10) { return
     * true; }
     * 
     * if (prodId != null) { rpt.setAfosProdId(new AFOSProductId(prodId,
     * getSiteId())); return false; } else { String tt = ispanId.substring(0,
     * 2); if (!Boolean.TRUE.equals(TT_MAP_A.get(tt))) { if
     * (staticData.matchStdCollective(dataDes) == null) { if
     * (staticData.matchUACollective(dataDes) == null) { if
     * (TextDecoderMode.WARN.equals(getMode())) { standard = true; } else {
     * standard = (!"NOUS71KNCF".equals(ispanId) &&
     * !"NTUS96KNCF".equals(ispanId) && !"NTUS98KNCF" .equals(ispanId)); } } } }
     * else if ("FRUS45".equals(dataDes)) { standard = true; } else {
     * removeReport = true; } } stdFlg = false;
     * 
     * if (standard) { if (TextDecoderMode.WARN.equals(getMode()) && (prodId !=
     * null)) { if (isExcluded()) { return true; } } } else { // Not standard
     * data. removeReport = true; } return removeReport; }
     */

    /*
     * TODO FIX THIS public boolean makeStdId(StringBuilder product_id, String
     * ispanId, String nnnxxx) {
     * 
     * String productId = null; if (stdFlg) { logger.debug("makeStdId.stdFlg");
     * if (staticData.isMappedISpanId(ispanId)) { productId =
     * staticData.getProductId(ispanId); } else { productId = nnnxxx;
     * logger.debug(traceId + "- Unable to create afos id from ispanId " +
     * ispanId); } } else if ("METAR".equals(nnnxxx)) {
     * logger.debug("makeStdId.METAR"); if (staticData.isMappedISpanId(ispanId))
     * { productId = staticData.getProductId(ispanId); } else {
     * logger.debug(traceId + "- Unable to create afos id from ispanId " +
     * ispanId); } }
     * 
     * String id = null; if (productId != null) { if
     * (!"NONAWIPS".equals(nnnxxx)) { id =
     * staticData.getSiteIdFromNNN(nnnxxx.substring(0, 3)); }
     * 
     * if (id != null) { productId = id + nnnxxx; } else { String icao = "K" +
     * nnnxxx.substring(3);
     * 
     * String ccc = staticData.mapICAOToCCC(icao); if (ccc != null) { productId
     * = ccc + nnnxxx; } } } return true; }
     */

    // -- public --------------------------------------------------------------
    // Name: StdTextDecoder::makeStdId()
    //
    // Takes 1st 6 chars from product and prepends proper CCC from afosTable. Or
    // will use dataDes and origin to map from ispanTable.
    //
    // ---------------------------------------------------------------------------
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
        if (!nnnxxx.equals("NONAWIPS")) {
            logger.trace("Making Id. NNNXXX: " + nnnxxx);
            nnnId = nnnxxx.substring(0, 3);
        }

        logger.trace("Origin in makeStdId: " + origin);
        // Check the status of the standard flag(s) to see if any special cases
        // apply otherwise, try to map the origin to the CCC with the afos
        // table.
        if (stdFlg) {
            logger.trace("Mapping from stdflag.");
            newId = staticData.getProductId(ispanId);
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
            newId = staticData.getProductId(ispanId);
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
        newId = staticData.getSiteIdFromNNN(nnnId);

        if (newId == null) {
            cccId = staticData.getAFOSTableMap(origin);
            if (cccId == null) {
                if (nnnxxx.length() >= 6) {
                    cccId = staticData.getAFOSTableMap("K" + nnnxxx.substring(3, 6));
                } else if (nnnxxx.length() > 3) {
                    cccId = staticData.getAFOSTableMap("K" + nnnxxx.substring(3));
                }
                if (cccId == null) { // KWBC RCM,VER
                    // logger.error("Can't get ccc: " + origin);
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
            newId = staticData.getProductId(ispanId);
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
                    newId = staticData.getProductId(ispanId);
                    if (newId == null) {
                        // logger.error("Invalid product - ispan table.");
                        return false;
                    } else {
                        logger.trace("Numbers and letters bad mapping from ispan."
                                + ispanId);
                        product_id.append(newId);
                        logger.trace("Character Id: " + newId);
                        return true;
                    }
                }
            } else {
                newId = staticData.getProductId(ispanId);
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
