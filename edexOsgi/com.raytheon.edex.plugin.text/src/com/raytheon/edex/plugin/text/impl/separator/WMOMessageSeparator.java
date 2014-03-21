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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.textdb.dbapi.impl.TextDBStaticData;
import com.raytheon.edex.textdb.dbapi.impl.WMOReportData;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder implementation for text products
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20090327     2151        jkorman     Modified separate to remove leading/trailing
 *                                      white space.
 * Jul 10, 2009 2191        rjpeter     Reimplemented.
 * Mar 04, 2014 2652        skorolev    Corrected NNNXXX pattern.
 * Mar 14, 2014 2652        skorolev    Changed logging for skipped headers.
 * </pre>
 * 
 * @author
 * @version 1
 */
public abstract class WMOMessageSeparator extends AbstractRecordSeparator {

    public static final AFOSProductId NOAFOSPIL = new AFOSProductId("NOA",
            "FOS", "PIL");

    public static final AFOSProductId REJECT_PIL = new AFOSProductId("---",
            "---", "---");

    public static final AFOSProductId NONAWIPS = new AFOSProductId("NON",
            "AWI", "PS");

    protected static final char EOM = 3;

    protected static final Pattern MARKERANDEOM = Pattern.compile("[="
            + (char) 0x1e + EOM + "]");

    protected static final Pattern CSEP = Pattern.compile("[=" + EOM + "]");

    protected static final Pattern OCSEP = Pattern.compile("[" + (char) 0x1e
            + EOM + "]");

    protected static final Pattern CSPC = Pattern.compile(" ");

    protected static final Pattern CSPL = Pattern.compile("[ \n]");

    protected static final Pattern rnl = Pattern.compile("[\r\n]");

    protected static final Pattern nl = Pattern.compile("\n");

    protected static final int MIN_COLL_DATA_LEN = 8;

    protected static final int MAX_SECND_LINE_LEN = 10;

    protected static final Pattern NNNXXX = Pattern
            .compile("\\w{3,6}(?:\\s{1,2})?[\\r\\n]+(?:" + (char) 0x1e + ")?");

    private final Log logger = LogFactory.getLog(getClass());

    protected final WMOHeader wmoHeader;

    protected final String traceId;

    protected final TextDBStaticData staticData;

    protected final String siteId;

    protected final TextDecoderMode mode;

    protected AFOSProductId productId = null;

    protected int currentReport = -1;

    protected List<WMOReportData> reports = null;

    protected int numSkipped = 0;

    // used for collective messages to show the skipped header and reason to
    // skip.
    protected Map<WMOHeader, String> subHeadersSkipped = new HashMap<WMOHeader, String>();

    /**
     * 
     * @param traceId
     */
    public WMOMessageSeparator(String traceId, String siteId,
            WMOHeader wmoHeader) {
        this(traceId, siteId, wmoHeader, TextDecoderMode.STD);
    }

    /**
     * 
     * @param traceId
     */
    public WMOMessageSeparator(String traceId, String siteId,
            WMOHeader wmoHeader, TextDecoderMode mode) {
        this.traceId = traceId;
        this.siteId = siteId;
        this.wmoHeader = wmoHeader;
        this.mode = mode;
        staticData = TextDBStaticData.instance(siteId);
        // localCCC = staticData.getProductId("LOCALCCC  ").substring(0, 3);
    }

    /**
     * 
     */
    @Override
    public WMOReportData next() {
        WMOReportData data = null;
        if (hasNext()) {
            data = reports.get(currentReport++);
        }
        return data;
    }

    /**
     * Is there another record available?
     * 
     * @return Is there another record available?
     */
    @Override
    public boolean hasNext() {
        return ((reports != null) && (reports.size() > 0) && (currentReport < reports
                .size()));
    }

    /**
     * Set the raw message data and invoke the internal message separation
     * process.
     * 
     * @param rawMessage
     *            The raw weather text message.
     */
    @Override
    public void setData(byte[] rawMessage, Headers headers) {
        currentReport = -1;
        reports = new ArrayList<WMOReportData>();
        createProductId();

        if (rawMessage != null) {
            identifyReports(rawMessage, headers);
        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        } else {
            logger.debug(traceId + " - setData():No reports found in data.");
        }
    }

    /**
     * Get the WMO header associated with the message data.
     * 
     * @return The WMO header.
     */
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @return the productId
     */
    public AFOSProductId getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(AFOSProductId productId) {
        this.productId = productId;
    }

    /**
     * @return the traceId
     */
    public String getTraceId() {
        return traceId;
    }

    /**
     * @return the siteId
     */
    public String getSiteId() {
        return siteId;
    }

    /**
     * 
     * @return
     */
    public TextDecoderMode getMode() {
        return mode;
    }

    /**
     * Parse rawData to identify the reports.
     */
    protected abstract void identifyReports(byte[] rawData, Headers headers);

    /**
     * 
     * @param data
     * @return
     */
    /*
     * public static String getLine(byte[] data) { return getLine(data, 0); }
     */
    /**
     * 
     * @param data
     * @return
     */
    public static String getLine(byte[] data, int startIndex) {
        String line = null;
        if (data != null) {
            if (data.length > 0 && startIndex >= 0 && startIndex < data.length) {
                for (int i = startIndex; i < data.length; i++) {
                    if ((data[i] == 10) || (data[i] == 13)) {
                        line = new String(data, startIndex, i - startIndex);
                        break;
                    }
                } // for
                  // if we didn't create a String, then no carriage
                  // control was found. Return the entire array as
                  // a String.
                if (line == null) {
                    line = new String(data, startIndex, data.length
                            - startIndex);
                }
            } else {
                line = new String();
            }
        }
        return line;
    }

    /**
     * 
     * @return
     */
    protected abstract void createProductId();

    public int getMessagesSkipped() {
        return numSkipped;
    }


    /*
     * private AFOSProductId createProductId() { AFOSProductId productId = null;
     * 
     * String tt = getWmoHeader().getWmoHeader().substring(0, 2); if
     * ("SA".equals(tt)) { productId = new AFOSProductId("CCC", "MTR", "XXX"); }
     * else if ("SP".equals(tt)) { productId = new AFOSProductId("CCC", "MTR",
     * "XXX"); } else if ("FR".equals(tt)) { productId = new
     * AFOSProductId("CCC", "TWB", "XXX"); } else if ("FT".equals(tt)) {
     * productId = new AFOSProductId("CCC", "TAF", "XXX"); } else { productId =
     * NOAFOSPIL; } return productId; }
     */

    /**
     * 
     * @param wmoHdr
     * @return
     */
    public static String createDataDes(WMOHeader wmoHdr) {
        return wmoHdr.getTtaaii();
    }

    /**
     * 
     * @param wmoHdr
     * @return
     */
    public static String createIspanId(WMOHeader wmoHdr) {
        return createDataDes(wmoHdr) + wmoHdr.getCccc();
    }

    /**
     * Updates the passed StringBuilder src, removing all characters that occur
     * before the first occurrence of any letter in the passed charSet or \0.
     * Returns false if no characters in the charSet are contained in the
     * StringBuilder.
     * 
     * @param sb
     * @param charSet
     * @return
     */
    static boolean safeStrpbrk(StringBuilder src, Pattern charSet) {
        return getTextSegment(src, null, charSet);
    }

    /**
     * Updates the passed StringBuilder src, removing all characters that occur
     * before the first occurrence of any letter in the passed charSet or \0 and
     * put the characters into out StringBuilder. If out is null, the characters
     * are just removed from src. Returns false if no characters in the charSet
     * are contained in the StringBuilder.
     * 
     * @param src
     * @param out
     * @param charSet
     * @return
     */
    static boolean getTextSegment(StringBuilder src, StringBuilder out,
            Pattern charSet) {
        String s = src.toString();
        String[] sArr = charSet.split(s, 2);

        if (out != null) {
            out.setLength(0);
            out.append(sArr[0]);
        }

        if (sArr.length != 2) {
            // no pattern found
            src.setLength(0);
            return false;
        }

        src.delete(0, sArr[0].length());
        return true;
    }

    /**
     * Returns
     * 
     * @param src
     * @param out
     * @param charSet
     * @return
     */
    static String assignTextSegment(String src, Pattern charSet) {
        String[] sArr = charSet.split(src, 2);

        return sArr[0];
    }

    // -- fileScope
    // --------------------------------------------------------------
    // checkCharNum ()
    //
    // Check character to see if it is a capital letter or number by checking
    // against the ASCII numbers.
    //
    // ---------------------------------------------------------------------------
    static boolean checkCharNum(char x) {
        if ((x > 64) && (x < 91))
            return true;
        else if ((x > 47) && (x < 58))
            return true;

        return false;
    }

    // -- fileScope
    // --------------------------------------------------------------
    // trim_message()
    //
    // Removes trailing '\r' and '\n' characters from a TextString.
    //
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    protected static void trim_message(StringBuilder msg) {
        char endChar = msg.charAt(msg.length() - 1);

        while (msg.length() > 0 && (endChar == '\r' || endChar == '\n')) {
            msg.setLength(msg.length() - 1);
            endChar = msg.charAt(msg.length() - 1);
        }
    }

    public Map<WMOHeader, String> getSubHeadersSkipped() {
        return subHeadersSkipped;
    }

    public static final void main(String[] args) {

        StringBuilder sb = new StringBuilder(
                "\r\r\nKOFF 1912/20/15\n\n\r     BECMG");

        safeStrpbrk(sb, nl);

        // Pattern NNNXXX = Pattern.compile("\\w{4,6}(?:\\s{1,2})?[\\r\\n]+(?:"
        // + (char) 0x1e + ")?");
        //
        //
        // Matcher m = NNNXXX.matcher("PIRUS\r\rOMA UUA /OV");
        //
        // if(m.find()) {
        // for(int i = 0;i <= m.groupCount();i++) {
        // System.out.println(m.group(i));
        // }
        // }
    }
}
