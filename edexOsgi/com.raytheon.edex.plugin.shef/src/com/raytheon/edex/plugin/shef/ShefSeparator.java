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
package com.raytheon.edex.plugin.shef;

import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.WMO_HEADER;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.plugin.shef.util.SHEFErrors;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFErrorCodes;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- ---------------------------------------
 * July2006     3, 14       Phillippe   Initial Creation    
 * 28Mar2008    387         M. Duff     Modified to use WMOHeader object and to
 *                                      correctly parse multiple SHEF formats
 * 12/03/2008               chammack    Camel refactor
 * 12/xx/2010               jkorman     Complete rewrite.
 * 11/29/2012               lbousaidi   fixed the decoding issue when the shef starts
 *                                      with :
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ShefSeparator extends AbstractRecordSeparator {

    private enum Continuation {
        NONE, CONTINUE, ERROR;
    }

    public static class ShefDecoderInput {
        public String record;

        public String rawMessage;

        public WMOHeader wmoHeader;

        public Date productDate;

        public String awipsHeader;

        public String traceId;
    }

    private static final Log log = LogFactory.getLog(ShefSeparator.class);

    private static final SHEFErrors ERR_LOGGER = SHEFErrors
            .registerLogger(ShefSeparator.class);

    // Regex used for separating multi-record files
    private static final String SHEFTYPE = "^( )*\\.([ABE])(((R)(\\d{1,})?)|(\\d{1,}))?([ \t]+(.*))?";

    // private static final String SHEFTYPE =
    // "^( )*\\.([ABE])(((R)(\\d{1,})?)|(\\d{1,}))? +(.*)";

    // private static final String SHEFTYPE =
    // "^\\.([ABE])(R|[ 0123456789])?(.*)";

    private static final Pattern P_SHEFTYPE = Pattern.compile(SHEFTYPE);

    private static final String SHEFEND = "^( )*\\.END( *)(:.*)?";

    private static final Pattern P_SHEFEND = Pattern.compile(SHEFEND);

    private static final String LINE_FMT = "%s:%05d";

    private static boolean removeLeadingComments = false;

    /** The WMO header */
    private WMOHeader wmoHeader = null;

    private String traceId = null;

    /** The AWIPS Header */
    private String awipsHeader = null;

    private byte[] messageData = null;

    private int currentRecord = -1;

    /** List of records contained in file */
    private List<String> records;

    /** Holds the original message */
    private String rawData = null;

    private boolean inTest = false;

    /** Holds the arrival time of the message */
    private Date productTime = new Date();

    /**
     * Constructor
     */
    public ShefSeparator() {
        records = new ArrayList<String>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IRecordSeparator#setData(byte[])
     */
    public void setData(byte[] data, Headers headers) {
        if ((data != null) && (data.length > 0)) {
            rawData = new String(data);
            currentRecord = -1;

            /* Extracts the header */
            wmoHeader = new WMOHeader(data, headers);
            if (wmoHeader.isValid()) {
                if (traceId == null) {
                    traceId = wmoHeader.getWmoHeader();
                    log.info("TraceId set to WMOHeader = " + traceId);
                }
                // TODO: DR 14 - Shef time changes.
                Calendar c = wmoHeader.getHeaderDate();
                if (c != null) {
                    productTime = c.getTime();
                }
                messageData = DecoderTools.stripWMOHeader(data, WMO_HEADER);
            } else {
                // No WMO header found or bad one, so process as best
                // as we can.
                Calendar c = TimeTools.getSystemCalendar();
                if (c != null) {
                    productTime = c.getTime();
                }
                messageData = data;
            }
            setData();
        }

        if ((records != null) && (records.size() > 0)) {
            currentRecord = 0;
        }
    }

    /**
     * 
     */
    private void setData() {
        if (messageData != null) {
            doSeparate(new String(messageData));
        } else {
            log.error(traceId + "- Invalid WMOHeader "
                    + wmoHeader.getWmoHeader());
        }
    }

    public static ShefSeparator separate(byte[] data, Headers headers) {
        ShefSeparator separator = new ShefSeparator();
        try {
            if (headers != null) {
                separator.traceId = (String) headers.get("traceId");
            }
            separator.setData(data, headers);
        } catch (Exception e) {
            if(log.isDebugEnabled()) {
                log.error(separator.traceId + "- Error separating data.", e);
            } else {
                log.error(separator.traceId + "- Error separating data " + e.toString());
            }
        }
        return separator;
    }

    /**
     * 
     * @return
     */
    public String getTraceId() {
        return traceId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#nextRecord()
     */
    public boolean hasNext() {
        return ((records != null) && (records.size() > 0) && (currentRecord < records
                .size()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#next()
     */
    @Override
    public ShefDecoderInput next() {
        ShefDecoderInput data = null;
        if (hasNext()) {
            data = new ShefDecoderInput();
            data.record = records.get(currentRecord++);
            data.rawMessage = getRawMessage();
            data.wmoHeader = getWmoHeader();
            data.awipsHeader = getAwipsHeader();
            data.productDate = getProductTime();
            data.traceId = traceId;
        }
        return data;
    }

    /**
     * Split the String into individual messages
     * 
     * @param messagepattern
     *            - SHEF Message
     */
    private void doSeparate(String message) {

        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                    new ByteArrayInputStream(message.getBytes())));

            reader.mark(0);
            StringBuilder buffer = null;
            StringBuilder bRecBuffer = null;

            String assemble = null;
            String mRevised = " ";
            String cRevised = " ";
            Continuation continued = Continuation.NONE;
            String lineData = null;
            boolean leadingSpaces = false;

            String currRec = null;
            // haven't seen the awips header (should be first line) so evaluate
            // for a possible awips header.
            int lineNumber = 1;

            while ((currRec = reader.readLine()) != null) {
                if (currRec.length() > 0) {
                    String c = currRec.substring(0, 1);
                    // Don't pick up possible comments or the beginning of
                    // a data line!
                    if (".".equals(c) || ":".equals(c)) {
                        awipsHeader = null;
                        reader.close();
                        reader = new BufferedReader(new InputStreamReader(
                                new ByteArrayInputStream(message.getBytes())));
                        lineNumber = 1;
                    } else {
                        awipsHeader = currRec.trim();
                    }
                    break;
                }
                lineNumber++;
            }
            Matcher m = null;
            String currLine = null;
            boolean endOfReport = false;
            // Are we assembling the data section of a B record?
            boolean bData = false;
            while ((currRec = reader.readLine()) != null) {
                lineNumber++;
                currRec = removeInternalComments(currRec);
                currLine = currRec;
                if (currRec.length() > 0) {
                    // need to find out if this report is terminated with an '='
                    StringBuilder sb = new StringBuilder(currLine);
                    while ((sb.length() > 0)
                            && ('=' == sb.charAt(sb.length() - 1))) {
                        endOfReport = true;
                        sb.deleteCharAt(sb.length() - 1);
                    }
                    currRec = sb.toString();
                    if ((currRec.charAt(0) == '.') || ("B".equals(assemble))) {
                        // Start by looking for a possible .END, makes it easier
                        // later
                        // when looking for .E messages!
                        m = P_SHEFEND.matcher(currRec);
                        if (m.find()) {
                            // Normal end - no leading spaces
                            if (m.group(1) == null) {
                                if ("B".equals(assemble)) {
                                    // We are assembling a B Record
                                    if (bRecBuffer != null) {
                                        buffer.append(bRecBuffer);
                                    }
                                    buffer.append("\n.END");
                                    records.add(buffer.toString());
                                } else {
                                    // We found an .END directive and didn't
                                    // expect one.
                                    ERR_LOGGER.warning(getClass(), String
                                            .format(LINE_FMT, traceId,
                                                    lineNumber));
                                    ERR_LOGGER.warning(getClass(), currLine);
                                    ERR_LOGGER.warning(getClass(), "   ?");
                                    ERR_LOGGER.warning(getClass(),
                                            SHEFErrorCodes.LOG_068);
                                }
                            } else {
                                ERR_LOGGER.error(getClass(), String.format(
                                        LINE_FMT, traceId, lineNumber));
                                ERR_LOGGER.error(getClass(), currLine);
                                ERR_LOGGER.error(getClass(),
                                        String.format("%s ?", m.group(1)));
                                ERR_LOGGER.warning(getClass(),
                                        SHEFErrorCodes.LOG_006);
                                // Are we assembling a B Record?
                                if ("B".equals(assemble)) {
                                    buffer.append("\n.END");
                                    records.add(buffer.toString());
                                } else {
                                    ERR_LOGGER.warning(getClass(), String
                                            .format(LINE_FMT, traceId,
                                                    lineNumber));
                                    ERR_LOGGER.warning(getClass(), currRec);
                                    ERR_LOGGER.warning(getClass(), "   ?");
                                    ERR_LOGGER.warning(getClass(),
                                            SHEFErrorCodes.LOG_068);
                                }
                            }
                            buffer = null;
                            bRecBuffer = null;
                            assemble = null;
                            continued = Continuation.NONE;
                            bData = false;
                        } else {
                            m = P_SHEFTYPE.matcher(currRec);
                            if (m.find()) {
                                leadingSpaces = (m.group(1) != null);
                                // No leading spaces on the line
                                cRevised = (m.group(5) != null) ? "R" : " ";
                                // ss will receive the sequence number if it
                                // exists.
                                String ss = ("R".equals(cRevised)) ? m.group(6)
                                        : m.group(7);

                                int len = (ss != null) ? ss.length() : -1;

                                if ((len == 1) || (len == 2)) {
                                    continued = Continuation.CONTINUE;
                                } else if (len > 2) {
                                    continued = Continuation.ERROR;
                                } else {
                                    continued = Continuation.NONE;
                                }
                                // are we starting a new record?
                                if (assemble == null) {
                                    mRevised = cRevised;
                                    cRevised = " ";
                                }
                                lineData = m.group(9);
                                if (Continuation.CONTINUE.equals(continued)) {
                                    if (lineData != null) {

                                        if ("B".equals(assemble)) {
                                            if (bData) {
                                                // We've hit a continuation
                                                // line and had been
                                                // assembling the B record
                                                // data section.
                                                ERR_LOGGER.warning(getClass(),
                                                        String.format(LINE_FMT,
                                                                traceId,
                                                                lineNumber));
                                                ERR_LOGGER.warning(getClass(),
                                                        buffer.toString());
                                                ERR_LOGGER.warning(getClass(),
                                                        bRecBuffer.toString());
                                                ERR_LOGGER.warning(getClass(),
                                                        "  ?");
                                                ERR_LOGGER.warning(getClass(),
                                                        SHEFErrorCodes.LOG_082);
                                                bRecBuffer = null;
                                                bData = false;
                                                buffer = assembleContinuedLines(
                                                        buffer, lineData);
                                                continue;
                                            }
                                        }
                                        // Is this continued record the same as
                                        // we are currently assembling?
                                        if (assemble == null) {
                                            // If we get here something is
                                            // wrong.

                                        } else if (assemble.equals(m.group(2))) {
                                            if ("R".equals(mRevised)) {
                                                buffer = assembleContinuedLines(
                                                        buffer, lineData);
                                            } else if (" ".equals(cRevised)) {
                                                if ("R".equals(mRevised)) {
                                                    // the revision on this
                                                    // continuation line is not
                                                    // the same as the main
                                                    // report.
                                                    ERR_LOGGER
                                                            .error(getClass(),
                                                                    String.format(
                                                                            LINE_FMT,
                                                                            traceId,
                                                                            lineNumber));
                                                    ERR_LOGGER
                                                            .error(getClass(),
                                                                    currRec);
                                                    ERR_LOGGER.error(
                                                            getClass(), " ?");
                                                    ERR_LOGGER
                                                            .error(getClass(),
                                                                    SHEFErrorCodes.LOG_010);
                                                } else {
                                                    buffer = assembleContinuedLines(
                                                            buffer, lineData);
                                                }
                                            }
                                        } else {
                                            // We have a continuation line, but
                                            // its from a different record
                                            // type than we started with.
                                            ERR_LOGGER.error(getClass(), String
                                                    .format(LINE_FMT, traceId,
                                                            lineNumber));
                                            ERR_LOGGER.error(getClass(),
                                                    currRec);
                                            ERR_LOGGER.error(getClass(), " ?");
                                            ERR_LOGGER.error(getClass(),
                                                    SHEFErrorCodes.LOG_009);
                                        }
                                    } else {
                                        // We have a continuation line with no
                                        // data
                                        ERR_LOGGER.warning(getClass(), String
                                                .format(LINE_FMT, traceId,
                                                        lineNumber));
                                        ERR_LOGGER.warning(getClass(), currRec);
                                        ERR_LOGGER.warning(getClass(), " ?");
                                        ERR_LOGGER.warning(getClass(),
                                                SHEFErrorCodes.LOG_067);
                                    }
                                } else if (continued.equals(Continuation.NONE)) {
                                    // Not a continuation line

                                    // Check to see if we were assembling a B
                                    // record.
                                    // if we get here with B then we didn't see
                                    // an
                                    // .END directive. complain, insert the .END
                                    // and
                                    // continue as normal
                                    if ("B".equals(assemble)) {
                                        ERR_LOGGER.warning(getClass(), String
                                                .format(LINE_FMT, traceId,
                                                        lineNumber));
                                        ERR_LOGGER.warning(getClass(), currRec);
                                        ERR_LOGGER.warning(getClass(), "  ?");
                                        ERR_LOGGER.warning(getClass(),
                                                SHEFErrorCodes.LOG_046);
                                        buffer.append("\n");
                                        buffer.append(bRecBuffer);
                                        buffer.append("\n.END");
                                        records.add(buffer.toString());
                                        buffer = null;
                                        bRecBuffer = null;
                                        assemble = null;
                                        continued = Continuation.NONE;
                                        bData = false;
                                    }
                                    assemble = m.group(2);
                                    // need to check for revision here for the
                                    // report level
                                    mRevised = (m.group(5) != null) ? "R" : " ";
                                    if (!leadingSpaces) {
                                        if (buffer != null) {
                                            records.add(buffer.toString());
                                        }
                                        buffer = new StringBuilder(currRec);
                                    } else {
                                        ERR_LOGGER.warning(getClass(), String
                                                .format(LINE_FMT, traceId,
                                                        lineNumber));
                                        ERR_LOGGER.warning(getClass(), currRec);
                                        ERR_LOGGER.warning(getClass(), "  ?");
                                        ERR_LOGGER.warning(getClass(),
                                                SHEFErrorCodes.LOG_006);
                                        assemble = null;
                                    }
                                } else {
                                    // continuation error
                                    ERR_LOGGER.warning(getClass(), String
                                            .format(LINE_FMT, traceId,
                                                    lineNumber));
                                    ERR_LOGGER.warning(getClass(), currRec);
                                    ERR_LOGGER.warning(getClass(), "  ?");
                                    ERR_LOGGER.warning(getClass(),
                                            SHEFErrorCodes.LOG_008);
                                    if (buffer != null) {
                                        records.add(buffer.toString());
                                    }
                                    buffer = null;
                                    bRecBuffer = null;
                                    assemble = null;
                                    continued = Continuation.NONE;
                                }
                            } else {
                                if ((currRec != null) && (currRec.length() > 0)) {
                                    if ("B".equals(assemble)) {
                                        // We are assembling a B Record
                                        if (currRec != null) {
                                            if (bRecBuffer == null) {
                                                bRecBuffer = new StringBuilder();
                                            }
                                            bRecBuffer.append("\n");
                                            bRecBuffer.append(currRec);
                                            bData = true;
                                        }
                                    } else {
                                        if ((currRec.length() >= 1)
                                                && (currRec.charAt(0) != ':')) {
                                            // Non-comment data embedded in 'A'
                                            // or
                                            // 'E' record.
                                            if ("A".equals(assemble)
                                                    || "E".equals(assemble)) {
                                                if (buffer != null) {
                                                    records.add(buffer
                                                            .toString());
                                                }
                                            }
                                            buffer = null;
                                            bRecBuffer = null;
                                            assemble = null;
                                            continued = Continuation.NONE;
                                        }
                                    }
                                }
                            }
                        }
                    } else {

                    }
                    if (endOfReport) {
                        // close out anything that may have been in progress.
                        if (buffer != null) {
                            if ("B".equals(assemble)) {
                                buffer.append("\n");
                                buffer.append(bRecBuffer);
                            }
                            records.add(buffer.toString());
                        }
                        buffer = null;
                        bRecBuffer = null;
                        assemble = null;
                        continued = Continuation.NONE;
                        endOfReport = false;
                    }
                }
            } // while
            if (buffer != null) {
                records.add(buffer.toString());
            }
        } catch (Exception e) {
            if (log.isDebugEnabled()) {
                ERR_LOGGER.error(getClass(), "Data error ", e);
            } else {
                ERR_LOGGER.error(getClass(), "Data error ");
            }
        }
        if (log.isDebugEnabled()) {
            ERR_LOGGER.debug(getClass(), "Message has " + records.size()
                    + " records.");
        }
    }

    /**
     * Apply rules for internal comments. (NWSM 10-944, sec 5.3.2)
     * 
     * @param buffer
     * @return
     */
    private static String removeInternalComments(String dataLine) {
        String s = null;
        if (dataLine != null) {
                StringBuilder buffer = new StringBuilder(dataLine.length());
                boolean inComment = false;
                for (int i = 0; i < dataLine.length(); i++) {
                    if (dataLine.charAt(i) != ':') {
                        if (!inComment) {
                            buffer.append(dataLine.charAt(i));
                        }
                    } else {
                        // Toggle comments
                        inComment = !inComment;
                    }
                }
                s = buffer.toString();           
        } else {
            s = new String();
        }
        return s;
    }

    /**
     * Apply rules for internal comments. (NWSM 10-944, sec 5.3.2)
     * 
     * @param buffer
     * @return
     */
    private static String removeInternalComments(StringBuilder buffer) {
        String s = (buffer != null) ? buffer.toString() : null;
        return removeInternalComments(s);
    }

    /**
     * Apply rules for continuation lines (NWSM 10-944, sec 5.3.3)
     * 
     * @param buffer
     * @param data
     * @return
     */
    private static StringBuilder assembleContinuedLines(StringBuilder buffer,
            String data) {
        data = removeInternalComments(data);
        boolean leadingSlash = findLeadingSlash(data);
        boolean trailingSlash = findTrailingSlash(buffer.toString());
        if (!trailingSlash && !leadingSlash) {
            // rule 5.3.3r1
            // buffer does not end with a "/" and data does not start with a "/"
            buffer.append("/").append(data);
        } else if (trailingSlash && !leadingSlash) {
            // rule 5.3.3r2.0
            // buffer ends with a "/" and data does not start with a "/"
            buffer.append(data);
        } else if (!trailingSlash && leadingSlash) {
            // rule 5.3.3r2.1
            // buffer does not end with a "/" and data starts with a "/"
            int i = data.indexOf("/");
            if (i > 0) {
                buffer.append(data.substring(i));
            } else {
                buffer.append(data);
            }
        } else if (trailingSlash && leadingSlash) {
            // rule 5.3.3r3
            // buffer ends with a "/" and data starts with a "/"
            int i = data.indexOf("/");
            if (i > 0) {
                buffer.append(data.substring(i));
            } else {
                buffer.append(data);
            }
        }
        return buffer;
    }

    /**
     * 
     * @param data
     * @return Does the data contain a leading slash "/" character?
     */
    private static boolean findLeadingSlash(String data) {
        boolean leadingSlash = false;
        if ((data != null) && (data.length() > 0)) {
            int i = 0;
            for (; i < data.length(); i++) {
                if (' ' != data.charAt(i)) {
                    break;
                }
            }
            leadingSlash = (data.charAt(i) == '/');
        }
        return leadingSlash;
    }

    /**
     * 
     * @param data
     * @return Does the data contain a trailing slash "/" character?
     */
    private static boolean findTrailingSlash(String data) {
        boolean trailingSlash = false;
        if ((data != null) && (data.length() > 0)) {
            trailingSlash = (data.charAt(data.length() - 1) == '/');
        }
        return trailingSlash;
    }

    /**
     * @return the removeLeadingComments
     */
    public boolean isRemoveLeadingComments() {
        return removeLeadingComments;
    }

    /**
     * @param removeLeadingComments
     *            the removeLeadingComments to set
     */
    public void setRemoveLeadingComments(boolean leadingComments) {
        removeLeadingComments = leadingComments;
    }

    /**
     * Returns the WMOHEader object.
     * 
     * @return - WMOHeader
     */
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Returns the SWIPS Header String.
     * 
     * @return - The AWIPS Header
     */
    public String getAwipsHeader() {
        return awipsHeader;
    }

    /**
     * Returns the message in its original format.
     * 
     * @return - The raw message
     */
    public String getRawMessage() {
        return this.rawData;
    }

    /**
     * Returns the product time, time the message was received.
     * 
     * @return - Date object representing when the message was received
     */
    public Date getProductTime() {
        return this.productTime;
    }

    /**
     * @return the inTest
     */
    private boolean isInTest() {
        return inTest;
    }

    /**
     * @param inTest
     *            the inTest to set
     */
    private void setInTest(boolean inTest) {
        this.inTest = inTest;
        if (isInTest()) {
            ERR_LOGGER.setOutMode(SHEFErrors.STDOUT);
        } else {
            ERR_LOGGER.setOutMode(SHEFErrors.LOGOUT);
        }
    }

    private static void test1() {
        String[] data = { ".A X", ".AR X", ".A1 X", ".A0 X", ".A9 X", ".AR0 X",
                ".AA0 X", ".B82 X", ".A1", ".A12", ".B82", ".B X", ".BR X",
                ".B1 X", ".B0 X", ".B9 X", ".BR0 X", ".BB X", ".B99 X", ".E X",
                ".ER X", ".E1 X", ".E0 X", ".E9 X", ".ER0 X", ".E1R X",
                ".E00 X", ".END", "ICL  : CLARINDA AIRPORT   :", };

        // Pattern p =
        // Pattern.compile("^.([ABE])(((R)([0-9])?)|([0-9]))? (.*)");
        Pattern p = Pattern.compile(SHEFTYPE);
        for (String s : data) {
            Matcher m = p.matcher(s);
            if (m.find()) {
                // System.out.println(s);
                for (int i = 0; i <= m.groupCount(); i++) {
                    System.out
                            .println(String.format("%2d %10s", i, m.group(i)));
                }
                System.out.println("----------------------------");
            }
            System.out.println("************************************");
        }

        System.out
                .println("* Trailing Slash ***********************************");
        System.out.println(findTrailingSlash("THIS IS A /TEST/") == true);
        System.out.println(findTrailingSlash("THIS IS A /TEST/ ") == false);
        System.out.println(findTrailingSlash("THIS IS A /TEST ") == false);

        System.out
                .println("* Leading Slash  ***********************************");
        System.out.println(findLeadingSlash(" /THIS IS A /TEST/") == true);
        System.out.println(findLeadingSlash("/THIS IS A /TEST/ ") == true);
        System.out.println(findLeadingSlash("   /THIS IS A /TEST ") == true);
        System.out.println(findLeadingSlash("THIS IS A /TEST/") == false);

        System.out
                .println("* Assemble *****************************************");
        String t = assembleContinuedLines(new StringBuilder("TEST1/"),
                "  TEST2").toString();
        System.out.println("TEST1/  TEST2".equals(t));

        t = assembleContinuedLines(new StringBuilder("TEST1"), "  /TEST2")
                .toString();
        System.out.println("TEST1/TEST2".equals(t));

        t = assembleContinuedLines(new StringBuilder("TEST1"), "  TEST2")
                .toString();
        System.out.println("TEST1/  TEST2".equals(t));

        t = assembleContinuedLines(new StringBuilder("TEST1/"), "  /TEST2")
                .toString();
        System.out.println("TEST1//TEST2".equals(t));

        System.out
                .println("* Comments processing ******************************");
        t = removeInternalComments(new StringBuilder(
                ":THIS IS AN INTERNAL COMMENT"));
        System.out.println("".equals(t));

        t = removeInternalComments(new StringBuilder(
                "THIS IS :AN INTERNAL: COMMENT"));
        System.out.println("THIS IS  COMMENT".equals(t));

        t = removeInternalComments(new StringBuilder("/TA 345.5:COMMENT"));
        System.out.println("/TA 345.5".equals(t));
    }

    private static void test2() {
        String data = "0x01\r\r\n615\r\r\nSRUS53 KGID 271240\r\r\nRR3GID\r\r\nWxCoder"
                + "\r\r\n.A SRRN1 100127 C DH0700/TX 38/TN 12/TA 21/PP 0.00/SF 0.0"
                + "\r\r\n.A1 SD 0/DC1001270634" + "\r\r\n\r\r\n" + "\r\r\n0x03";

        String[] msgData = {
                // 0
                "0x01\r\r\n261\r\r\nASUS63 KDMX 111600\r\r\nRTPIA"
                        + "\r\r\nMAX/MIN TEMPERATURE AND PRECIPITATION TABLE FOR IOWA"
                        + "\r\r\nNATIONAL WEATHER SERVICE DES MOINES IA"
                        + "\r\r\n10:00 AM CST WED NOV 11 2009"
                        + "\r\r\n.B DMX 1111 C DH00/DC0911110959/TAIRZX/DH06/TAIRZP/"
                        + "\r\r\n.B1 PPDRZZ/SFDRZZ/SDIRZZ"
                        + "\r\r\n:"
                        + "\r\r\n: VALUES REPRESENT HIGHS YESTERDAY...12-HOUR LOWS..."
                        + "\r\r\n: AND 24-HOUR PRECIPITATION ENDING AT  6 AM CENTRAL TIME"
                        + "\r\r\n:"
                        + "\r\r\n:                           MAX   MIN                  SNOW"
                        + "\r\r\n:      LOCATION            TEMP  TEMP    PCPN   SNOW  DEPTH"
                        + "\r\r\n:"
                        + "\r\r\nBB00200  : CLARINDA AIRPORT   :  63 /DD11/DH04/  30 /  0.00\"SEE IF WE FIND THIS\"/    M /   M"
                        + "\r\r\nBB00210  : COUNCIL BLUFFS AIRP:   M /   M / DH06 /  0.00 /    M /   M"
                        + "\r\r\nBB00220  : HARLAN AIRPORT     :  61 /  28 /  0.00 /    M /   M"
                        + "\r\r\nBB00230 61/28/0.00/M/M,BB00231 61/28/0.00/M/M"
                        + "\r\r\n:* COOPERATIVE WEATHER OBSERVATION SITE"
                        + "\r\r\n:" + "\r\r\n.END\r\r\n$$\r\r\n0x03",
                // 1
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n:This is testing a DOT E product for stage"
                        + "\r\r\n.E RUSS1 20090930 C DH1315/HGARG/DIH1/10.0/11.0/12.0/13.0"
                        + "\r\r\n.E RUSS2 20091202 E DH1510/PPD/DRH2/DID7/10.0/11.0/12.0/13.0\"RETAINED COMMENT\""
                        + "\r\r\n.E RUSS3 20091202 E DH1510/PP\'WRONG PLACE\'D/DIN15/10.0/11.0/12.0/13.0"
                        + "\r\r\n.E RUSS4 20080229 E DH1510/PPD/DIY1/10.0/11.0/12.0/13.0\"END OF LINE NO TRAILING QUOTE"
                        + "\r\r\n.E RUSS5 20090131 E DH1510/PPD/DIE1/10.0/11.0/12.0/13.0\"END OF LINE NO TRAILING QUOTE"
                        + "\r\r\n:End of Report" + "\r\r\n0x03",
                // 2
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n:This is testing a DOT E product for stage"
                        + "\r\r\n.A AA0280D 18820207 Z DH12/DY81/PC 4.3/DC8102051030/PC 4.1"
                        + "\r\r\n.E RUSS1 20090930 DH1315/DUE/PPDRZZ/DQR/DC1001131215/DIN15/10.0//11.0F/12.0\"SUSPECT\"/-13.0"
                        + "\r\r\n.E1/14.0/m/16.0/17.0" + "\r\r\n:End of Report"
                        + "\r\r\n0x03",
                // 3
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n:This is testing a DOT E product for stage"
                        + "\r\r\n.E EE0037 090426 C DH01/HG/DID1/2.2/3.3/4.4/DRH+3/5.5/6.6/DRD+1/"
                        + "\r\r\n.E1 7.7/8.8" + "\r\r\n:End of Report"
                        + "\r\r\n0x03",
                // 4
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n.B DMX 1111 C DH00/DC0911110959/TAIRZX/DH06/TAIRZP/"
                        + "\r\r\n.B1 PPDRZZ/SFDRZZ/SDIRZZ"
                        + "\r\r\n:"
                        + "\r\r\n: VALUES REPRESENT HIGHS YESTERDAY...12-HOUR LOWS..."
                        + "\r\r\n: AND 24-HOUR PRECIPITATION ENDING AT  6 AM CENTRAL TIME"
                        + "\r\r\n:"
                        + "\r\r\n:                           MAX   MIN                  SNOW"
                        + "\r\r\n:      LOCATION            TEMP  TEMP    PCPN   SNOW  DEPTH"
                        + "\r\r\n:"
                        + "\r\r\nICL  : CLARINDA AIRPORT   :  63 /DD11/DH04/  30 /  0.00 /    M /   M"
                        + "\r\r\nCBF  : COUNCIL BLUFFS AIRP:   M /   M / DH06 /  0.00 /    M /   M"
                        + "\r\r\nHNR  : HARLAN AIRPORT     :  61 /  28 /  0.00 /    M /   M"
                        + "\r\r\n:* COOPERATIVE WEATHER OBSERVATION SITE"
                        + "\r\r\n:" + "\r\r\n.END\r\r\n$$\r\r\n" + "\r\r\n0x03",
                // 5
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n:This is testing a DOT E product for stage"
                        + "\r\r\n.E RUSS1 20090930 C DH1315/HGIRG/DIN15/10.0/11.0/12.0/13.0"
                        + "\r\r\n.E1/14.0/15.0/16.0/17.0"
                        + "\r\r\n:End of Report"
                        + "\r\r\n.B DMX 1111 C DH00/DC0911110959/TAIRZX/DH06/TAIRZP/"
                        + "\r\r\n.B1 PPDRZZ/SFDRZZ/SDIRZZ"
                        + "\r\r\n:"
                        + "\r\r\n: VALUES REPRESENT HIGHS YESTERDAY...12-HOUR LOWS..."
                        + "\r\r\n: AND 24-HOUR PRECIPITATION ENDING AT  6 AM CENTRAL TIME"
                        + "\r\r\n:"
                        + "\r\r\n:                           MAX   MIN                  SNOW"
                        + "\r\r\n:      LOCATION            TEMP  TEMP    PCPN   SNOW  DEPTH"
                        + "\r\r\n:"
                        + "\r\r\nICL  : CLARINDA AIRPORT   :  63 /DD11/DH04/  30 /  0.00 /    M /   M"
                        + "\r\r\nCBF  : COUNCIL BLUFFS AIRP:   M /   M / DH06 /  0.00 /    M /   M"
                        + "\r\r\nHNR  : HARLAN AIRPORT     :  61 /  28 /  0.00 /    M /   M"
                        + "\r\r\n:* COOPERATIVE WEATHER OBSERVATION SITE"
                        + "\r\r\n:"
                        + "\r\r\n.END\r\r\n$$\r\r\n"
                        + "\r\r\n.E RUSS2 20090930 C DH1315/HGIRG/DIN15/10.0/11.0/12.0/13.0"
                        + "\r\r\n0x03",
                // 6
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n:"
                        + "\r\r\n.B DMX 1111 C DH00/DC0911110959/TAIRZX/DH06/TAIRZP/"
                        + "\r\r\n.B1 PPDRZZ/SFDRZZ/SDIRZZ"
                        + "\r\r\nICL  63 /DD11/DH04/  30/                0.00/   10/  21"
                        + "\r\r\nCBF   M /             M/ DH06/          0.00/   11/   M"
                        + "\r\r\nHNR  61 /            28/                0.00/    M/  23"
                        + "\r\r\nHNR  61 /            28/DC0911111259/   0.00/   13/  24"
                        + "\r\r\n.END\r\r\n$$\r\r\n" + "\r\r\n0x03",
                // 7
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n.A  GPET2 1210 E DH0625/NG 0/QS 0\"TEST TEST\"/NG 1"
                        + "\r\r\n.A  GPET2 0305 M    DH1500/NG 0/QS 0\"TEST TEST\""
                        + "\r\r\n.A1 DH1625/NG 1/DUS/QS 0.6/DUE/DH1830/NG 1/QS 1.2"
                        + "\r\r\n.A2 DH1910/NG 2/QS 2.4//DH2005/NG 4/QS 4.8"
                        + "\r\r\n.A3 DH2215/NG 8/QS 9.6/DH2314/NG 10/QS 12.0"
                        + "\r\r\n.A4 DM0306/DH0214/NG 6/QS 7.2/DH0430/NG 3/QS 3.6"
                        + "\r\r\n.A5 DH0600/NG 1/QS 1.2/DH0745/NG 0/QS 0"
                        + "\r\r\n:End of Report" + "\r\r\n0x03",
                // 8
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n.A  GPET2 1210 E DH0625/NG M/QS 0\"TEST TEST\"/NG 1"
                        + "\r\r\n:End of Report" + "\r\r\n0x03",
                // 9
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n.A  AA00001 1210 E DH06/NG M/QS 0/NG 11"
                        + "\r\r\n.A  AA00002 1210 E DH0625/NG m/QS 1/NG 123"
                        + "\r\r\n.A  AA00003 1210 E DH062530/NG -/QS 2/NG 1"
                        + "\r\r\n.A  AA00004 1210 E DH06/DN26/NG +/QS 3/NG 14"
                        + "\r\r\n.A  AA00005 1210 E DH06/DN26/DS31/NG MM/DRH1/QS 4/DRH-1/NG 15"
                        + "\r\r\n.A  AA00006 1210 E DH06/DN26/DS31/NG MM/DH01/QS 4/DRH-1/NG 15"
                        + "\r\r\n:End of Report" + "\r\r\n0x03",
                // 10
                "0x01\r\r\n708\r\r\nSXUS40 KWOH 211446\r\r\nRR3OAX\r\r\nWxCoder"
                        + "\r\r\n.A AA05301 100121 C DH0700/PP 0.03/PT 3/SF 0.0/SD 11/XW 10"
                        + "\r\r\n.A1 DC1001210842"
                        + "\r\r\n.A AA05302 100118 C DH0800/TX 33/TN 16/TA M/PP 0.00/SF 0.0"
                        + "\r\r\n.A1 DC1001210851"
                        + "\r\r\n.A AA05303 0121 C DH07/PP 0.00/SD 0/AD 04/"
                        + "\r\r\n.A AA05304 0121 C DH07/TX 32/TN 31/TA 31/PP 0.20/SD 10/AD 04/"
                        + "\r\r\n:End of Report"
                        + "\r\r\n.A AA05305 100118 C DH0800/TX 33/TN 16/TA M/PP 0.00/SF 0.0"
                        + "\r\r\n: WRONG CONTINUATION LINE"
                        + "\r\r\n.E1 DC1001210851"
                        + "\r\r\n: STARTS IN WRONG COLUMN"
                        + "\r\r\n .A AA05306 100118 C DH0800/TX 33/TN 16/TA M/PP 0.00/SF 0.0"
                        + "\r\r\n.A AA05307 100118 C DH0800/TX 33/TN 16/TA M/PP 0.00/SF 0.0"
                        + "\r\r\n0x03",
                // 11
                "0x01\r\r\n261\r\r\nSXUS44 TEST 301404\r\r\nRTPIA"
                        + "\r\r\n.A  GPET2 1210 E DH0625/DC200912181711/DC0912181712/DC12181713/DC121817"
                        + "\r\r\n.A1 DC1218" + "\r\r\n:End of Report"
                        + "\r\r\n0x03",
                // 12
                "0x01\r\r\n708\r\r\nSXUS40 KWOH 211446\r\r\nRR3OAX\r\r\nWxCoder"
                        + "\r\r\n.A AA06101 070228 DH12/HG 2.2/DRE+1/HG 3.3/DRE+2/HG 4.4/DRE+3/HG 5.5/"
                        + "\r\r\n.A1 DRE+4/HG 6.6/DRE+5/HG 7.7/DRE+6/HG 8.8/DRE+7/HG 9.9/DRE+8/HG 1.1"
                        + "\r\r\n.A2 DRE+9/HG 2.2/DRE+10/HG 3.3/DRE+11/HG 4.4/DRE+12/HG 5.5"
                        + "\r\r\n:End of Report" + "\r\r\n0x03" };

        String failMsg = "0x01\r\r\n467\r\r\nSXUS40 KWOH 091836\r\r\nRRSDMX"
                + "\r\r\n:&&HADS SOR REPORT FOR USER DMX"
                + "\r\r\n.E OOAI4 20100209 DH1745/HGIRG/DIN15/13.20/13.23/13.24/13.27"
                + "\r\r\n.E OOAI4 20100209 DH1745/PCIRG/DIN15/5.09/5.09/5.09/5.09"
                + "\r\r\n:END OF REPORT" + "\r\r\n\r\r\n0x03";

        ShefSeparator sep = new ShefSeparator();
        sep.setInTest(true);
        Headers headers = new Headers();
        headers.put("ingestFileName", "DOSENTMATTER.20110103");
        sep.setData(failMsg.getBytes(), headers);
        while (sep.hasNext()) {
            ShefDecoderInput inp = sep.next();

            System.out.println(inp.record);
            System.out.println(inp.wmoHeader);
            System.out.println(inp.awipsHeader);
            System.out.println(inp.productDate);
            System.out.println("----------------------");
        }
        System.out.println("*********************************************");
    }

    private static void test3() {

        String[] data1 = { ".A X", ".AR X", ".A1  X", ".A221 X", "  .A9 X",
                ".AR0 X", ".AA0 X", ".B82 X", ".B X", ".BR X", ".B1  X",
                ".B0 X", ".B9 X", ".BR0 X", ".BB X", ".B99 X", ".E X", ".ER X",
                ".E1  X", ".E0 X", ".E9 X", ".ER0 X", ".E1R X", ".E00 X",
                ".END", "ICL  : CLARINDA AIRPORT   :", ": LINE COMMENT", };

        String[] data2 = { ".B X", ".END   ", ".B X",
                ".END  : DONE WITH THIS DATA", ".B X", "  .END", ".A X",
                "  .END", };

        String[] data3 = { "\r\r\n001\r\r\nSRXX99 KWOH 021645"
                + "\r\r\n.A SUX : SYNOPTIC : 20100902 Z DH1152/DC201009021152/TAIRZZZ  63"
                + "\r\r\n.A SUX : SYNOPTIC : 20100902 Z DH1152/DC201009021152/TDIRZZZ  61"
                + "\r\r\n.A SUX : SYNOPTIC : 20100902 Z DH1152/DC201009021152/USIRZZZ   3"
                + "\r\r\n.A SUX : SYNOPTIC : 20100902 Z DH1152/DC201009021152/UDIRZZZ  24\r\r\n", };

        String fmt = "%-12s %1s %1s %1s %-4s %-1s";

        Matcher m = null;
        for (String s : data3) {
            ShefSeparator sep = new ShefSeparator();
            sep.setInTest(true);
            Headers headers = new Headers();
            headers.put("ingestFileName", "DOSENTMATTER.20110103");
            sep.setData(s.getBytes(), headers);
            while (sep.hasNext()) {
                ShefDecoderInput inp = sep.next();

                System.out.println(inp.wmoHeader);
                System.out.println(inp.awipsHeader);
                System.out.println(inp.productDate);
                System.out.println("----------------------");
                System.out.println(inp.record);
            }
            System.out.println("*********************************************");
        }
    }

    private static void test4() {

        ShefSeparator sep = new ShefSeparator();
        sep.setInTest(true);

        FileInputStream fis = null;
        PrintStream sv = null;
        try {
            File fout = new File("./test_data/test.log");
            PrintStream ps = new PrintStream(fout);
            sv = System.out;
            System.setOut(ps);

            File f = new File("./test_data/SHEFencoded_input");
            fis = new FileInputStream(f);
            byte[] data = new byte[(int) f.length()];
            fis.read(data);
            Headers headers = new Headers();
            headers.put("ingestFileName", "DOSENTMATTER.20110103");
            sep.setData(data, headers);
            while (sep.hasNext()) {
                ShefDecoderInput inp = sep.next();

                System.out.println(inp.wmoHeader);
                System.out.println(inp.awipsHeader);
                System.out.println(inp.productDate);
                System.out.println(inp.record);
                System.out.println("----------------------");
            }
            System.out.println("*********************************************");
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            System.setOut(sv);
        }
    }

    /**
     * 
     */
    private static void test5() {
        String testData1 = "ASUS63 KARX 271430\n.BR LSE 0127 C DH00/TAIRZX/DH06/TAIRZP/PPDRZZ/SFDRZZ/SDIRZZ\nTOB  : DODGE CENTER AWOS     MN :   25 /  19 /   M /     /    \n.END";
        String testData2 = "ASUS63 KARX 271430\n.BR LSE 0127 C DH07/TAIRZX/TAIRZN/PPDRZZ/SFDRZZ/SDIRZZ\nASTM5: AUSTIN                MN :DH0700/  20 /  12 /   T / 0.1 /  13\n.END";

        Headers headers = new Headers();
        headers.put("ingestFileName", "DOSENTMATTER.20110103");

        ShefSeparator sep = new ShefSeparator();
        sep.setInTest(true);
        sep.setData(testData1.getBytes(), headers);
        while (sep.hasNext()) {
            ShefDecoderInput inp = sep.next();
            System.out.println(inp.record);
        }
    }

    private static void test6() {
        String testData1 = "ASUS63 KARX 271430\n"
                + ".B:REVISION:R MCI:KANSAS CITY: 08:AUG:10:TENTH: DH12:1200Z:/HG/PPP\n"
                + "BB02:KANSAS CITY:23:MISSOURI: 4.8/1:ONE:.06:POINT 06\n"
                + ".E:EEE:N:NNN:D:DDD\n";

        ShefSeparator sep = new ShefSeparator();
        sep.setInTest(true);
        Headers headers = new Headers();
        headers.put("ingestFileName", "DOSENTMATTER.20110103");
        sep.setData(testData1.getBytes(), headers);
        while (sep.hasNext()) {
            ShefDecoderInput inp = sep.next();
            System.out.println(inp.record);
        }
    }

    private static void test7() {
        String testData1 = "ASUS63 KARX 271430\n" + ".A AA0259 960714\n"
                + ".A1  PCIRG 76.24\n";

        ShefSeparator sep = new ShefSeparator();
        sep.setInTest(true);
        Headers headers = new Headers();
        headers.put("ingestFileName", "DOSENTMATTER.20110103");
        sep.setData(testData1.getBytes(), headers);
        while (sep.hasNext()) {
            ShefDecoderInput inp = sep.next();
            System.out.println(inp.record);
        }
    }

    private static void test8() {

        String testData1 = "ASUS63 KARX 271430\n"
                + ".A AA0145 821007 DH12/HG 5.5/\n" + ".AR1 PP 9.99\n";

        ShefSeparator sep = new ShefSeparator();
        sep.setInTest(true);
        Headers headers = new Headers();
        headers.put("ingestFileName", "DOSENTMATTER.20110315");
        sep.setData(testData1.getBytes(), headers);
        while (sep.hasNext()) {
            ShefDecoderInput inp = sep.next();
            System.out.println(inp.record);
        }
    }

    private static void test9() {

        String[] testData = { ".B AA0145 821007 DH12/HG 5.5/\n",
                ".BR10 AA0145 821007 DH12/HG 5.5/\n",
                ".BR2 AA0145 821007 DH12/HG 5.5/\n",
                ".B1 AA0145 821007 DH12/HG 5.5/\n", };

        for (String s : testData) {
            Matcher m = P_SHEFTYPE.matcher(s);
            if (m.find()) {
                for (int i = 0; i <= m.groupCount(); i++) {
                    System.out.println(String.format("%3d  %s", i, m.group(i)));
                }
            }
            System.out.println("--------------------------------------");
        }

        // ShefSeparator sep = new ShefSeparator();
        // sep.setInTest(true);
        // Headers headers = new Headers();
        // headers.put("ingestFileName", "DOSENTMATTER.20110315");
        // sep.setData(testData1.getBytes(), headers);
        // while (sep.hasNext()) {
        // ShefDecoderInput inp = sep.next();
        // System.out.println(inp.record);
        // }
    }

    /**
     * Test Function
     * 
     * @param args
     */
    public static void main(String[] args) {
        // test1();
        // test9();

        String test = "THIS IS A TEST :=======: AND MORE DATA :================: AND THE END";

        System.out.println("[" + removeInternalComments(test) + "]");

        test = ":THIS IS A TEST :=======: AND MORE DATA :================: AND THE END";

        System.out.println("[" + removeInternalComments(test) + "]");

        // Matcher m = P_SHEFTYPE.matcher(".A1 DH1430/PCIRG\t76.24==\n");
        // if (m.find()) {
        // for (int i = 0; i <= m.groupCount(); i++) {
        // System.out.println(String.format("%4d   %s", i, m.group(i)));
        // }
        // }
    }
}
