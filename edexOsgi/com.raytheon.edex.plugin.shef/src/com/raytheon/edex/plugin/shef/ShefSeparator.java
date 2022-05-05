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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.plugin.shef.util.SHEFErrors;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFErrorCodes;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

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
 * 6/27/2013    16225       wkwock      Fixed trail with slash and space issue.
 * 04/29/2014    3088       mpduff      Use UFStatus logging
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed TimeTools usage
 * Mar 09, 2018 6881        mduff       Cleanup.
 * </pre>
 * 
 * @author bphillip
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

    private static final IUFStatusHandler log = UFStatus
            .getHandler(ShefSeparator.class);

    private static final SHEFErrors ERR_LOGGER = SHEFErrors
            .registerLogger(ShefSeparator.class);

    // Regex used for separating multi-record files
    private static final String SHEFTYPE = "^( )*\\.([ABE])(((R)(\\d{1,})?)|(\\d{1,}))?([ \t]+(.*))?";

    private static final Pattern P_SHEFTYPE = Pattern.compile(SHEFTYPE);

    private static final String SHEFEND = "^( )*\\.END( *)(:.*)?";

    private static final Pattern P_SHEFEND = Pattern.compile(SHEFEND);

    private static final String LINE_FMT = "%s:%05d";

    private boolean removeLeadingComments = false;

    /** The WMO header */
    private WMOHeader wmoHeader = null;

    private String traceId = null;

    /** The AWIPS Header */
    private String awipsHeader = null;

    private int currentRecord = -1;

    /** List of records contained in file */
    private List<String> records;

    /** Holds the original message */
    private String rawData = null;

    /** Holds the arrival time of the message */
    private Date productTime = new Date();

    /**
     * Constructor
     */
    public ShefSeparator() {
        records = new ArrayList<>();
    }

    @Override
    public void setData(byte[] data, Headers headers) {
        if ((data != null) && (data.length > 0)) {
            rawData = new String(data);
            currentRecord = -1;

            /* Extracts the header */
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            wmoHeader = new WMOHeader(data, fileName);
            byte[] messageData;
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
                messageData = DecoderTools.stripWMOHeader(data,
                        IDecoderConstants.WMO_HEADER);
            } else {
                // No WMO header found or bad one, so process as best
                // as we can.
                Calendar c = TimeUtil.newGmtCalendar();
                if (c != null) {
                    productTime = c.getTime();
                }
                messageData = data;
            }

            if (messageData != null) {
                doSeparate(new String(messageData));
            } else {
                log.error(traceId + "- Invalid WMOHeader "
                        + wmoHeader.getWmoHeader());
            }
        }

        if ((records != null) && (!records.isEmpty())) {
            currentRecord = 0;
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
            log.error(separator.traceId + "- Error separating data.", e);
        }
        return separator;
    }

    public String getTraceId() {
        return traceId;
    }

    @Override
    public boolean hasNext() {
        return ((records != null) && (!records.isEmpty())
                && (currentRecord < records.size()));
    }

    @Override
    public ShefDecoderInput next() {
        ShefDecoderInput data = null;
        if (hasNext()) {
            data = new ShefDecoderInput();
            data.record = records.get(currentRecord);
            currentRecord++;
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
                                    ERR_LOGGER.warning(getClass(),
                                            String.format(LINE_FMT, traceId,
                                                    lineNumber));
                                    ERR_LOGGER.warning(getClass(), currLine);
                                    ERR_LOGGER.warning(getClass(), "   ?");
                                    ERR_LOGGER.warning(getClass(),
                                            SHEFErrorCodes.LOG_068);
                                }
                            } else {
                                ERR_LOGGER.error(getClass(), String
                                        .format(LINE_FMT, traceId, lineNumber));
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
                                    ERR_LOGGER.warning(getClass(),
                                            String.format(LINE_FMT, traceId,
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
                                                                traceId, lineNumber));
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

                                        } else if (assemble
                                                .equals(m.group(2))) {
                                            if ("R".equals(mRevised)) {
                                                buffer = assembleContinuedLines(
                                                        buffer, lineData);
                                            } else if (" ".equals(cRevised)) {
                                                if ("R".equals(mRevised)) {
                                                    // the revision on this
                                                    // continuation line is not
                                                    // the same as the main
                                                    // report.
                                                    ERR_LOGGER.error(getClass(),
                                                            String.format(
                                                                    LINE_FMT,
                                                                    traceId,
                                                                    lineNumber));
                                                    ERR_LOGGER.error(getClass(),
                                                            currRec);
                                                    ERR_LOGGER.error(getClass(),
                                                            " ?");
                                                    ERR_LOGGER.error(getClass(),
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
                                            ERR_LOGGER.error(getClass(),
                                                    String.format(LINE_FMT,
                                                            traceId, lineNumber));
                                            ERR_LOGGER.error(getClass(),
                                                    currRec);
                                            ERR_LOGGER.error(getClass(), " ?");
                                            ERR_LOGGER.error(getClass(),
                                                    SHEFErrorCodes.LOG_009);
                                        }
                                    } else {
                                        // We have a continuation line with no
                                        // data
                                        ERR_LOGGER.warning(getClass(),
                                                String.format(LINE_FMT, traceId,
                                                        lineNumber));
                                        ERR_LOGGER.warning(getClass(), currRec);
                                        ERR_LOGGER.warning(getClass(), " ?");
                                        ERR_LOGGER.warning(getClass(),
                                                SHEFErrorCodes.LOG_067);
                                    }
                                } else if (continued
                                        .equals(Continuation.NONE)) {
                                    // Not a continuation line

                                    /*
                                     * Check to see if we were assembling a B
                                     * record. If we get here with B then we
                                     * didn't see a .END directive. complain,
                                     * insert the .END and continue as normal
                                     */
                                    if ("B".equals(assemble)) {
                                        ERR_LOGGER.warning(getClass(),
                                                String.format(LINE_FMT, traceId,
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
                                        ERR_LOGGER.warning(getClass(),
                                                String.format(LINE_FMT, traceId,
                                                        lineNumber));
                                        ERR_LOGGER.warning(getClass(), currRec);
                                        ERR_LOGGER.warning(getClass(), "  ?");
                                        ERR_LOGGER.warning(getClass(),
                                                SHEFErrorCodes.LOG_006);
                                        assemble = null;
                                    }
                                } else {
                                    // continuation error
                                    ERR_LOGGER.warning(getClass(),
                                            String.format(LINE_FMT, traceId,
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
                                if ((currRec != null)
                                        && (currRec.length() > 0)) {
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
                                                    records.add(
                                                            buffer.toString());
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
            ERR_LOGGER.error(getClass(), "Data error ", e);
        }
    }

    /**
     * Apply rules for internal comments. (NWSM 10-944, sec 5.3.2)
     * 
     * @param buffer
     * @return
     */
    private String removeInternalComments(String dataLine) {
        StringBuilder buffer = new StringBuilder();
        if (dataLine != null) {
            buffer.ensureCapacity(dataLine.length());
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
        }
        buffer.trimToSize();
        return buffer.toString();
    }

    /**
     * Apply rules for continuation lines (NWSM 10-944, sec 5.3.3)
     * 
     * @param buffer
     * @param data
     * @return
     */
    private StringBuilder assembleContinuedLines(StringBuilder buffer,
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
            int i = data.indexOf('/');
            if (i > 0) {
                buffer.append(data.substring(i));
            } else {
                buffer.append(data);
            }
        } else if (trailingSlash && leadingSlash) {
            // rule 5.3.3r3
            // buffer ends with a "/" and data starts with a "/"
            int i = data.indexOf('/');
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
    boolean findLeadingSlash(String data) {
        if ((data != null) && (data.length() > 0)) {
            String trimData = data.trim();
            return trimData.charAt(0) == '/';
        }

        return false;
    }

    /**
     * 
     * @param data
     * @return Does the data contain a trailing slash "/" character?
     */
    private boolean findTrailingSlash(String data) {
        if ((data != null) && (data.length() > 0)) {
            String trimData = data.trim();
            return trimData.charAt(trimData.length() - 1) == '/';
        }
        return false;
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
     * Returns the AWIPS Header String.
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
}
