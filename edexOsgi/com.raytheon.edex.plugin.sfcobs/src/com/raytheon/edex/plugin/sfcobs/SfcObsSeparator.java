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
package com.raytheon.edex.plugin.sfcobs;

import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.ETX;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.SOM;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.WMO_HEADER;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.plugin.sfcobs.common.SfcObsPart;
import com.raytheon.edex.plugin.sfcobs.common.SfcObsSubMessage;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * The SfcObsSeparator takes a potential weather message and attempts to
 * determine the WMO header and data type of the enclosed data. Normal usage is
 * to create an instance and set the message data using the setData method. When
 * complete the separator contains the WMO header, the message data with all
 * leading data up to and including the WMO header removed. In addition all
 * extraneous spaces and carriage control has been removed. The message reports
 * are available using hasNext to determine if data is available, and the
 * getRecord method to retrieve the actual report data. Note that this separator
 * implementation should not be used on mixed text/binary weather messages.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SfcObsSeparator extends AbstractRecordSeparator {
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private static final String SPLITCHARS = " \r=;$" + SOM + ETX;

    // private static final String GGYYiW_GRP =
    // "((0[1-9])|([1-2]\\d)|3[0-1])(([0-1]\\d)|2[0-3])[1-4]";

    private static final String EMAIL_FAILS = "=(20|3D)[^\\r\\n]*[\\r\\n]+";

    // private Pattern GGYYiW = Pattern.compile(GGYYiW_GRP);

    private WMOHeader wmoHeader = null;

    private byte[] messageData = null;

    private List<String> reports = null;

    private int currentReport = -1;

    // Once found the report type must not change within the message. The
    // YYGGIw may change for AAXX and CMAN however.
    private SfcObsPart messageType = null;

    private List<SfcObsSubMessage> subMessages = null;

    public static SfcObsSeparator separate(byte[] data, Headers headers) {
        SfcObsSeparator separator = new SfcObsSeparator();
        separator.setData(data, headers);
        return separator;
    }

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     */
    @Override
    public SfcObsDecoder.SfcObsDecoderInput next() {
        SfcObsDecoder.SfcObsDecoderInput data = null;
        if (hasNext()) {
            data = new SfcObsDecoder.SfcObsDecoderInput();
            data.report = reports.get(currentReport++);
            data.wmoHeader = wmoHeader;
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
        reports = null;
        rawMessage = DecoderTools.cleanData(rawMessage);
        if (rawMessage != null) {
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            wmoHeader = new WMOHeader(rawMessage, fileName);
            if (wmoHeader.isValid()) {
                messageData = DecoderTools.stripWMOHeader(rawMessage,
                        WMO_HEADER);
            }
        }
        cleanGarbage();

        subMessages = new ArrayList<SfcObsSubMessage>();
        parseParts();

        for (SfcObsSubMessage msg : subMessages) {
            List<String> list = msg.getReports();
            if (reports == null) {
                reports = list;
            } else {
                reports.addAll(list);
            }
        }
        // Done with the subMessages, so release them.
        subMessages = null;

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        }
    }

    /**
     * Get the message data.
     * 
     * @return The cleaned message data.
     */
    public byte[] getMessage() {
        return messageData;
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
     * Create a list of all subMessage parts within the message.
     */
    private void parseParts() {
        SfcObsSubMessage subMessage = new SfcObsSubMessage();
        subMessages.add(subMessage);
        List<SfcObsPart> parts = subMessage.getParts();

        if (messageData != null) {
            StringTokenizer st = new StringTokenizer(new String(messageData),
                    SPLITCHARS, true);
            while (st.hasMoreTokens()) {
                String s = st.nextToken();
                if (" ".equals(s)) {
                } else if ("\r".equals(s)) {
                    parts.add(SfcObsPart.CR_PART);
                } else if ("=".equals(s)) {
                    parts.add(SfcObsPart.RE_PART);
                } else if (";".equals(s)) {
                    parts.add(SfcObsPart.RE_PART);
                } else if (SOM.equals(s)) {
                    parts.add(SfcObsPart.MS_PART);
                } else if (ETX.equals(s)) {
                    parts.add(SfcObsPart.ME_PART);
                } else {
                    SfcObsPart reportPart = SfcObsPart.partFactory(s);
                    if (reportPart == null) {
                        parts.add(new SfcObsPart(s, false));
                    } else {
                        if (messageType == null) {
                            messageType = reportPart;
                            if (reportPart.isStartNew()) {
                                // need to create a new subMessage
                                subMessage = new SfcObsSubMessage();
                                subMessages.add(subMessage);
                                parts = subMessage.getParts();
                            }
                        } else {
                            if (!checkParts(messageType, reportPart)) {
                                String message = "Report type has changed in message ["
                                        + messageType + "|" + reportPart + "]";
                                logger.info(message);
                            }
                        }

                        if (subMessage.getMessagePart() != null) {
                            // need to create a new subMessage
                            subMessage = new SfcObsSubMessage();
                            subMessages.add(subMessage);
                            parts = subMessage.getParts();
                        }
                        subMessage.setMessagePart(reportPart);
                        // Special case for AAXX and CMAN
                        if (SfcObsPart.AAXX_PART.equals(reportPart)
                                || SfcObsPart.CMAN_PART.equals(reportPart)) {
                            while (st.hasMoreTokens()) {
                                s = st.nextToken();
                                if (!" ".equals(s)) {
                                    subMessage.setYYGGIwPart(new SfcObsPart(s,
                                            false));
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Ensures that the message type does not change when creating multiple
     * reports. METAR/SPECI data is handled as a special case.
     * 
     * @param messagePart
     *            The first message type found in the message.
     * @param reportPart
     *            A potential message type.
     * @return Are the message and report parts the same.
     */
    private boolean checkParts(SfcObsPart messagePart, SfcObsPart reportPart) {
        boolean ok = false;

        ok = messagePart.equals(reportPart);
        if (!ok) {
            ok = SfcObsPart.METAR_PART.equals(messagePart)
                    && SfcObsPart.SPECI_PART.equals(reportPart);

            if (!ok) {
                ok = SfcObsPart.METAR_PART.equals(reportPart)
                        && SfcObsPart.SPECI_PART.equals(messagePart);
            }
        }
        return ok;
    }

    /**
     * This method checks the incoming data for certain known "bad" information
     * that needs to be removed before processing.
     * 
     */
    private void cleanGarbage() {
        if (messageData != null) {
            StringBuilder msg = new StringBuilder(new String(messageData));

            Pattern p = Pattern.compile(EMAIL_FAILS);

            boolean found = true;
            while (found) {
                Matcher m = p.matcher(msg);
                found = m.find();
                if (found) {
                    msg.delete(m.start(), m.start() + 3);
                }
            } // while

            messageData = msg.toString().getBytes();
        }
    }
}
