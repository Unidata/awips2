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
package com.raytheon.edex.plugin.airep;

import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.WMO_HEADER;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.plugin.airep.AirepDecoder.AirepDecoderInput;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * The AirepSeparator takes a potential weather message and attempts to
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
 * 20080103            384 jkorman     Initial Coding.
 * 11/13/2008         1684 chammack    Camel Refactor
 * ======================================
 * AWIPS2 DR Work
 * 20120911           1011 jkorman     Properly handle trailing end of report.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class AirepSeparator extends AbstractRecordSeparator {
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private static final String SPLITCHARS = "[=;$][^\\r\\n]*[\\r\\n]+";

    private static final String AIREP_HDR = "AIREP[\\r\\n]+";

    private static final String AIREP_MSG_LINE = "(ARP|ARS)";

    private static final String EOR_E = "=";

    private static final String EOR_S = ";";

    private WMOHeader wmoHeader = null;

    private byte[] messageData = null;

    private List<String> reports = null;

    private int currentReport = -1;

    public static AirepSeparator separate(byte[] data, Headers headers) {
        AirepSeparator airepSeparator = new AirepSeparator();
        airepSeparator.setData(data, headers);
        return airepSeparator;
    }

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     */
    public AirepDecoderInput next() {
        AirepDecoderInput data = null;
        if (hasNext()) {
            data = new AirepDecoderInput();
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
            wmoHeader = new WMOHeader(rawMessage, headers);
            if (wmoHeader.isValid()) {
                messageData = DecoderTools.stripWMOHeader(rawMessage,
                        WMO_HEADER);

                separate(new String(messageData));
            }
        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
            for (int i = 0; i < reports.size(); i++) {
                String s = reports.get(i);
                if (s != null) {
                    if (s.endsWith(EOR_E) || s.endsWith(EOR_S)) {
                        reports.set(i, s.substring(0, s.length() - 1));
                    }
                }
            }
        } else {
            logger.info("No reports found in data");
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

    private void separate(String message) {

        Pattern pattern = Pattern.compile(AIREP_HDR);
        Matcher matcher = pattern.matcher(message);
        if (matcher.find()) {
            separateAIREP(message.substring(matcher.end()));
        } else {
            separateARPARS(message);
        }
    }

    /**
     * Separate airep data that comes in with AIREP header line. This data
     * usually does not contain ARP/ARS report start data.
     */
    private void separateAIREP(String message) {

        Pattern pattern = Pattern.compile(SPLITCHARS);
        Matcher matcher = pattern.matcher(message);

        reports = new ArrayList<String>();
        int start = 0;
        int stop = message.length();
        while (matcher.find()) {
            stop = matcher.start() + 1;
            reports.add("ARP " + message.substring(start, stop));
            start = matcher.end();
        }
    }

    /**
     * Separate airep data that does not have the AIREP header line. This data
     * is checked to ensure that it contains ARP/ARS report start data.
     */
    private void separateARPARS(String message) {
        ArrayList<Integer> bodyRecords = new ArrayList<Integer>();

        Pattern pattern = Pattern.compile(AIREP_MSG_LINE);
        Matcher matcher = pattern.matcher(message);

        bodyRecords = new ArrayList<Integer>();
        while (matcher.find()) {
            bodyRecords.add(matcher.start());
        }

        reports = new ArrayList<String>();
        for (int i = 0; i < bodyRecords.size(); i++) {
            String observation = null;
            if (i < bodyRecords.size() - 1) {
                observation = message.substring(bodyRecords.get(i),
                        bodyRecords.get(i + 1)).trim();
            } else {
                observation = message.substring(bodyRecords.get(i)).trim();
            }
            reports.add(observation);
        }
        bodyRecords = null;
    }

}
