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
package com.raytheon.edex.plugin.taf.decoder;

import static com.raytheon.edex.plugin.taf.common.TafConstants.AMD_IND;
import static com.raytheon.edex.plugin.taf.common.TafConstants.COR_IND;
import static com.raytheon.edex.plugin.taf.common.TafConstants.REPORT_HEADER;
import static com.raytheon.edex.plugin.taf.common.TafConstants.REPORT_HEADER30;
import static com.raytheon.edex.plugin.taf.common.TafConstants.TAF_IND;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.plugin.taf.TafDecoder;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * 
 * Separator implementation for taf plugin
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * July2006		3 &amp; 14  Phillippe   Initial Creation	
 * 29August2006	3           Phillippe   Updated regular expression
 *                                      to account for corrected 
 *                                      and amended records
 * 9/4/2008     1444        grichard    Move constants to TafConstants class.
 * May 14, 2014 2536        bclement    moved WMO Header to common
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class TafSeparator extends AbstractRecordSeparator {

    private final Log logger = LogFactory.getLog(getClass());

    private static final String TEMPO_EXC = "EMPO (\\d{4}/\\d{4})";

    private static final String BECMG_EXC = "ECMG (\\d{4}/\\d{4})";

    private static final String PROB_EXC = "OB[34]0 (\\d{4}/\\d{4})";

    public static final int STATION_ID = 4;

    public static final int ISSUE_TIME = 5;

    public static final int VALID_TIME = 6;

    private WMOHeader wmoHeader = null;

    private List<TAFParts> tafRecords;

    private int currentReport = -1;

    private String traceId;

    /**
     * Constructor.
     * 
     */
    public TafSeparator() {
    }

    public static TafSeparator separate(byte[] data, Headers headers) {
        TafSeparator ts = new TafSeparator();
        ts.setData(data, headers);
        return ts;
    }

    /**
     * 
     * @param messageData
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#setData(byte[])
     */
    public void setData(byte[] messageData, Headers headers) {
        doSeparate(messageData, headers);
        if ((tafRecords != null) && (tafRecords.size() > 0)) {
            currentReport = 0;
            logger.debug(traceId + "- Separated " + tafRecords.size()
                    + " records");
        }
    }

    /**
     * Is the another record available in this separator?
     * 
     * @return Is the another record available?
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#hasNext()
     */
    public boolean hasNext() {
        return ((tafRecords != null) && (tafRecords.size() > 0) && (currentReport < tafRecords
                .size()));
    }

    /**
     * Get the currently available record from this separator. Make the next
     * record available if it exists.
     * 
     * @return The current available record.
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#next()
     */
    public TafDecoder.TAFDecoderInput next() {
        TafDecoder.TAFDecoderInput data = null;
        if (hasNext()) {
            data = new TafDecoder.TAFDecoderInput();
            data.tafParts = tafRecords.get(currentReport++);
            data.wmoHeader = wmoHeader;
        }

        return data;
    }

    /**
     * Separate a potential TAF message into separate TAF records.
     * 
     * @param message
     */
    private void doSeparate(byte[] message, Headers headers) {

        byte[] data = DecoderTools.cleanData(message);
        if (data != null) {

            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            wmoHeader = new WMOHeader(data, fileName);
            if (wmoHeader.isValid()) {

                int dStart = wmoHeader.getMessageDataStart();

                // Default length of the select text.
                int dLen = data.length - dStart;
                for (int i = data.length - 1; i > dStart; i--) {
                    // If we find an ETX then set the length there.
                    if ((data[i] & 0xFF) == 0x03) {
                        dLen = i - dStart;
                        break;
                    }
                }
                String s = wmoHeader.getBBBIndicator();

                boolean isAmd = s.startsWith("AA");
                boolean isCor = s.startsWith("CC");

                s = new String(data, dStart, dLen);

                Pattern tafLine = Pattern.compile("\\nTAF *(AMD|COR)?\\n");
                Matcher m = tafLine.matcher(s);
                if (m.find()) {
                    int pos = 0;
                    if (!isAmd) {
                        pos = s.indexOf(AMD_IND);
                        isAmd = ((pos >= 0) && (pos <= m.end()));
                    }
                    if (!isCor) {
                        pos = s.indexOf(COR_IND);
                        isCor = ((pos >= 0) && (pos <= m.end()));
                    }
                }

                ArrayList<Integer> partPos = new ArrayList<Integer>();

                Pattern p = Pattern.compile(REPORT_HEADER);

                m = p.matcher(s);

                if (m.find()) {
                    partPos.add(m.start());
                    partPos.add(m.end());

                    int start = -1;
                    int stop = -1;
                    while (m.find()) {
                        start = m.start();
                        stop = m.end();

                        partPos.add(start);
                        partPos.add(start);
                        partPos.add(stop);

                    }
                    partPos.add(s.length());
                } else {
                    p = Pattern.compile(REPORT_HEADER30);

                    Pattern pT = Pattern.compile(TEMPO_EXC);
                    Pattern pB = Pattern.compile(BECMG_EXC);
                    Pattern pP = Pattern.compile(PROB_EXC);
                    Matcher m1 = null;

                    m = p.matcher(s);
                    if (m.find()) {
                        int start = m.start();
                        int stop = m.end();
                        partPos.add(start);
                        partPos.add(stop);

                        start = -1;
                        stop = -1;
                        while (m.find()) {
                            start = m.start();
                            stop = m.end();

                            // The TEMPO, BECMG, and PROB patterns can match the
                            // start
                            // of a taf so skip if there's a match.
                            m1 = pT.matcher(s.substring(start, stop));
                            if (m1.find()) {
                                continue;
                            }
                            m1 = pB.matcher(s.substring(start, stop));
                            if (m1.find()) {
                                continue;
                            }
                            m1 = pP.matcher(s.substring(start, stop));
                            if (m1.find()) {
                                continue;
                            }
                            partPos.add(start);
                            partPos.add(start);
                            partPos.add(stop);

                        }
                        partPos.add(s.length());
                    }
                }

                if (partPos.size() > 0) {
                    tafRecords = new ArrayList<TAFParts>();

                    for (int i = 0; i < partPos.size(); i += 3) {

                        TAFParts parts = new TAFParts();

                        StringBuilder tafHdr = new StringBuilder(s.substring(
                                partPos.get(i), partPos.get(i + 1)));

                        if ((isCor) && (tafHdr.indexOf(COR_IND) < 0)) {
                            tafHdr.insert(0, ' ');
                            tafHdr.insert(0, COR_IND);
                        }

                        if ((isAmd) && (tafHdr.indexOf(AMD_IND) < 0)) {
                            tafHdr.insert(0, ' ');
                            tafHdr.insert(0, AMD_IND);
                        }

                        if (tafHdr.indexOf(TAF_IND) < 0) {
                            tafHdr.insert(0, ' ');
                            tafHdr.insert(0, TAF_IND);
                        }

                        parts.setTafHeader(tafHdr.toString());

                        parts.setTafBody(s.substring(partPos.get(i + 1),
                                partPos.get(i + 2)));

                        tafRecords.add(parts);
                    }
                }
                partPos = null;
            }
        }
    }

    /**
     * Get the trace identifier associated with the message being separated.
     * 
     * @return The associated message trace identifier.
     */
    public String getTraceId() {
        return traceId;
    }

    /**
     * Set the trace identifier associated with the message being separated.
     * 
     * @param traceId
     *            The associated message trace identifier.
     */
    public void setTraceId(String traceId) {
        this.traceId = traceId;
    }

}
