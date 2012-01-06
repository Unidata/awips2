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
package com.raytheon.uf.edex.plugin.mesowest;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.edex.decodertools.core.DecoderInput;
import com.raytheon.uf.edex.decodertools.core.IDecoderInput;
import com.raytheon.uf.edex.plugin.mesowest.decoder.MESOWestConstants;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20090310           1969 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class MESOWestSeparator extends AbstractRecordSeparator {

    private Log logger = LogFactory.getLog(getClass());

    private static final int MIN_LINE_LEN = 10;

    private byte[] messageData = null;

    private Headers headerData = null;

    private List<String> reports = null;

    private String traceId = null;

    private int currentReport = -1;

    private UUID uuid = null;

    /**
     * 
     * @param data
     * @param headers
     */
    private MESOWestSeparator(byte[] data, Headers headers) {
        headerData = headers;
        if (headerData != null) {
            traceId = (String) headerData.get(MESOWestConstants.TRACEID);
        }

        uuid = UUID.randomUUID();

        setData(data, headers);
    }

    /**
     * Empty constructor so Spring doesn't complain. TODO : Make sure this is
     * needed.
     */
    public MESOWestSeparator() {
    }

    /**
     * Create an instance of a separator using the supplied data.
     * 
     * @param data
     * @param headers
     * @return
     * @throws Exception
     */
    public static MESOWestSeparator separate(byte[] data, Headers headers)
            throws Exception {
        return new MESOWestSeparator(data, headers);
    }

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     */
    @Override
    public IDecoderInput next() {
        IDecoderInput data = null;
        if (hasNext()) {

            String rpt = reports.get(currentReport++);
            data = new DecoderInput(null, rpt);
            data.setProperty("uuid", uuid.toString());

            if (rpt.startsWith(MESOWestConstants.D_PARMLEADER)) {
                data.setProperty(MESOWestConstants.K_DATATYPE,
                        MESOWestConstants.T_PARMHEADER);
            } else if (rpt.startsWith(MESOWestConstants.LAST_DATA_ITEM)) {
                data.setProperty(MESOWestConstants.K_DATATYPE,
                        MESOWestConstants.T_LASTITEM);
            } else {
                data.setProperty(MESOWestConstants.K_DATATYPE,
                        MESOWestConstants.T_REPORTDATA);
            }
            data.setProperty(MESOWestConstants.TRACEID, traceId);
        }
        logger.debug("Separator.next()");
        return data;
    }

    /**
     * Is there another record available?
     * 
     * @return Is there another record available?
     */
    @Override
    public boolean hasNext() {
        logger.debug("Separator.hasNext()");
        boolean hasNext = false;
        if (reports != null) {
            if (reports.size() > 0) {
                hasNext = currentReport < reports.size();
            } else {
                reports.clear();
                reports = null;
            }
        }
        return hasNext;
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
        reports = new ArrayList<String>();
        if (rawMessage != null) {

            InputStream strm = new ByteArrayInputStream(rawMessage);

            separate(strm);
        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        } else {
            logger.info("No reports found in data.");
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
     * Separate the mesonet data, one report per line. No need to account for
     * reports spanning lines.
     * 
     * @param strm
     *            An input stream to the meso data.
     */
    private void separate(InputStream strm) {
        BufferedReader in = null;

        try {
            in = new BufferedReader(new InputStreamReader(strm));

            String s = null;
            while ((s = in.readLine()) != null) {
                if (s.length() > MIN_LINE_LEN) {
                    reports.add(s.trim());
                }
            }
            reports.add(MESOWestConstants.LAST_DATA_ITEM);

        } catch (IOException ioe) {
            logger.error("Error reading mesowest data " + ioe);
        }
    }

}
