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
package com.raytheon.edex.plugin.redbook;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080512           1131 jkorman     Initial implementation.
 * 20080529           1131 jkorman     Added traceId constructor and getter
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class RedbookSeparator extends AbstractRecordSeparator {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(RedbookSeparator.class);

    private WMOHeader wmoHeader = null;

    private byte[] messageData = null;

    private List<byte[]> reports = null;

    private int currentReport = -1;

    private final String traceId;

    public RedbookSeparator(String traceId) {
        this.traceId = traceId;
    }

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     */
    // @Override
    @Override
    public Object next() {
        byte[] data = null;
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

        wmoHeader = new WMOHeader(rawMessage, headers);
        if (wmoHeader.isValid()) {
            reports = new ArrayList<byte[]>();
            int start = wmoHeader.getMessageDataStart();

            int len = rawMessage.length - start;
            messageData = new byte[len];
            System.arraycopy(rawMessage, start, messageData, 0, len);
            reports.add(messageData);
        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        } else {
            logger.debug("No reports found in data");
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
     * @return the traceId
     */
    public String getTraceId() {
        return traceId;
    }

}
