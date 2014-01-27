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
package com.raytheon.edex.plugin.profiler;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * The ProfilerSeparator takes a potential weather message and attempts to
 * determine the WMO header and data type of the enclosed data. Normal usage is
 * to create an instance and set the message data using the setData method. When
 * complete the separator contains the WMO header, and all of the data decoded.
 * The message reports are available using hasNext to determine if data is
 * available, and the getRecord method to retrieve the actual report data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  969      jkorman     Initial implementation.
 * Dec 03, 2013  2537     bsteffen    Switch logger to ufstatus.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ProfilerSeparator extends AbstractRecordSeparator implements
        IDescriptorFactorySelector {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ProfilerSeparator.class);

    private WMOHeader wmoHeader = null;

    // List of report data to be decoded.
    private List<BUFRDataDocument> reportData = null;

    // Select the next report to retrieve via getRecord().
    private int currentReport = -1;

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#next()
     */
    // @Override
    public Object next() {
        BUFRDataDocument data = null;
        if (hasNext()) {
            data = reportData.get(currentReport);
            currentReport++;
        }
        return data;
    }

    /**
     * Is there another record available?
     * 
     * @return Is there another record available?
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#hasNext()
     */
    @Override
    public boolean hasNext() {
        boolean hasMore = (reportData != null);
        hasMore = hasMore && (reportData.size() > 0);
        hasMore = hasMore && (currentReport < reportData.size());
        return hasMore;
    }

    /**
     * Set the data to be decoded. This method also performs the BUFR decode
     * leaving a collection of BUFR data subsets to be interpreted.
     * 
     * @param rawMessage
     *            A byte array containing the BUFR data to be decoded.
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#setData(byte[])
     */
    @Override
    public void setData(byte[] rawMessage, Headers headers) {
        currentReport = -1;
        try {
            if (rawMessage != null) {

                wmoHeader = new WMOHeader(rawMessage, headers);

                if ((wmoHeader != null) && (wmoHeader.isValid())) {

                    int start = wmoHeader.getMessageDataStart();
                    int len = rawMessage.length - start;

                    byte[] messageData = new byte[len];
                    System.arraycopy(rawMessage, start, messageData, 0, len);

                    decodeBUFRData(messageData);

                }
            }
        } finally {
            if ((reportData != null) && (reportData.size() > 0)) {
                currentReport = 0;
            }
        }
    }

    /**
     * Get the selector associated with this separators data. ProfilerSeparator
     * always returns "DEFAULT".
     * 
     * @return Return the string value "DEFAULT".
     * @see com.raytheon.edex.tools.bufr.descriptors.IDescriptorFactorySelector#getSelector()
     */
    @Override
    public String getSelector() {
        return "DEFAULT";
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
     * Decode the entire physical file that was used to create this separator.
     */
    @SuppressWarnings("unchecked")
    private void decodeBUFRData(byte[] messageData) {
        BUFRFile bFile = new BUFRFile(messageData,
                new DefaultDescriptorDelegate(this));
        reportData = new ArrayList<BUFRDataDocument>();

        for (BUFRDocument doc : bFile) {
            BUFRDataDocument docData = doc.execute();

            List<IBUFRDataPacket> data = docData.getList();
            for (IBUFRDataPacket packet : data) {
                if (packet != null) {
                    if (packet instanceof BUFRSublistPacket) {
                        List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                                .getValue();
                        BUFRDataDocument dd = new BUFRDataDocument(p);
                        reportData.add(dd);
                        logger.debug("Number of packet subsets = "
                                + data.size());
                    }
                }
            }
        }

    }

}
