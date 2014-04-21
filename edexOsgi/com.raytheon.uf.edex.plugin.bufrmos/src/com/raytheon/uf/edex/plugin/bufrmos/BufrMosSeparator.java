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
package com.raytheon.uf.edex.plugin.bufrmos;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData;
import com.raytheon.uf.edex.plugin.bufrmos.decoder.MOSDescriptorDelegate;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * The BufrMosSeparator takes a potential weather message and attempts to
 * determine the WMO header and data type of the enclosed data. Normal usage is
 * to create an instance and set the message data using the setData method. When
 * complete the separator contains the WMO header, and all of the data decoded.
 * The message reports are available using hasNext to determine if data is
 * available, and the getRecord method to retrieve the actual report data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080221            862 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BufrMosSeparator extends AbstractRecordSeparator implements
        IDescriptorFactorySelector {

    private Log logger = LogFactory.getLog(getClass());

    private WMOHeader wmoHeader = null;

    private byte[] messageData = null;

    private List<BUFRDataDocument> reportData = null;

    private int currentReport = -1;

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#next()
     */
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
     */
    @Override
    public boolean hasNext() {
        boolean hasMore = (reportData != null);
        hasMore = hasMore && (reportData.size() > 0);
        hasMore = hasMore && (currentReport < reportData.size());
        return hasMore;
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
        try {
            if (rawMessage != null) {

                wmoHeader = new WMOHeader(rawMessage, headers);

                if ((wmoHeader != null) && (wmoHeader.isValid())) {

                    int start = wmoHeader.getMessageDataStart();
                    int len = rawMessage.length - start;

                    messageData = new byte[len];
                    System.arraycopy(rawMessage, start, messageData, 0, len);

                    decodeBUFRData();
                }
            }
        } finally {
            if ((reportData != null) && (reportData.size() > 0)) {
                currentReport = 0;
            }
        }
    }

    /**
     * Get the message data.
     * 
     * @return The cleaned message data. For this separator, the data should be
     *         positioned to the beginning of the BUFR data within the message.
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
     * Decode the entire physical file that was used to create this separator.
     */
    @SuppressWarnings("unchecked")
    private void decodeBUFRData() {

        BUFRFile bFile = new BUFRFile(messageData, new MOSDescriptorDelegate(
                this));

        reportData = new ArrayList<BUFRDataDocument>();

        int docNum = 1;
        for (BUFRDocument doc : bFile) {
            logger.debug("Executing document " + docNum);
            BUFRDataDocument docData = doc.execute();
            logger.debug("Document " + docNum + " complete");
            docNum++;
            List<IBUFRDataPacket> data = docData.getList();
            logger.debug("Number of packet subsets = " + data.size());

            for (IBUFRDataPacket packet : data) {
                if (packet != null) {
                    if (packet instanceof BUFRSublistPacket) {
                        List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                                .getValue();
                        BUFRDataDocument dd = new BUFRDataDocument(p);
                        reportData.add(dd);
                    }
                }
            }
        }
    }

    /**
     * Get the model selector value associated with this separator. The
     * BUFRMOSStaticData uses the separator WMOHEader to determine the selector.
     * 
     * @return The model selector associated with this separator.
     */
    @Override
    public String getSelector() {
        String retValue = null;

        BUFRMOSStaticData data = BUFRMOSStaticData.getInstance();
        if (data.isLoaded()) {
            retValue = data.getMOSType(this);
        }

        return retValue;
    }

    public int size() {
        return (reportData == null ? 0 : reportData.size());
    }
}
