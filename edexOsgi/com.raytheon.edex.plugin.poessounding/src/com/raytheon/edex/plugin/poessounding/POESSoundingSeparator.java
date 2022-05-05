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
package com.raytheon.edex.plugin.poessounding;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.BUFRDataDocument;
import com.raytheon.uf.edex.bufrtools.BUFRDocument;
import com.raytheon.uf.edex.bufrtools.BUFRFile;
import com.raytheon.uf.edex.bufrtools.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.bufrtools.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;

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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 03, 2008 1026       jkorman     Initial implementation.
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * Dec 15, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class POESSoundingSeparator extends AbstractRecordSeparator implements
        IDescriptorFactorySelector {

    private Logger logger = LoggerFactory.getLogger(getClass());

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
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                wmoHeader = new WMOHeader(rawMessage, fileName);

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
     * @see com.raytheon.uf.edex.bufrtools.descriptors.tools.bufr.descriptors.IDescriptorFactorySelector#getSelector()
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
                        dd.setEnclosingDocument(docData.getEnclosingDocument());
                        reportData.add(dd);
                        logger.debug("Number of packet subsets = "
                                + data.size());
                    }
                }
            }
        }

    }

}
