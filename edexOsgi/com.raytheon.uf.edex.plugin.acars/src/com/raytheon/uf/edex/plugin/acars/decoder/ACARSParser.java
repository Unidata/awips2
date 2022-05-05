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
package com.raytheon.uf.edex.plugin.acars.decoder;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.BUFRDataDocument;
import com.raytheon.uf.edex.bufrtools.BUFRDocument;
import com.raytheon.uf.edex.bufrtools.BUFRFile;
import com.raytheon.uf.edex.bufrtools.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.bufrtools.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;

/**
 * ACARS Parser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009  1939       jkorman     Initial creation
 * Mar 27, 2014  2811       skorolev    Updated logger.
 * May 14, 2014 2536        bclement    moved WMO Header to common
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSParser implements Iterator<BUFRDataDocument>,
        Iterable<BUFRDataDocument>, IDescriptorFactorySelector {

    private IUFStatusHandler logger = UFStatus.getHandler(ACARSParser.class);

    // WMO header of the message containing the BUFR data.
    private WMOHeader wmoHeader = null;

    // Raw message data.
    private byte[] messageData = null;

    private static IDescriptorFactoryDelegate delegate = null;

    // Positions within the data used to separate logical BUFR messages.
    // private List<BUFROffsets> reports = null;

    // List of BUFR documents created from the physical message.
    private List<BUFRDataDocument> reportData = null;

    // Pointer to the current BUFR document to be processed.
    private int currentReport = -1;

    /**
     * 
     * @param rawData
     */
    public ACARSParser(byte[] rawMessage, Headers headers) {
        if (delegate == null) {
            delegate = new DefaultDescriptorDelegate(this);
        }
        setInternalData(rawMessage, headers);
    }

    /**
     * 
     */
    public void setInternalData(byte[] rawMessage, Headers headers) {
        currentReport = -1;
        // reports = new ArrayList<BUFROffsets>();
        try {
            if (rawMessage != null) {

                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                wmoHeader = new WMOHeader(rawMessage, fileName);

                if ((wmoHeader != null) && (wmoHeader.isValid())) {

                    int start = wmoHeader.getMessageDataStart();
                    int len = rawMessage.length - start;

                    messageData = new byte[len];
                    System.arraycopy(rawMessage, start, messageData, 0, len);
                    // findBUFRParts();

                    decodeBUFRData();
                }
            }
        } finally {
            // reports = null;
            if ((reportData != null) && (reportData.size() > 0)) {
                currentReport = 0;
            }
        }
    }

    /**
     * 
     */
    public Iterator<BUFRDataDocument> iterator() {
        Iterator<BUFRDataDocument> it = null;

        if (reportData != null) {
            it = reportData.iterator();
        }

        return it;
    }

    /**
     * 
     */
    @Override
    public boolean hasNext() {
        boolean hasMore = (reportData != null);
        hasMore = hasMore && (reportData.size() > 0);
        hasMore = hasMore && (currentReport < reportData.size());
        return hasMore;
    }

    /**
     * 
     */
    @Override
    public BUFRDataDocument next() {
        BUFRDataDocument data = null;
        if (hasNext()) {
            data = reportData.get(currentReport);
            currentReport++;
        }
        return data;
    }

    /**
     * 
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException(
                "ACARSParser does not support remove()");
    }

    /**
     * Get the name of the BUFR selector for acars data.
     * 
     * @return The BUFR selector.
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
    private void decodeBUFRData() {

        BUFRFile bFile = new BUFRFile(messageData, delegate);
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
