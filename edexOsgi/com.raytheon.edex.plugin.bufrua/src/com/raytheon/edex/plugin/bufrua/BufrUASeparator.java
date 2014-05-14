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
package com.raytheon.edex.plugin.bufrua;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.plugin.bufrua.decoder.UARawinDescriptorDelegate;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.BUFROffsets;
import com.raytheon.uf.edex.decodertools.bufr.BUFRSection0;
import com.raytheon.uf.edex.decodertools.bufr.BUFRSection5;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * The BufrUASeparator takes a potential weather message and attempts to
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
 * 20071127            382 jkorman     Initial Coding.
 * 20080107            382 jkorman     Fixed NullPointerEx in hasNext.
 * 20080214            862 jkorman     Refactored data separation into BUFRFile.
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BufrUASeparator extends AbstractRecordSeparator implements
        IDescriptorFactorySelector {
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    // WMO header of the message containing the BUFR data.
    private WMOHeader wmoHeader = null;

    // Raw message data.
    private byte[] messageData = null;

    // Positions within the data used to separate logical BUFR messages.
    private List<BUFROffsets> reports = null;

    // List of BUFR documents created from the physical message.
    private List<BUFRDataDocument> reportData = null;

    // Pointer to the current BUFR document to be processed.
    private int currentReport = -1;

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
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
        reports = new ArrayList<BUFROffsets>();
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
                    findBUFRParts();

                    decodeBUFRData();

                    reports = null;
                }
            }
        } finally {
            reports = null;
            if ((reportData != null) && (reportData.size() > 0)) {
                currentReport = 0;
            }
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
     * Decode the entire physical file that was used to create this separator.
     */
    @SuppressWarnings("unchecked")
    private void decodeBUFRData() {

        BUFRFile bFile = new BUFRFile(messageData,
                new UARawinDescriptorDelegate(this));
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

    /**
     * Get the name of the BUFR selector for upper air data. BUFRUA uses the
     * default BUFR tables.
     */
    @Override
    public String getSelector() {
        return "DEFAULT";
    }

    /**
     * Determine the number and location offsets of any BUFR data contained in
     * this message.
     */
    private void findBUFRParts() {
        if ((messageData != null) && (messageData.length > 0)) {

            int start = findStart(0);
            while (start >= 0) {
                // we have the starting position of the BUFR signature.
                int len = findBUFRLength(start);

                if (verifyEnd(start, start + len)) {
                    BUFROffsets offset = new BUFROffsets(start, start + len);
                    reports.add(offset);
                }
                start = findStart(start + len);
            }
        }
    }

    /**
     * Find the start of a BUFR section starting at some specified location
     * within the data.
     * 
     * @param startAt
     *            The starting position.
     * @return The found location. If not found a value of -1 is returned.
     */
    private int findStart(int startAt) {
        return DecoderTools
                .indexOf(startAt, BUFRSection0.BUFR_HDR, messageData);
    }

    /**
     * Verify that the end signature is located at the declared end of the BUFR
     * document.
     * 
     * @param startAt
     *            Starting location of the BUFR document.
     * @param endPos
     *            Declared ending location of the BUFR document.
     * @return Is there a valid end marker in the document?
     */
    private boolean verifyEnd(int startAt, int endPos) {
        boolean valid = false;
        // find the section 5 signature.
        int i = DecoderTools.indexOf(startAt, BUFRSection5.BUFR_TRAILER,
                messageData);
        if (i > startAt) {
            valid = (i == endPos - BUFRSection5.BUFR_TRAILER.length);
        }
        return valid;
    }

    /**
     * Attempt to read the BUFR section 0 length segment.
     * 
     * @param startAt
     *            The starting position of the BUFR signature.
     * @return The length of the BUFR document.
     */
    private int findBUFRLength(int startAt) {
        final int sigLen = BUFRSection0.BUFR_HDR.length;
        final int lenSize = 3;

        int value = 0;
        for (int i = startAt + sigLen; i < startAt + sigLen + lenSize; i++) {
            value <<= 8;
            value |= (messageData[i] & 0xFF);
        }
        return value;
    }

}
