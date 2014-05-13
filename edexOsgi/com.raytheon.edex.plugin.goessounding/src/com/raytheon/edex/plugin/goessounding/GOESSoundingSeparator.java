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
package com.raytheon.edex.plugin.goessounding;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.bufr.BUFROffsets;
import com.raytheon.uf.edex.decodertools.bufr.BUFRSection0;
import com.raytheon.uf.edex.decodertools.bufr.BUFRSection5;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * The GOESSounding a potential weather message and attempts to determine the
 * WMO header and data type of the enclosed data. Normal usage is to create an
 * instance and set the message data using the setData method. When complete the
 * separator contains the WMO header, and all of the data decoded. The message
 * reports are available using hasNext to determine if decoded data is
 * available, and the getRecord method to retrieve the actual report data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080414           1077 jkorman     Initial implementation.
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed unused logger
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class GOESSoundingSeparator extends AbstractRecordSeparator {

    private WMOHeader wmoHeader;

    private byte[] rawBUFR;

    // List of report data to be decoded.
    private List<BUFROffsets> reportData = null;

    // Select the next report to retrieve via getRecord().
    private int currentReport = -1;

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#next()
     */
    // @Override
    public GoesSoundingInput next() {
        GoesSoundingInput data = null;
        if (hasNext()) {
            data = new GoesSoundingInput();
            data.setWMOHeader(wmoHeader);
            BUFROffsets offset = reportData.get(currentReport);
            byte[] retData = new byte[offset.getLength()];
            System.arraycopy(rawBUFR, offset.getStartPos(), retData, 0,
                    offset.getLength());
            data.setDocumentData(retData);
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
        boolean hasMore = false;
        if (reportData != null) {
            hasMore = ((reportData.size() > 0) && (currentReport < reportData
                    .size()));
            if (!hasMore) {
                // Out of data, so deallocate stuff
                rawBUFR = null;
                reportData = null;
            }
        }
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
                rawBUFR = rawMessage;
                reportData = new ArrayList<BUFROffsets>();
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                wmoHeader = new WMOHeader(rawMessage, fileName);

                int start = findStart(0, rawMessage);
                while (start >= 0) {
                    // we have the starting position. So get the document
                    // length.
                    int len = findBUFRLength(start, rawMessage);
                    // and verify that the document ends where it supposed to!
                    if (verifyEnd(start + len, rawMessage)) {
                        BUFROffsets offset = new BUFROffsets(start, start + len);
                        reportData.add(offset);
                    }
                    // Update the location for the next possible document.
                    start = findStart(start + len, rawMessage);
                }
            }
        } finally {
            if ((reportData != null) && (reportData.size() > 0)) {
                currentReport = 0;
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
    private static int findStart(int startAt, byte[] message) {
        return DecoderTools.indexOf(startAt, BUFRSection0.BUFR_HDR, message);
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
    private static boolean verifyEnd(int endPos, byte[] message) {
        // find the section 5 signature.
        int sigPos = endPos - BUFRSection5.BUFR_TRAILER.length;
        int i = DecoderTools
                .indexOf(sigPos, BUFRSection5.BUFR_TRAILER, message);

        return (i == sigPos);
    }

    /**
     * Attempt to read the BUFR section 0 length segment.
     * 
     * @param startAt
     *            The starting position of the BUFR signature.
     * @return The length of the BUFR document.
     */
    private static int findBUFRLength(int startAt, byte[] message) {
        final int sigLen = BUFRSection0.BUFR_HDR.length;
        final int lenSize = 3;

        int value = 0;
        for (int i = startAt + sigLen; i < startAt + sigLen + lenSize; i++) {
            value <<= 8;
            value |= (message[i] & 0xFF);
        }
        return value;
    }

}
