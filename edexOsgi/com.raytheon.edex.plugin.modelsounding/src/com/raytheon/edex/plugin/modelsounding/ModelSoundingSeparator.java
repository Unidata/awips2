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
package com.raytheon.edex.plugin.modelsounding;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.wmo.message.WMOHeaderFinder;

/**
 * The ModelSoundingSeparator takes a potential weather message and attempts to
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
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Dec 02, 2013  2537     bsteffen    Switch logger to ufstatus.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ModelSoundingSeparator extends AbstractRecordSeparator {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ModelSoundingSeparator.class);

    // List of report start locations.
    private List<Integer> reportLocations = null;

    private boolean hasNext = false;

    private byte[] messageData = null;

    private static int chunkSize;

    // Select the next report to retrieve via getRecord().
    private int currentReport = -1;

    /**
     * Default constructor.
     */
    public ModelSoundingSeparator() {
        chunkSize = 0;
    }

    private ModelSoundingSeparator(int size) {
        chunkSize = size;
    }

    /**
     * 
     * @param size
     */
    public void setChunkSize(int size) {
        logger.info("Setting chunkSize to " + size);
        if (size >= 0) {
            chunkSize = size;
        } else {
            chunkSize = 0;
        }
    }

    /**
     * Factory method to return a separator to the client.
     * 
     * @param rawMessage
     * @return
     */
    public ModelSoundingSeparator getSeparator(byte[] rawMessage,
            Headers headers) {
        ModelSoundingSeparator separator = new ModelSoundingSeparator(chunkSize);
        separator.setData(rawMessage, headers);
        return separator;
    }

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#next()
     */
    @Override
    public Object next() {
        byte[] outData = null;
        if (hasNext) {
            int start = reportLocations.get(currentReport++);
            int stop = reportLocations.get(currentReport);
            outData = new byte[stop - start];
            System.arraycopy(messageData, start, outData, 0, outData.length);
            hasNext = (currentReport < reportLocations.size() - 1);
            logger.debug("Returning " + outData.length + " bytes");
        }
        return outData;
    }

    /**
     * Is there another record available?
     * 
     * @return Is there another record available?
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#hasNext()
     */
    @Override
    public boolean hasNext() {
        return hasNext;
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
        if ((rawMessage == null) || (rawMessage.length == 0)) {
            hasNext = false;
        } else {
            reportLocations = new ArrayList<Integer>();
            messageData = rawMessage;
            WMOHeaderFinder finder = new WMOHeaderFinder(messageData);
            int position = finder.getNext();
            while (position > WMOHeaderFinder.NOT_FOUND) {
                reportLocations.add(position);
                logger.debug("Location at " + position);
                position = finder.getNext();
            }
            finder.dispose();
            if (reportLocations.size() > 2) {
                if (chunkSize > 0) {
                    int fence = reportLocations.get(0) + chunkSize;
                    for (int i = 1; i < reportLocations.size() - 1;) {
                        int n = reportLocations.get(i);
                        if (n < fence) {
                            reportLocations.remove(i);
                        } else {
                            fence = n + chunkSize;
                            i++;
                        }
                    }
                }
                // Ok now to add the end of the data.
                reportLocations.add(rawMessage.length);
                currentReport = 0;
                hasNext = true;
            } else {
                reportLocations.add(rawMessage.length);
                currentReport = 0;
                hasNext = true;
            }
            logger.debug("Total of " + reportLocations.size()
                    + " messages found");
        }
    }

}
