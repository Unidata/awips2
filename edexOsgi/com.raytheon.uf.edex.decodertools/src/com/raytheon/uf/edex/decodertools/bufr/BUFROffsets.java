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
package com.raytheon.uf.edex.decodertools.bufr;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFROffsets {
    private final int startPos;

    private final int stopPos;

    public BUFROffsets(int start, int stop) {
        startPos = start;
        stopPos = stop;
    }

    /**
     * Get the start position for BUFR data within a buffer.
     * 
     * @return The data start position.
     */
    public int getStartPos() {
        return startPos;
    }

    /**
     * Get the stop position for BUFR data within a buffer.
     * 
     * @return The data stop position.
     */
    public int getStopPos() {
        return stopPos;
    }

    /**
     * Get the length of the BUFR data.
     * 
     * @return The length of the BUFR data.
     * @return
     */
    public int getLength() {
        return stopPos - startPos;
    }
}
