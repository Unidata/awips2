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
package com.raytheon.uf.edex.plugin.mpe;

import java.nio.ByteBuffer;

/**
 * Advance a ByteBuffer to the byte after byte-sequence pattern or throw an
 * exception.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2016 4622       jschmid     Initial creation
 * 
 * </pre>
 * 
 * @author jschmid
 */

public class SequenceFinder {

    /**
     * Advance ByteBuffer to byte after byte-sequence pattern or throw
     * BufferUnderflowException if not found.
     * 
     * @param productBytes
     *            ByteBuffer of data to match against.
     * @param sequenceBytes
     *            Byte sequence to match in ByteBuffer data.
     */
    public boolean advanceToSequence(ByteBuffer productBytes,
            byte[] sequenceBytes) {

        int pos = productBytes.position();
        int matchIndex = 0;

        while (productBytes.hasRemaining() && matchIndex < sequenceBytes.length) {

            if (productBytes.get() == sequenceBytes[matchIndex]) {
                matchIndex++;
                if (sequenceBytes.length == matchIndex) {
                    return true; // Buffer now at 1-byte past end of match
                }
            } else {
                pos++;

                if (matchIndex > 0) {
                    matchIndex = 0;
                    productBytes.position(pos);
                }
            }
        }
        return (matchIndex == sequenceBytes.length);
    }
}
