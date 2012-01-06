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
package com.raytheon.uf.edex.decodertools.bufr.io;

import java.io.Closeable;
import java.io.IOException;

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
public abstract class BUFRBitInputStream implements Closeable {

    /**
     * Returns an estimate of the number of bytes that can be read (or skipped
     * over) from this input stream without blocking by the next invocation of a
     * method for this input stream.
     */
    public abstract long available();

    /**
     * Marks the current position in this input stream.
     * 
     * @param readlimit
     */
    public abstract void mark(int readlimit);

    /**
     * Tests if this input stream supports the mark and reset methods.
     * 
     * @return
     */
    public abstract boolean markSupported();

    /**
     * Repositions this stream to the position at the time the mark method was
     * last called on this input stream.
     */
    public abstract void reset();

    /**
     * Skips over and discards n bits of data from this input stream.
     * 
     * @param skipAmount
     *            The number of bits to be skipped.
     * @return The number of bits that were skipped.
     */
    public abstract long skip(long skipAmount);

    /**
     * Reads the next bit of data from the input stream.
     * 
     * @return The next bit of data from the input stream.
     */
    public abstract int read();

    /**
     * Reads a specified number of bits of data from the input stream.
     * 
     * @param numberOfBits
     *            The number of bits to be read.
     * @return The next bit of data from the input stream.
     */
    public abstract long read(int numberOfBits);

    /**
     * Closes this stream and releases any system resources associated with it.
     * If the stream is already closed then invoking this method has no effect.
     * 
     * @throws IOException
     *             if an I/O error occurs
     */
    @Override
    public abstract void close() throws IOException;

}
