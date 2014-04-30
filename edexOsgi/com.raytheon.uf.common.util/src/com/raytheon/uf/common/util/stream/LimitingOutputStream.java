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
package com.raytheon.uf.common.util.stream;

import java.io.IOException;
import java.io.OutputStream;

import com.raytheon.uf.common.util.SizeUtil;

/**
 * Stream that limits the number of bytes that can be written. If limit is
 * reached an IOException is thrown. This does not preclude more bytes from
 * being written to the stream.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2014 2928       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class LimitingOutputStream extends CountingOutputStream {
    /**
     * Maximum number of bytes that can be written to the wrapped stream before
     * errors are thrown on write.
     */
    protected final long maxBytes;

    /**
     * Wraps the given {@code OutputStream} and will throw IOException once the
     * specified number of bytes have been written to the stream.
     * 
     * @param inputStream
     * @param maxBytes
     */
    public LimitingOutputStream(OutputStream outputStream, long maxBytes) {
        super(outputStream);
        this.maxBytes = maxBytes;
    }

    /**
     * Tracks number of bytes written to wrapped stream. An IOException will be
     * thrown if number of bytes written exceeds {@code maxBytes}.
     * 
     * @param bytesWritten
     * @throws IOException
     */
    @Override
    public void increaseBytesWritten(int bytesWritten) throws IOException {
        super.increaseBytesWritten(bytesWritten);
        long curBytes = getBytesWritten();
        if (curBytes > maxBytes) {
            throw new IOException("Maximum number of bytes ["
                    + SizeUtil.prettyByteSize(maxBytes)
                    + "] has been exceeded by stream");
        }
    }
}
