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
import java.io.InputStream;

import com.raytheon.uf.common.util.SizeUtil;

/**
 * Stream that limits the number of bytes that can be read. If limit is reached
 * an IOException is thrown. This does not preclude more bytes being read from
 * the stream.
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
public class LimitingInputStream extends CountingInputStream {
    /**
     * Maximum number of bytes that can be read from the wrapped stream before
     * errors are thrown on read.
     */
    protected final long maxBytes;

    /**
     * Wraps the given {@code InputStream} and will throw IOException once the
     * specified number of bytes have been read from the stream.
     * 
     * @param inputStream
     * @param maxBytes
     */
    public LimitingInputStream(InputStream inputStream, long maxBytes) {
        super(inputStream);
        this.maxBytes = maxBytes;
    }

    /**
     * Tracks number of bytes read from wrapped stream. An IOException will be
     * thrown if number of bytes read exceeds {@code maxBytes}.
     * 
     * @param bytesRead
     * @throws IOException
     */
    @Override
    public void increaseBytesRead(int bytesRead) throws IOException {
        super.increaseBytesRead(bytesRead);
        long curBytes = getBytesRead();
        if (curBytes > maxBytes) {
            throw new IOException("Maximum number of bytes ["
                    + SizeUtil.prettyByteSize(maxBytes)
                    + "] has been exceeded by stream");
        }
    }
}
