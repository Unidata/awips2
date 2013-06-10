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
package com.raytheon.uf.common.comm.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.http.entity.AbstractHttpEntity;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient.OStreamHandler;

/**
 * An http entity for streaming an object when the content length is needed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2013            dfriedman     Initial creation
 * Jan 21, 2013          njensen        Moved to separate class
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class OStreamEntity extends AbstractHttpEntity {

    private OStreamHandler handler;

    private long contentLength = -1;

    public OStreamEntity(OStreamHandler handler) {
        this.handler = handler;
    }

    @Override
    public InputStream getContent() throws IOException, IllegalStateException {
        throw new IllegalStateException(
                "OStreamEntity does not support getContent().");
    }

    @Override
    public long getContentLength() {
        if (contentLength < 0) {
            CountingStream cs = new CountingStream();
            try {
                handler.writeToStream(cs);
                contentLength = cs.count;
            } catch (CommunicationException e) {
                // ignore
            }
        }
        return contentLength;
    }

    @Override
    public boolean isRepeatable() {
        return true;
    }

    @Override
    public boolean isStreaming() {
        return false;
    }

    @Override
    public void writeTo(OutputStream stream) throws IOException {
        try {
            handler.writeToStream(stream);
        } catch (CommunicationException e) {
            throw new IOException(e.getMessage(), e.getCause());
        }
    }

    private class CountingStream extends OutputStream {
        long count;

        @Override
        public void write(int b) throws IOException {
            ++count;
        }

        @Override
        public void write(byte[] b) throws IOException {
            count += b.length;
        }

        @Override
        public void write(byte[] b, int off, int len) throws IOException {
            count += len;
        }
    }

}
