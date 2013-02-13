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
import java.util.zip.GZIPOutputStream;

import org.apache.http.entity.AbstractHttpEntity;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.ByteArrayOutputStreamPool;
import com.raytheon.uf.common.util.ByteArrayOutputStreamPool.ByteArrayOutputStream;

/**
 * An Http Entity that serializes an object through dynamic serialize.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 22, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DynamicSerializeEntity extends AbstractHttpEntity {

    private Object obj;

    private boolean stream;

    private boolean gzip;

    private byte[] objAsBytes;

    /**
     * Constructor
     * 
     * @param obj
     *            the object to be sent over http
     * @param stream
     *            whether or not to stream the object over http. Ignored if gzip
     *            is true.
     * @param gzip
     *            whether or not to gzip the object's bytes. Note that if gzip
     *            is true, stream will be ignored.
     */
    public DynamicSerializeEntity(Object obj, boolean stream, boolean gzip) {
        super();
        this.obj = obj;
        this.setChunked(!gzip && stream);
        this.gzip = gzip;
        this.stream = stream;
        if (gzip) {
            // TODO can't support streaming gzip at this time
            this.stream = false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#getContent()
     */
    @Override
    public InputStream getContent() throws IOException, IllegalStateException {
        throw new UnsupportedOperationException(
                "DynamicSerializeEntity does not support getContent()");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#getContentLength()
     */
    @Override
    public long getContentLength() {
        if (isStreaming()) {
            return -1;
        } else {
            if (objAsBytes == null) {
                try {
                    objAsBytes = convertObjToBytes();
                } catch (IOException e) {
                    throw new RuntimeException("Error getting content length",
                            e);
                }
            }
            return objAsBytes.length;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#isRepeatable()
     */
    @Override
    public boolean isRepeatable() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#isStreaming()
     */
    @Override
    public boolean isStreaming() {
        return stream;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#writeTo(java.io.OutputStream)
     */
    @Override
    public void writeTo(OutputStream os) throws IOException {
        if (isStreaming()) {
            try {
                DynamicSerializationManager
                        .getManager(SerializationType.Thrift)
                        .serialize(obj, os);
            } catch (SerializationException e) {
                throw new IOException("Error serializing "
                        + (obj != null ? obj.getClass() : null) + " to stream",
                        e);
            }
        } else {
            if (objAsBytes == null) {
                objAsBytes = convertObjToBytes();
            }
            os.write(objAsBytes);
        }
    }

    /**
     * Converts the object to bytes, and gzips those bytes if gzip is true.
     * 
     * @return the DynamicSerialize bytes representing the object
     * @throws IOException
     */
    private byte[] convertObjToBytes() throws IOException {
        byte[] bytes = null;
        try {
            bytes = SerializationUtil.transformToThrift(obj);
        } catch (SerializationException e) {
            throw new IOException("Error serializing object " + obj, e);
        }
        if (gzip) {
            ByteArrayOutputStream byteStream = ByteArrayOutputStreamPool
                    .getInstance().getStream(bytes.length);
            GZIPOutputStream gzipStream = new GZIPOutputStream(byteStream);
            gzipStream.write(bytes);
            gzipStream.finish();
            gzipStream.flush();
            bytes = byteStream.toByteArray();
            gzipStream.close();
        }
        return bytes;
    }

}
