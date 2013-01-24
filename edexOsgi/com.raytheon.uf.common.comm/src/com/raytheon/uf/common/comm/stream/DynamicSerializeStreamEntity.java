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

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;

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

public class DynamicSerializeStreamEntity extends AbstractHttpEntity {

    private Object obj;

    public DynamicSerializeStreamEntity(Object obj) {
        super();
        this.obj = obj;
        this.setChunked(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#getContent()
     */
    @Override
    public InputStream getContent() throws IOException, IllegalStateException {
        throw new UnsupportedOperationException(
                "Stream does not support getContent()");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#getContentLength()
     */
    @Override
    public long getContentLength() {
        return -1;
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
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.http.HttpEntity#writeTo(java.io.OutputStream)
     */
    @Override
    public void writeTo(OutputStream os) throws IOException {
        try {
            DynamicSerializationManager.getManager(SerializationType.Thrift)
                    .serialize(obj, os);
        } catch (SerializationException e) {
            throw new IOException("Error serializing " + obj.getClass()
                    + " to stream", e);
        }
    }

}
