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

import java.io.InputStream;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient.IStreamHandler;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * A stream handler that streams the response back through dynamic serialize to
 * produce an object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DynamicSerializeStreamHandler implements IStreamHandler {

    protected Object resp;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.comm.HttpClient.IStreamHandler#handleStream(java
     * .io.InputStream)
     */
    @Override
    public void handleStream(InputStream is) throws CommunicationException {
        try {
            resp = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).deserialize(is);
        } catch (SerializationException e) {
            throw new CommunicationException(
                    "Error deserializing streamed response");
        }
    }

    public Object getResponseObject() {
        return resp;
    }

}
