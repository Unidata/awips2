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
package com.raytheon.uf.common.serialization.thrift;

import java.io.InputStream;
import java.io.OutputStream;

import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransport;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationContextBuilder;

/**
 * Build a Thrift Serialization context
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 12, 2008				chammack	Initial creation
 * Jul 23, 2013  2215       njensen     Updated for thrift 0.9.0
 * Aug 06, 2013    2228     njensen     Added buildDeserializationContext(byte[], dsm)
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ThriftSerializationContextBuilder implements
        ISerializationContextBuilder {

    /**
     * Default constructor
     */
    public ThriftSerializationContextBuilder() {

    }

    @Override
    public IDeserializationContext buildDeserializationContext(
            InputStream data, DynamicSerializationManager manager) {
        TTransport transport = new TIOStreamTransport(data);
        SelfDescribingBinaryProtocol proto = new SelfDescribingBinaryProtocol(
                transport);

        return new ThriftSerializationContext(proto, manager);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContextBuilder#
     * buildSerializationContext()
     */
    @Override
    public ISerializationContext buildSerializationContext(OutputStream data,
            DynamicSerializationManager manager) {

        TTransport transport = new TIOStreamTransport(data);
        SelfDescribingBinaryProtocol proto = new SelfDescribingBinaryProtocol(
                transport);

        return new ThriftSerializationContext(proto, manager);
    }

    @Override
    public IDeserializationContext buildDeserializationContext(byte[] data,
            DynamicSerializationManager manager) {
        TTransport transport = new TMemoryInputTransport(data);

        SelfDescribingBinaryProtocol proto = new SelfDescribingBinaryProtocol(
                transport);

        return new ThriftSerializationContext(proto, manager);
    }

}
