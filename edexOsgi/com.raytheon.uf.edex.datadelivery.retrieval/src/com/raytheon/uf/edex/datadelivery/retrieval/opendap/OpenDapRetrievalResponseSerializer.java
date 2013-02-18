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
package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

import dods.dap.DataDDS;

/**
 * Dynamic serializer for OpenDAP retrieval responses.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2013 1543       djohnson     Initial creation
 * Feb 15, 2013 1543       djohnson     Also can be used as JAXB adapter for DataDDS.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class OpenDapRetrievalResponseSerializer extends
        XmlAdapter<byte[], DataDDS> implements
        ISerializationTypeAdapter<OpenDapRetrievalResponse> {

    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer,
            OpenDapRetrievalResponse object) throws SerializationException {
        serializer.writeObject(object.getAttribute());
        try {
            serializer.writeBinary(marshal(object.getPayLoad()));
        } catch (Exception e) {
            throw new SerializationException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public OpenDapRetrievalResponse deserialize(
            IDeserializationContext deserializer) throws SerializationException {
        OpenDapRetrievalResponse response = new OpenDapRetrievalResponse();
        response.setAttribute((RetrievalAttribute) deserializer.readObject());
        try {
            response.setPayLoad(unmarshal(deserializer.readBinary()));
        } catch (Exception e) {
            throw new SerializationException(e);
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DataDDS unmarshal(byte[] v)
            throws Exception {
        return DodsUtils.restoreDataDdsFromByteArray(v);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public byte[] marshal(DataDDS v)
            throws Exception {
        return DodsUtils.convertDataDdsToByteArray(v);
    }
}
