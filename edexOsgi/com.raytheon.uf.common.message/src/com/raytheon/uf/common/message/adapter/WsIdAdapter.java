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
package com.raytheon.uf.common.message.adapter;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2009            randerso     Initial creation
 * Apr 25, 2012       545  randerso     Repurposed the lockKey field as threadId
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WsIdAdapter extends XmlAdapter<String, WsId> implements
        ISerializationTypeAdapter<WsId> {

    @Override
    public String marshal(WsId wsId) throws Exception {
        return wsId.toString();
    }

    @Override
    public WsId unmarshal(String s) throws Exception {
        return new WsId(s);
    }

    @Override
    public WsId deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        String s = deserializer.readString();
        WsId wsId = null;
        try {
            wsId = new WsId(s);
        } catch (Exception e) {
            throw new SerializationException("Error parsing wsid", e);
        }
        return wsId;
    }

    @Override
    public void serialize(ISerializationContext serializer, WsId wsId)
            throws SerializationException {
        serializer.writeString(wsId.toString());
    }

}
