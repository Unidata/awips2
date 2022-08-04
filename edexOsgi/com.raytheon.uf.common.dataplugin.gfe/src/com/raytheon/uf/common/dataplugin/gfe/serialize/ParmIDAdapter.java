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
package com.raytheon.uf.common.dataplugin.gfe.serialize;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * String adapter for ParmID
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 26, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ParmIDAdapter extends XmlAdapter<String, ParmID> implements
        ISerializationTypeAdapter<ParmID> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
     * (com.raytheon.uf.common.serialization.ISerializationContext,
     * java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer, ParmID parmId)
            throws SerializationException {
        serializer.writeString(parmId.toString());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#deserialize
     * (com.raytheon.uf.common.serialization.IDeserializationContext)
     */
    @Override
    public ParmID deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return new ParmID(deserializer.readString());
    }

    @Override
    public String marshal(ParmID v) throws Exception {
        return v.toString();
    }

    @Override
    public ParmID unmarshal(String v) throws Exception {
        return new ParmID(v);
    }

}
