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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
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
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DatabaseIDAdapter extends XmlAdapter<String, DatabaseID> implements
        ISerializationTypeAdapter<DatabaseID> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
     * (com.raytheon.uf.common.serialization.ISerializationContext,
     * java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer, DatabaseID dbId)
            throws SerializationException {
        serializer.writeString(dbId.toString());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#deserialize
     * (com.raytheon.uf.common.serialization.IDeserializationContext)
     */
    @Override
    public DatabaseID deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return new DatabaseID(deserializer.readString());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.xml.bind.annotation.adapters.XmlAdapter#unmarshal(java.lang.Object)
     */
    @Override
    public DatabaseID unmarshal(String v) throws Exception {
        return new DatabaseID(v);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.xml.bind.annotation.adapters.XmlAdapter#marshal(java.lang.Object)
     */
    @Override
    public String marshal(DatabaseID v) throws Exception {
        return v.toString();
    }

}
