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
package com.raytheon.uf.common.serialization.adapters;

import java.util.EnumSet;
import java.util.Iterator;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Dynamic serialize adapter for EnumSet. Due to the restrictions on EnumSet, an
 * empty EnumSet will be deserialized as null.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2009            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@SuppressWarnings("unchecked")
public class EnumSetAdapter implements ISerializationTypeAdapter<EnumSet> {

    @Override
    public EnumSet deserialize(IDeserializationContext deserializer)
            throws SerializationException {

        int setSize = deserializer.readI32();
        EnumSet rval = null;

        if (setSize > 0) {
            String enumClassName = deserializer.readString();

            try {
                Class baseClass = Class.forName(enumClassName);
                if (baseClass.isEnum()) {
                    Enum firstEnum = Enum.valueOf(baseClass, deserializer
                            .readString());
                    rval = EnumSet.of(firstEnum);
                    for (int i = 1; i < setSize; i++) {
                        rval.add(Enum.valueOf(baseClass, deserializer
                                .readString()));
                    }
                } else {
                    throw new SerializationException(
                            "Cannot deserialize EnumSet.  Class ["
                                    + enumClassName + "] is not an enum");
                }
            } catch (ClassNotFoundException e) {
                throw new SerializationException("Unable to find enum class ["
                        + enumClassName + "]", e);
            }
        }

        return rval;
    }

    @Override
    public void serialize(ISerializationContext serializer, EnumSet set)
            throws SerializationException {
        int setSize = set.size();
        serializer.writeI32(set.size());
        if (setSize > 0) {
            Iterator iter = set.iterator();
            Enum firstEnum = (Enum) iter.next();
            serializer.writeString(firstEnum.getClass().getName());
            serializer.writeString(firstEnum.name());

            while (iter.hasNext()) {
                serializer.writeString(((Enum) iter.next()).name());
            }
        }
    }
}
