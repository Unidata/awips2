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
 * Dynamic serialize adapter for EnumSet.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 16, 2009           rjpeter     Initial creation
 * Dec 02, 2013  2537     bsteffen    Serialize empty enum sets.
 * 
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

        String enumClassName = deserializer.readString();
        try {
            Class baseClass = Class.forName(enumClassName);
            if (baseClass.isEnum()) {
                rval = EnumSet.noneOf(baseClass);
                for (int i = 0; i < setSize; i++) {
                    rval.add(Enum.valueOf(baseClass, deserializer.readString()));
                }
            } else {
                throw new SerializationException(
                        "Cannot deserialize EnumSet.  Class [" + enumClassName
                                + "] is not an enum");
            }
        } catch (ClassNotFoundException e) {
            throw new SerializationException("Unable to find enum class ["
                    + enumClassName + "]", e);
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
        } else {
            /*
             * Due to generic type erasure there is no simple way to access the
             * enum type from the enum set. To work around this limitation get
             * the complement of the empty set which will contain all the values
             * and use an arbitrary example Object to access the enum class.
             */ 
            Object example = EnumSet.complementOf(set).iterator().next();
            serializer.writeString(example.getClass().getName());
        }
    }
}
