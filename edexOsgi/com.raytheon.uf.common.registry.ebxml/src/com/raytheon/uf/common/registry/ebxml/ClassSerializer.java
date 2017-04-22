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
package com.raytheon.uf.common.registry.ebxml;

import com.raytheon.uf.common.serialization.BuiltInTypeSupport;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * Serializes a {@link Class} variable. Cannot be added to
 * {@link BuiltInTypeSupport} because all variables end up resolving to this
 * serializer through some manner or another when added there.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1195     djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ClassSerializer implements ISerializationTypeAdapter<Class<?>> {

    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer, Class<?> object)
            throws SerializationException {
        serializer.writeString(object.getName());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<?> deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return ReflectionUtil.forName(deserializer.readString());
    }

}
