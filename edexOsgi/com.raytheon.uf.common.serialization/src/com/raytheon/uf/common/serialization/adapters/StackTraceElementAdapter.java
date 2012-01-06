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

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * The adapter used to serialize StackTraceElement arrays for transporting
 * Throwable objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class StackTraceElementAdapter implements
        ISerializationTypeAdapter<StackTraceElement> {

    @Override
    public StackTraceElement deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return new StackTraceElement(deserializer.readString(),
                deserializer.readString(), deserializer.readString(),
                deserializer.readI32());
    }

    @Override
    public void serialize(ISerializationContext serializer,
            StackTraceElement object) throws SerializationException {
        serializer.writeString(object.getClassName() == null ? "" : object
                .getClassName());
        serializer.writeString(object.getMethodName() == null ? "" : object
                .getMethodName());
        serializer.writeString(object.getFileName() == null ? "" : object
                .getFileName());
        serializer.writeI32(object.getLineNumber());
    }

}
