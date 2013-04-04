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
package com.raytheon.uf.common.time.domain;

import java.util.concurrent.TimeUnit;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.domain.api.IDuration;

/**
 * {@link ISerializationTypeAdapter} for {@link IDuration} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class IDurationTypeAdapter extends XmlAdapter<String, IDuration>
        implements
        ISerializationTypeAdapter<IDuration> {

    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer, IDuration object)
            throws SerializationException {
        serializer.writeI64(object.getNanos());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IDuration deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return Durations.of(deserializer.readI64(), TimeUnit.NANOSECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IDuration unmarshal(String v) throws Exception {
        return Durations.fromString(v);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String marshal(IDuration v) throws Exception {
        return Durations.toString(v);
    }

}
