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

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.domain.api.ITimePoint;

/**
 * {@link ISerializationTypeAdapter} for {@link ITimePoint} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class ITimePointTypeAdapter extends XmlAdapter<Date, ITimePoint>
        implements ISerializationTypeAdapter<ITimePoint> {
    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer, ITimePoint object)
            throws SerializationException {
        serializer.writeI64(object.asMilliseconds());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ITimePoint deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return TimePoints.fromMillis(deserializer.readI64());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ITimePoint unmarshal(Date v) throws Exception {
        return TimePoints.fromDate(v);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Date marshal(ITimePoint v) throws Exception {
        return v.asDate();
    }
}
