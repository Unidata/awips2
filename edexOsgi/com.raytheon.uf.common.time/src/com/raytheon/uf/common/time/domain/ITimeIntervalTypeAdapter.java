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

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.domain.api.ITimeInterval;
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
public class ITimeIntervalTypeAdapter extends
        XmlAdapter<TimeIntervalJaxbable, ITimeInterval> implements
        ISerializationTypeAdapter<ITimeInterval> {
    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer, ITimeInterval object)
            throws SerializationException {
        serializer.writeObject(object.getStart());
        serializer.writeObject(object.getEnd());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ITimeInterval deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        ITimePoint start = (ITimePoint) deserializer.readObject();
        ITimePoint end = (ITimePoint) deserializer.readObject();

        return TimeIntervals.fromTimePoints(start, end);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ITimeInterval unmarshal(TimeIntervalJaxbable v)
            throws Exception {
        return TimeIntervals.fromTimePoints(TimePoints.fromDate(v.getStart()),
                TimePoints.fromDate(v.getEnd()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TimeIntervalJaxbable marshal(ITimeInterval v) throws Exception {
        TimeIntervalJaxbable jaxbable = new TimeIntervalJaxbable();
        jaxbable.setStart(v.getStart().asDate());
        jaxbable.setEnd(v.getEnd().asDate());

        return jaxbable;
    }
}
