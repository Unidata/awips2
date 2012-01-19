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
package com.raytheon.uf.common.time.adapter;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Serializes a timerange in a binary format (as two longs)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 7, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TimeRangeTypeAdapter implements
        ISerializationTypeAdapter<TimeRange> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.esb.serialize.ISerializationFactory#serialize(com.raytheon
     * .edex.esb.serialize.ISerializer, java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer, TimeRange object)
            throws SerializationException {
        serializer.writeI64(object.getStart().getTime());
        serializer.writeI64(object.getEnd().getTime());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.esb.serialize.ISerializationFactory#deserialize(com
     * .raytheon.edex.esb.serialize.ISerializer)
     */
    @Override
    public TimeRange deserialize(IDeserializationContext serializer)
            throws SerializationException {
        long t1 = serializer.readI64();
        long t2 = serializer.readI64();

        return new TimeRange(t1, t2);
    }

}
