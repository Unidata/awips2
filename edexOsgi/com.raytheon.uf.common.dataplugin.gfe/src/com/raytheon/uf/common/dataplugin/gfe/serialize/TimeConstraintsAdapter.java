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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Thrift serialization adapter for TimeConstraints
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2013     #1774  randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TimeConstraintsAdapter implements
        ISerializationTypeAdapter<TimeConstraints> {

    @Override
    public void serialize(ISerializationContext serializer,
            TimeConstraints object) throws SerializationException {
        serializer.writeI32(object.getDuration());
        serializer.writeI32(object.getRepeatInterval());
        serializer.writeI32(object.getStartTime());
    }

    @Override
    public TimeConstraints deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return new TimeConstraints(deserializer.readI32(),
                deserializer.readI32(), deserializer.readI32());
    }
}
