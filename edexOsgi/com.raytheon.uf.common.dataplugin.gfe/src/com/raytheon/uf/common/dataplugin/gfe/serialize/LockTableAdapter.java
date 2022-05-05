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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * SerializeAdapter for LockTable. Strips out parmId and common WsIds to reduce
 * serialization size and reflection calls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 02, 2013	1949        rjpeter     Initial creation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class LockTableAdapter implements ISerializationTypeAdapter<LockTable> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
     * (com.raytheon.uf.common.serialization.ISerializationContext,
     * java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer, LockTable lockTable)
            throws SerializationException {

        // get all the unique WsIds
        int index = 0;
        // use linked hash so wsIds stay in order
        LinkedHashMap<WsId, Integer> wsIds = new LinkedHashMap<WsId, Integer>();
        wsIds.put(lockTable.getWsId(), index++);
        List<Lock> locks = lockTable.getLocks();
        WsId curWsId = null;
        for (Lock lock : locks) {
            curWsId = lock.getWsId();
            if (!wsIds.containsKey(curWsId)) {
                wsIds.put(curWsId, index++);
            }
        }

        // write the parm
        serializer.writeObject(lockTable.getParmId());

        // write the unique wsIds
        serializer.writeI32(index);
        for (WsId id : wsIds.keySet()) {
            serializer.writeObject(id);
        }

        serializer.writeI32(locks.size());
        for (Lock lock : locks) {
            serializer.writeI64(lock.getStartTime());
            serializer.writeI64(lock.getEndTime());
            serializer.writeI32(wsIds.get(lock.getWsId()));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#deserialize
     * (com.raytheon.uf.common.serialization.IDeserializationContext)
     */
    @Override
    public LockTable deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        ParmID parmId = (ParmID) deserializer.readObject();
        int numWsIds = deserializer.readI32();
        WsId[] ids = new WsId[numWsIds];
        for (int i = 0; i < numWsIds; i++) {
            ids[i] = (WsId) deserializer.readObject();
        }
        int numLocks = deserializer.readI32();
        List<Lock> locks = new ArrayList<Lock>(numLocks);
        long startDate = 0;
        long endDate = 0;
        int wsIdIndex = 0;
        for (int i = 0; i < numLocks; i++) {
            startDate = deserializer.readI64();
            endDate = deserializer.readI64();
            wsIdIndex = deserializer.readI32();
            locks.add(new Lock(parmId, ids[wsIdIndex], startDate, endDate));
        }

        return new LockTable(parmId, locks, ids[0]);
    }
}
