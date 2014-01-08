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
package com.raytheon.hprof.data.heap.dump;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.hprof.BigByteBuffer;
import com.raytheon.hprof.Id;
import com.raytheon.hprof.data.HeapDumpRecord;
import com.raytheon.hprof.data.heap.BasicType;

/**
 * 
 * Class data for a single class in a {@link HeapDumpRecord}. This class
 * contains most of the same information as a {@link Class} object, in a much
 * more cryptic form.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 08, 2014  2648     bsteffen    Initial doc
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ClassDump extends AbstractDump {

    public static final int TAG = 0x20;

    private final Id superId;

    private final int instanceSize;

    private final BasicType[] constantPool;

    private final Id[] staticFieldIds;

    private final BasicType[] staticFieldValues;

    private final Id[] instanceFieldIds;

    private final byte[] instanceFieldTypes;

    public ClassDump(BigByteBuffer buffer, int idSize) {
        super(buffer, idSize);
        superId = new Id(buffer, idSize);
        /* Id classLoaderId = */new Id(buffer, idSize);
        /* Id signerId = */new Id(buffer, idSize);
        /* Id protectionDomainId = */new Id(buffer, idSize);
        /* Id reservedId1 = */new Id(buffer, idSize);
        /* Id reservedId2 = */new Id(buffer, idSize);
        instanceSize = buffer.getInt();
        short constantPoolSize = buffer.getShort();
        constantPool = new BasicType[constantPoolSize];
        for (int i = 0; i < constantPoolSize; i++) {
            /* short constantPoolIndex = */buffer.getShort();
            constantPool[i] = new BasicType(buffer, idSize);
        }
        short numberOfStaticFields = buffer.getShort();
        staticFieldIds = new Id[numberOfStaticFields];
        staticFieldValues = new BasicType[numberOfStaticFields];
        for (int i = 0; i < numberOfStaticFields; i++) {
            staticFieldIds[i] = new Id(buffer, idSize);
            staticFieldValues[i] = new BasicType(buffer, idSize);
        }
        short numberOfInstanceFields = buffer.getShort();
        instanceFieldIds = new Id[numberOfInstanceFields];
        instanceFieldTypes = new byte[numberOfInstanceFields];
        for (int i = 0; i < numberOfInstanceFields; i++) {
            instanceFieldIds[i] = new Id(buffer, idSize);
            instanceFieldTypes[i] = buffer.get();
        }
    }

    public Id getSuperId() {
        return superId;
    }

    public int getInstanceSize() {
        return instanceSize;
    }

    public Map<Id, BasicType> parseInstanceData(BigByteBuffer buffer, int idSize) {
        Map<Id, BasicType> results = new HashMap<Id, BasicType>();
        for (int i = 0; i < instanceFieldIds.length; i++) {
            BasicType type = new BasicType(buffer, instanceFieldTypes[i],
                    idSize);
            results.put(instanceFieldIds[i], type);
        }
        return results;
    }
    
    public Map<Id, BasicType> getStaticFields(){
        Map<Id, BasicType> results = new HashMap<Id, BasicType>(instanceFieldIds.length * 2);
        for(int i = 0 ; i < staticFieldIds.length ; i += 1){
            results.put(staticFieldIds[i], staticFieldValues[i]);
        }
        return results;
    }

}
