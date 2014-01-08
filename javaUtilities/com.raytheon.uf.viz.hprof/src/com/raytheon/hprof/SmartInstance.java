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
package com.raytheon.hprof;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.hprof.data.HeapDumpRecord;
import com.raytheon.hprof.data.heap.BasicType;
import com.raytheon.hprof.data.heap.dump.ClassDump;
import com.raytheon.hprof.data.heap.dump.InstanceDump;
import com.raytheon.hprof.data.heap.dump.ObjectArrayDump;

/**
 * Represents an instance of an object within a heap dump. This is an enhanced
 * version of an {@link InstanceDump} that uses all the information in an
 * {@link HprofFile} to provide useful introspection.
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
public class SmartInstance {

    private final HprofFile hprof;

    private final Id id;

    private final String className;

    private final Map<String, BasicType> fields = new HashMap<String, BasicType>();

    public SmartInstance(HprofFile hprof, InstanceDump instance) {
        this.hprof = hprof;
        this.id = instance.getId();
        Id classId = instance.getClassId();
        className = hprof.getClassName(classId);
        int idSize = classId.size();
        HeapDumpRecord heapDump = hprof.getHeapDump();
        ClassDump classDump = heapDump.getClassDump(classId);
        BigByteBuffer instanceData = instance.getInstanceData();
        while (classDump != null) {
            Map<Id, BasicType> id2type = classDump.parseInstanceData(
                    instanceData, idSize);
            for (Entry<Id, BasicType> entry : id2type.entrySet()) {
                String fieldName = hprof.getString(entry.getKey());
                fields.put(fieldName, entry.getValue());
            }
            classDump = heapDump.getClassDump(classDump.getSuperId());
        }

    }

    protected Id getId(String fieldName) {
        BasicType type = fields.get(fieldName);
        if (type == null) {
            return null;
        }
        return type.getObjectId();
    }

    protected String getClassName() {
        return className;
    }

    public SmartInstance get(String fieldName) {
        Id fieldId = getId(fieldName);
        if (fieldId == null) {
            return null;
        }
        InstanceDump field = hprof.getHeapDump().getInstance(fieldId);
        if (field == null) {
            return null;
        }
        return new SmartInstance(hprof, field);
    }

    private BasicType getTypeNotNull(String fieldName) {
        BasicType type = fields.get(fieldName);
        if (type == null) {
            throw new IllegalStateException(fieldName
                    + " is not a a valid field");
        }
        return type;
    }

    public int getInt(String fieldName) {
        return getTypeNotNull(fieldName).getInt();
    }

    public long getLong(String fieldName) {
        return getTypeNotNull(fieldName).getLong();
    }

    public SmartInstance[] getObjectArray(String fieldName) {
        BasicType type = fields.get(fieldName);
        if (type == null) {
            return null;
        }
        HeapDumpRecord heapDump = hprof.getHeapDump();
        Id fieldId = type.getObjectId();
        ObjectArrayDump objectArray = heapDump.getObjectArray(fieldId);
        if (objectArray == null) {
            return null;
        }
        Id[] idArray = objectArray.getArray();
        SmartInstance[] smartArray = new SmartInstance[idArray.length];
        for (int i = 0; i < idArray.length; i += 1) {
            InstanceDump instance = heapDump.getInstance(idArray[i]);
            if (instance != null) {
                smartArray[i] = new SmartInstance(hprof, instance);
            }
        }
        return smartArray;
    }

    public char[] getCharArray(String fieldName) {
        BasicType type = fields.get(fieldName);
        if (type == null) {
            return null;
        }
        Id fieldId = type.getObjectId();
        BasicType[] primArray = hprof.getHeapDump().getPrimitiveArray(fieldId);
        if (primArray == null) {
            return null;
        }
        char[] charArray = new char[primArray.length];

        for (int i = 0; i < charArray.length; i += 1) {
            charArray[i] = primArray[i].getChar();
        }
        return charArray;
    }

    public int[] getIntArray(String fieldName) {
        BasicType type = fields.get(fieldName);
        if (type == null) {
            return null;
        }
        Id fieldId = type.getObjectId();
        BasicType[] primArray = hprof.getHeapDump().getPrimitiveArray(fieldId);
        if (primArray == null) {
            return null;
        }
        int[] intArray = new int[primArray.length];

        for (int i = 0; i < intArray.length; i += 1) {
            intArray[i] = primArray[i].getInt();
        }
        return intArray;
    }

    public String getString(String fieldName) {
        Id fieldId = getId(fieldName);
        if (fieldId == null) {
            return null;
        }
        InstanceDump field = hprof.getHeapDump().getInstance(fieldId);
        if (field != null) {
            SmartInstance smartField = new SmartInstance(hprof, field);
            if (!smartField.getClassName().equals(String.class.getName())) {
                throw new IllegalStateException(fieldName + " is not a String.");
            }
            return smartField.toString();
        } else {
            Iterator<ClassDump> classDumps = hprof.getHeapDump()
                    .getClassDumps();
            while (classDumps.hasNext()) {
                ClassDump classDump = classDumps.next();
                for (Entry<Id, BasicType> entry : classDump.getStaticFields()
                        .entrySet()) {
                    BasicType f = entry.getValue();
                    if (f.isObject() && f.getObjectId().equals(fieldId)) {
                        String className = hprof
                                .getClassName(classDump.getId());
                        return "{@link " + shortenClass(className) + "#"
                                + hprof.getString(entry.getKey()) + "}";
                    }
                }
            }
            return "String (" + fieldId + ")";
        }
    }

    public ArrayList<SmartInstance> toArrayList() {
        if (!getClassName().equals(ArrayList.class.getName())) {
            throw new IllegalStateException(this + " is not an ArrayList.");
        }
        int size = getInt("size");
        SmartInstance[] elementData = getObjectArray("elementData");
        ArrayList<SmartInstance> arrayList = new ArrayList<SmartInstance>(size);
        for (int i = 0; i < size; i += 1) {
            arrayList.add(elementData[i]);
        }
        return arrayList;
    }

    public ConcurrentHashMap<SmartInstance, SmartInstance> toConcurrentHashMap() {
        if (!getClassName().equals(ConcurrentHashMap.class.getName())) {
            throw new IllegalStateException(this
                    + " is not a ConcurrentHashMap.");
        }
        ConcurrentHashMap<SmartInstance, SmartInstance> map = new ConcurrentHashMap<SmartInstance, SmartInstance>();
        SmartInstance[] segments = getObjectArray("segments");
        for (SmartInstance segment : segments) {
            SmartInstance[] table = segment.getObjectArray("table");
            for (SmartInstance entry : table) {
                while (entry != null) {
                    map.put(entry.get("key"), entry.get("value"));
                    entry = entry.get("next");
                }
            }
        }
        return map;
    }

    public HashMap<SmartInstance, SmartInstance> toHashMap() {
        if (!getClassName().equals(HashMap.class.getName())) {
            throw new IllegalStateException(this + " is not a HashMap.");
        }
        HashMap<SmartInstance, SmartInstance> map = new HashMap<SmartInstance, SmartInstance>();
        SmartInstance[] table = getObjectArray("table");
        for (SmartInstance entry : table) {
            while (entry != null) {
                map.put(entry.get("key"), entry.get("value"));
                entry = entry.get("next");
            }
        }
        return map;
    }

    public HashMap<String, SmartInstance> toStringKeyedHashMap() {
        if (!getClassName().equals(HashMap.class.getName())) {
            throw new IllegalStateException(this + " is not a HashMap.");
        }
        HashMap<String, SmartInstance> map = new HashMap<String, SmartInstance>();
        SmartInstance[] table = getObjectArray("table");
        for (SmartInstance entry : table) {
            while (entry != null) {
                map.put(entry.getString("key"), entry.get("value"));
                entry = entry.get("next");
            }
        }
        return map;
    }

    public String toString() {
        if (className.equals(String.class.getName())) {
            return "\"" + new String(getCharArray("value")) + "\"";
        }
        return shortenClass(className) + " (" + id + ")";
    }

    private String shortenClass(String className) {
        int index = className.lastIndexOf('.');
        return className.substring(index + 1);
    }

}
