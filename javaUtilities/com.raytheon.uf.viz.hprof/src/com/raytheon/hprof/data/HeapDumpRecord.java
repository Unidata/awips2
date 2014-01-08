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
package com.raytheon.hprof.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.hprof.BigByteBuffer;
import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.Id;
import com.raytheon.hprof.data.heap.BasicType;
import com.raytheon.hprof.data.heap.dump.ClassDump;
import com.raytheon.hprof.data.heap.dump.InstanceDump;
import com.raytheon.hprof.data.heap.dump.ObjectArrayDump;
import com.raytheon.hprof.data.heap.dump.PrimitiveArrayDump;
import com.raytheon.hprof.data.heap.root.AbstractRoot;
import com.raytheon.hprof.data.heap.root.RootJNIGlobal;
import com.raytheon.hprof.data.heap.root.RootJNILocal;
import com.raytheon.hprof.data.heap.root.RootJavaFrame;
import com.raytheon.hprof.data.heap.root.RootMonitorUsed;
import com.raytheon.hprof.data.heap.root.RootStickyClass;
import com.raytheon.hprof.data.heap.root.RootThreadObject;

/**
 * 
 * Heap dump Record within an {@link HprofFile}. This object holds most of the
 * interesting infromation in a hprof file.
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
public class HeapDumpRecord extends AbstractHprofRecord {

    public static final int TAG = 0x0c;

    protected int idSize;

    protected BigByteBuffer buffer;

    protected List<AbstractRoot> roots;

    protected long[] instancesById;

    protected long[] instancesByClass;

    protected long[] objectArraysById;

    protected long[] primitiveArraysById;

    protected long[] classesById;

    protected Map<Id, ClassDump> usedClasses;


    public HeapDumpRecord(BigByteBuffer buffer, int idSize) {
        super(buffer, idSize);
    }

    @Override
    protected void init(BigByteBuffer buffer, int idSize) {
        this.buffer = buffer;
        this.idSize = idSize;
        this.roots = new ArrayList<AbstractRoot>();
        this.usedClasses = new HashMap<Id, ClassDump>();
        List<Long> instances = new ArrayList<Long>();
        List<Long> objectArrays = new ArrayList<Long>();
        List<Long> primitiveArrays = new ArrayList<Long>();
        List<Long> classes = new ArrayList<Long>();
        while (buffer.hasRemaining()) {
            byte type = buffer.get();
            switch (type & 0xFF) {
            case RootJNIGlobal.TAG: {
                roots.add(new RootJNIGlobal(buffer, idSize));
                break;
            }
            case RootJNILocal.TAG: {
                roots.add(new RootJNILocal(buffer, idSize));
                break;
            }
            case RootJavaFrame.TAG: {
                roots.add(new RootJavaFrame(buffer, idSize));
                break;
            }
            case RootStickyClass.TAG: {
                roots.add(new RootStickyClass(buffer, idSize));
                break;
            }
            case RootMonitorUsed.TAG: {
                roots.add(new RootMonitorUsed(buffer, idSize));
                break;
            }
            case RootThreadObject.TAG: {
                roots.add(new RootThreadObject(buffer, idSize));
                break;
            }
            case ClassDump.TAG: {
                classes.add(buffer.position());
                new ClassDump(buffer, idSize);
                break;
            }
            case InstanceDump.TAG: {
                instances.add(buffer.position());
                new InstanceDump(buffer, idSize);
                break;
            }
            case ObjectArrayDump.TAG: {
                objectArrays.add(buffer.position());
                new ObjectArrayDump(buffer, idSize);
                break;
            }
            case PrimitiveArrayDump.TAG: {
                primitiveArrays.add(buffer.position());
                new PrimitiveArrayDump(buffer, idSize);
                break;
            }
            case HeapDumpSegmentRecord.TAG: {
                buffer.position(buffer.position() + 8);
                break;
            }
            default:
                throw new IllegalStateException("Unknown heap type "
                        + Integer.toHexString(type & 0xFF));
            }
        }

        System.out.println("Sorting all objects...");
        long startTime = System.currentTimeMillis();

        Collections.sort(instances, new DumpBufferIndexComparator());
        Collections.sort(objectArrays, new DumpBufferIndexComparator());
        Collections.sort(primitiveArrays, new DumpBufferIndexComparator());
        Collections.sort(classes, new DumpBufferIndexComparator());

        this.instancesById = new long[instances.size()];
        for (int i = 0; i < instances.size(); i++) {
            this.instancesById[i] = instances.get(i);
        }

        this.objectArraysById = new long[objectArrays.size()];
        for (int i = 0; i < objectArrays.size(); i++) {
            this.objectArraysById[i] = objectArrays.get(i);
        }

        this.primitiveArraysById = new long[primitiveArrays.size()];
        for (int i = 0; i < primitiveArrays.size(); i++) {
            this.primitiveArraysById[i] = primitiveArrays.get(i);
        }

        this.classesById = new long[classes.size()];
        for (int i = 0; i < classes.size(); i++) {
            this.classesById[i] = classes.get(i);
        }

        /* Add one id and one int before the class id */
        Collections.sort(instances, new DumpBufferIndexComparator(idSize + 4));

        this.instancesByClass = new long[instances.size()];
        for (int i = 0; i < instances.size(); i++) {
            this.instancesByClass[i] = instances.get(i);
        }

        System.out.println("Spent " + (System.currentTimeMillis() - startTime)
                / 1000 + " seconds sorting");
    }

    public ClassDump getClassDump(Id classId) {
        if (usedClasses.containsKey(classId)) {
            return usedClasses.get(classId);
        }
        long index = binarySearch(classesById, classId);
        if (index < 0) {
            return null;
        }
        buffer.position(index);
        ClassDump classDump = new ClassDump(buffer, idSize);
        usedClasses.put(classId, classDump);
        return classDump;
    }

    public InstanceDump getInstance(Id objectId) {
        long index = binarySearch(instancesById, objectId);
        if (index < 0) {
            return null;
        }
        buffer.position(index);
        return new InstanceDump(buffer, idSize);
    }

    public ObjectArrayDump getObjectArray(Id objectId) {
        long index = binarySearch(objectArraysById, objectId);
        if (index < 0) {
            return null;
        }
        buffer.position(index);
        return new ObjectArrayDump(buffer, idSize);
    }

    public BasicType[] getPrimitiveArray(Id objectId) {
        long index = binarySearch(primitiveArraysById, objectId);
        if (index < 0) {
            return null;
        }
        buffer.position(index);
        return new PrimitiveArrayDump(buffer, idSize).getArray();
    }

    private long binarySearch(long[] array, Id objectId) {
        int low = 0;
        int high = array.length - 1;
        while (low <= high) {
            int mid = (low + high) / 2;
            buffer.position(array[mid]);
            Id id = new Id(buffer, idSize);

            int compval = id.compareTo(objectId);
            if (compval < 0) {
                low = mid + 1;
            } else if (compval > 0) {
                high = mid - 1;
            } else {
                return array[mid];
            }
        }
        return -1;
    }

    public List<InstanceDump> getInstances(Id classId) {
        int low = 0;
        int high = instancesByClass.length - 1;
        int mid = (low + high) / 2;
        while (low <= high) {
            mid = (low + high) / 2;
            buffer.position(instancesByClass[mid] + idSize + 4);
            Id id = new Id(buffer, idSize);

            int compval = id.compareTo(classId);
            if (compval < 0) {
                low = mid + 1;
            } else if (compval > 0) {
                high = mid - 1;
            } else {
                break;
            }
        }
        if (low > high) {
            return Collections.emptyList();
        }
        List<InstanceDump> instances = new ArrayList<InstanceDump>();

        int next = mid;
        buffer.position(instancesByClass[next--]);
        InstanceDump instance = new InstanceDump(buffer, idSize);
        while (instance.getClassId().equals(classId) && next >= 0) {
            instances.add(instance);
            buffer.position(instancesByClass[next--]);
            instance = new InstanceDump(buffer, idSize);
        }
        next = mid + 1;
        buffer.position(instancesByClass[next++]);
        instance = new InstanceDump(buffer, idSize);
        while (instance.getClassId().equals(classId)
                && next < instancesByClass.length) {
            instances.add(instance);
            buffer.position(instancesByClass[next++]);
            instance = new InstanceDump(buffer, idSize);
        }
        return instances;
    }

    public Map<Id, BasicType> getInstanceFields(InstanceDump instance) {
        Map<Id, BasicType> id2type = new HashMap<Id, BasicType>();
        ClassDump classDump = getClassDump(instance.getClassId());
        BigByteBuffer instanceData = instance.getInstanceData();
        while (classDump != null) {
            id2type.putAll(classDump.parseInstanceData(instanceData, idSize));
            classDump = getClassDump(classDump.getSuperId());
        }
        return id2type;
    }

    public Iterator<ClassDump> getClassDumps() {
        return new Iterator<ClassDump>() {

            private int index;

            @Override
            public boolean hasNext() {
                return index < classesById.length;
            }

            @Override
            public ClassDump next() {
                buffer.position(classesById[index]);
                index += 1;
                return new ClassDump(buffer, idSize);
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }

        };
    }

    protected class DumpBufferIndexComparator implements Comparator<Long> {

        private final int indexOffset;

        protected DumpBufferIndexComparator() {
            this(0);
        }

        protected DumpBufferIndexComparator(int indexOffset) {
            this.indexOffset = indexOffset;
        }

        @Override
        public int compare(Long o1, Long o2) {
            try {
                HeapDumpRecord.this.buffer.position(o1 + indexOffset);
                Id i1 = new Id(HeapDumpRecord.this.buffer,
                        HeapDumpRecord.this.idSize);
                HeapDumpRecord.this.buffer.position(o2 + indexOffset);
                Id i2 = new Id(HeapDumpRecord.this.buffer,
                        HeapDumpRecord.this.idSize);
                return i1.compareTo(i2);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    };

}
