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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.hprof.data.HeapDumpEndRecord;
import com.raytheon.hprof.data.HeapDumpRecord;
import com.raytheon.hprof.data.HeapDumpSegmentRecord;
import com.raytheon.hprof.data.LoadClassRecord;
import com.raytheon.hprof.data.MergedHeapDumpSegmentRecord;
import com.raytheon.hprof.data.StackFrameRecord;
import com.raytheon.hprof.data.StackTraceRecord;
import com.raytheon.hprof.data.StringRecord;
import com.raytheon.hprof.data.heap.dump.InstanceDump;

/**
 * 
 * Root object for an hprof file, description of the data structure can be found
 * in a java installation at java/demo/jvmti/hprof/src/manual.html.
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
public class HprofFile {

    private HeapDumpRecord heapDump;

    private List<StringRecord> strings = new ArrayList<StringRecord>();

    private List<LoadClassRecord> loadClasses = new ArrayList<LoadClassRecord>();

    /* If these ever get full and cause problems they should be switched to LRU. */
    private Map<Id, String> usedStrings = new HashMap<Id, String>();

    private Map<Id, String> usedClassnames = new HashMap<Id, String>();

    public HprofFile(String fileName) throws IOException {
        BigByteBuffer buffer = new BigByteBuffer(fileName);

        String format = null;

        while (buffer.hasRemaining()) {
            if (buffer.get() == 0x00) {
                byte[] formatBytes = new byte[(int) buffer.position() - 1];
                buffer.rewind();
                /* reread the string */
                buffer.get(formatBytes);
                /* reread the null */
                buffer.get();
                format = new String(formatBytes);
                break;
            }
        }

        if (format == null) {
            throw new IllegalArgumentException(
                    "This does not appear to be a valid hprof file!");
        }
        System.out.println("Hprof format is " + format);
        int idSize = buffer.getInt();
        System.out.println("Dump is from " + new Date(buffer.getLong()));
        
        long segmentStart = 0;

        while (buffer.hasRemaining()) {
            byte type = buffer.get();
            switch (type & 0xFF) {
            case StringRecord.TAG: {
                strings.add(new StringRecord(buffer, idSize));
                break;
            }
            case LoadClassRecord.TAG: {
                loadClasses.add(new LoadClassRecord(buffer, idSize));
                break;
            }
            case StackFrameRecord.TAG: {
                new StackFrameRecord(buffer, idSize);
                break;
            }
            case StackTraceRecord.TAG: {
                new StackTraceRecord(buffer, idSize);
                break;
            }
            case HeapDumpRecord.TAG: {
                heapDump = new HeapDumpRecord(buffer, idSize);
                break;
            }
            case HeapDumpSegmentRecord.TAG: {
                if (segmentStart == 0) {
                    segmentStart = buffer.position() - 1;
                }
                new HeapDumpSegmentRecord(buffer, idSize);
                break;
            }
            case HeapDumpEndRecord.TAG: {
                long segmentEnd = buffer.position() - 1;
                buffer.position(segmentStart);
                BigByteBuffer safeBuffer = buffer.slice();
                safeBuffer.limit(segmentEnd - segmentStart);
                heapDump = new MergedHeapDumpSegmentRecord(safeBuffer, idSize);
                segmentStart = 0;
                buffer.position(segmentEnd + 1);
                new HeapDumpEndRecord(buffer, idSize);
                break;
            }
            default:
                throw new IllegalStateException("Unknown type "
                        + Integer.toHexString(type & 0xFF));
            }
        }
    }

    public Id lookUpClass(String className) {
        Id classNameId = getStringId(className.replace(".", "/"));
        for (LoadClassRecord loadClass : loadClasses) {
            if (loadClass.getClassNameId().equals(classNameId)) {
                return loadClass.getClassId();
            }
        }
        return null;
    }

    public HeapDumpRecord getHeapDump() {
        return heapDump;
    }

    public String getString(Id stringId) {
        if (usedStrings.containsKey(stringId)) {
            return usedStrings.get(stringId);
        }
        for (StringRecord string : strings) {
            if (string.getId().equals(stringId)) {
                usedStrings.put(stringId, string.getString());
                return string.getString();
            }
        }
        return null;
    }

    public Id getStringId(String string) {
        for (Entry<Id, String> entry : usedStrings.entrySet()) {
            if (entry.getValue().equals(string)) {
                return entry.getKey();
            }
        }
        for (StringRecord stringRec : strings) {
            if (stringRec.getString().equals(string)) {
                usedStrings.put(stringRec.getId(), stringRec.getString());
                return stringRec.getId();
            }
        }
        return null;
    }

    public String getClassName(Id classId) {
        if (usedClassnames.containsKey(classId)) {
            return usedClassnames.get(classId);
        }
        for (LoadClassRecord loadClass : loadClasses) {
            if (loadClass.getClassId().equals(classId)) {
                String className = getString(loadClass.getClassNameId())
                        .replace("/", ".");
                usedClassnames.put(classId, className);
                return className;
            }
        }
        return null;
    }

    public List<InstanceDump> getInstances(String className) {
        Id classId = lookUpClass(className);
        if (classId == null) {
            return Collections.emptyList();
        }
        return heapDump.getInstances(classId);

    }

}
