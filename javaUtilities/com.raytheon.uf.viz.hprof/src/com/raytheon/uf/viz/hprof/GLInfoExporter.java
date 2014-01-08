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
package com.raytheon.uf.viz.hprof;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.Id;
import com.raytheon.hprof.SmartInstance;
import com.raytheon.hprof.data.HeapDumpRecord;
import com.raytheon.hprof.data.heap.BasicType;
import com.raytheon.hprof.data.heap.dump.ClassDump;

/**
 * 
 * Export information about openGl memory useage.
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
public class GLInfoExporter extends AbstractExporter {

    public GLInfoExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected String getFileName() {
        return "glInfo.txt";
    }

    @Override
    protected String getComment() {
        return "# This file contains information about possible openGL memory usage.";
    }

    @Override
    protected String getInfo() {
        return "Generating output for GL...";
    }

    @Override
    protected void exportInternal() throws IOException {
        Id classId = hprof
                .lookUpClass("com.raytheon.viz.core.gl.internal.cache.ImageCache");
        HeapDumpRecord heapDump = hprof.getHeapDump();
        ClassDump classDump = heapDump.getClassDump(classId);
        SmartInstance textureCache = null;
        SmartInstance memoryCache = null;
        for (Entry<Id, BasicType> staticEntry : classDump.getStaticFields()
                .entrySet()) {
            String name = hprof.getString(staticEntry.getKey());
            if ("textureCache".equals(name)) {
                textureCache = new SmartInstance(hprof,
                        heapDump.getInstance(staticEntry.getValue()
                                .getObjectId()));
            } else if ("memoryCache".equals(name)) {
                memoryCache = new SmartInstance(hprof,
                        heapDump.getInstance(staticEntry.getValue()
                                .getObjectId()));
            }
        }
        long memUsed = memoryCache.getLong("curSize");
        long memMax = memoryCache.getLong("maxSize");
        long texUsed = textureCache.getLong("curSize");
        long texMax = textureCache.getLong("maxSize");
        println("RAM texture cache using " + (memUsed / 1024 / 1024) + "MB of "
                + (memMax / 1024 / 1024) + "MB");
        println("GL texture cache using " + (texUsed / 1024 / 1024) + "MB of "
                + (texMax / 1024 / 1024) + "MB");
        List<SmartInstance> geoms = getInstances("com.raytheon.viz.core.gl.GLGeometryObject2D");
        int coords = 0;
        for (SmartInstance geom : geoms) {
            SmartInstance lengthBuffer = geom.get("compiledLengths");
            if (lengthBuffer != null) {
                int[] lengthArr = lengthBuffer.getIntArray("hb");
                for (int length : lengthArr) {
                    coords += length;
                }
            }
        }
        println("GL vbo size(estimate) is " + (coords * 2 * 4 / 1024 / 1024)
                + "MB");
    }

}
