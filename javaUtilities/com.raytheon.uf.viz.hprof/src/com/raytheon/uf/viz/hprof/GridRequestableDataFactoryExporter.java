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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Export information about the cached grid data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 20, 2014  3093     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridRequestableDataFactoryExporter extends
        RequestableResourceExporter {

    public GridRequestableDataFactoryExporter(HprofFile hprof,
            File outputDirectory) {
        super(hprof, outputDirectory, null);
    }

    @Override
    protected String getFileName() {
        return "gridRequestableDataFactory.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains information about GridRequestableDataFactory\n");
        comment.append("# which caches grids used in derived parameters using soft references.\n");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for GridRequestableDataFactory...";
    }

    @Override
    protected void exportInternal() throws IOException {
        List<SmartInstance> factories = getInstances("com.raytheon.viz.grid.data.GridRequestableDataFactory");
        if (factories.isEmpty()) {
            return;
        }
        long totalSize = 0;
        for (SmartInstance factory : factories) {
            Map<String, SmartInstance> dataMap = factory.get(
                    "requestableDataMap").toStringKeyedConcurrentHashMap();
            for (Entry<String, SmartInstance> entry : dataMap.entrySet()) {
                SmartInstance data = entry.getValue();
                println(data + "{");
                println("  dataURI = " + entry.getKey());
                long size = 0;
                HashMap<SmartInstance, SmartInstance> cache = data.get("cache")
                        .get("m").toHashMap();
                for (SmartInstance ref : cache.values()) {
                    SmartInstance[] recArray = ref.getObjectArray("referent");
                    if (recArray != null) {
                        for (SmartInstance rec : recArray) {
                            float[] rawFloats = rec.getFloatArray("floatData");
                            size += rawFloats.length * 4;
                        }
                    }
                }
                totalSize += size;
                printKB("  Soft Referenced Memory = ", size);
                println("}");
            }

        }
        if (totalSize > 0) {
            printMB("Total Soft Referenced Memory = ", totalSize);
        }
    }

}
