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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Export information about D2DGridResources, mostly analyze memory usage of raw
 * data.
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
public class D2DGridResourceExporter extends RequestableResourceExporter {

    private static final boolean useDataMap = false;

    public D2DGridResourceExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory, null);
    }

    @Override
    protected String getFileName() {
        return "d2dGridResources.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains information about D2DGridResources, there are 4 sections:\n");
        comment.append("#  1) Metadata maps for each resource. \n");
        comment.append("#  2) Size and type of all GeneralGridData. \n");
        comment.append("#  3) Total size of each resource. \n");
        comment.append("#  4) Total size of all resources.\n#");

        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for D2DGridResource...";
    }

    @Override
    protected void exportInternal() throws IOException {
        List<SmartInstance> resources = getInstances("com.raytheon.viz.grid.rsc.general.D2DGridResource");
        if (resources.isEmpty()) {
            return;
        }
        println("# Section 1 metadata maps for each resource.");
        for (SmartInstance resource : resources) {
            outputResource(resource);
        }
        println("# Section 2 size and type of all GeneralGridData.");
        Map<SmartInstance, Integer> sizes = new HashMap<SmartInstance, Integer>();
        for (SmartInstance resource : resources) {
            int floats = 0;
            List<GeneralGridDataInstance> datas = new ArrayList<GeneralGridDataInstance>();
            if (useDataMap) {
                ConcurrentHashMap<SmartInstance, SmartInstance> dataMap = resource
                        .get("dataMap").toConcurrentHashMap();
                for (Entry<SmartInstance, SmartInstance> dataEntry : dataMap
                        .entrySet()) {
                    List<SmartInstance> dataList = dataEntry.getValue()
                            .toArrayList();
                    for (SmartInstance gridData : dataList) {
                        datas.add(new GeneralGridDataInstance(gridData));
                    }
                }
            } else {
                ArrayList<SmartInstance> requests = resource.get("requestJob")
                        .get("requests").toArrayList();
                for (SmartInstance request : requests) {
                    SmartInstance data = request.get("gridData");
                    if (data != null) {
                        List<SmartInstance> dataList = data.toArrayList();
                        for (SmartInstance gridData : dataList) {
                            datas.add(new GeneralGridDataInstance(gridData));
                        }
                    }
                }
            }
            if (datas.isEmpty()) {
                continue;
            }
            println(resource + "{");

            Map<GeneralGridDataInstance, Integer> redundantCounts = new HashMap<GeneralGridDataInstance, Integer>();

            for (GeneralGridDataInstance gridDataInst : datas) {
                int count = 0;
                if (redundantCounts.containsKey(gridDataInst)) {
                    count = redundantCounts.get(gridDataInst);
                }
                count += 1;
                redundantCounts.put(gridDataInst, count);
                floats += gridDataInst.getFloatCount();
            }
            for (Entry<GeneralGridDataInstance, Integer> entry : redundantCounts
                    .entrySet()) {
                sizes.put(resource, floats);
                println("  " + entry.getValue()
                        + " instances of GeneralGridData like {");
                println(entry.getKey().toString());
                println("  }");
            }
            println("}");
        }
        println("# Section 3 total size of each resource.");
        List<Entry<SmartInstance, Integer>> sizesList = new ArrayList<Entry<SmartInstance, Integer>>(
                sizes.entrySet());
        Collections.sort(sizesList,
                new Comparator<Entry<SmartInstance, Integer>>() {

                    @Override
                    public int compare(Entry<SmartInstance, Integer> e1,
                            Entry<SmartInstance, Integer> e2) {
                        return e1.getValue().compareTo(e2.getValue());
                    }
                });
        int totalFloats = 0;
        for (Entry<SmartInstance, Integer> entry : sizesList) {
            SmartInstance resource = entry.getKey();
            StringBuilder modHint = new StringBuilder();
            try {
                boolean dataModified = resource.getBoolean("dataModified");
                modHint.append("(dataModified=").append(dataModified)
                        .append(")");
            } catch (IllegalStateException e) {
                /* heap dump is from before 14.2 */
            }
            try {
                boolean reprojectedData = resource
                        .getBoolean("reprojectedData");
                modHint.append("(reprojectedData=").append(reprojectedData)
                        .append(")");
            } catch (IllegalStateException e) {
                /* heap dump is from after 14.2 */
            }
            resource.getBoolean("reprojectedData");
            int floats = entry.getValue();
            int size = floats * 4 / 1024;
            String suffix = "KB";
            if (size > 1024) {
                size /= 1024;
                suffix = "MB";
            }
            println(resource.toString() + modHint.toString() + " uses is "
                    + size + suffix);
            totalFloats += floats;
        }
        println("# Section 4 total size of all resources.");
        println("Total memory usage for " + resources.size()
                + " resources  is " + totalFloats * 4 / 1024 / 1024 + "MB");
    }

    private static class GeneralGridDataInstance {

        private final int width;

        private final int height;

        /* number of floats */
        private final int scalarCapacity;

        /* number of floats */
        private final int dirCapacity;

        /* number of floats */
        private final int uCapacity;

        /* number of floats */
        private final int vCapacity;

        public GeneralGridDataInstance(SmartInstance generalGridData) {
            SmartInstance gridGeometry = generalGridData.get("gridGeometry");
            SmartInstance gridRange = gridGeometry.get("gridRange");
            int[] index = gridRange.getIntArray("index");
            width = index[2] - index[0];
            height = index[3] - index[1];

            SmartInstance buffer = generalGridData.get("scalarData");
            scalarCapacity = getCapacity(buffer);

            buffer = generalGridData.get("direction");
            dirCapacity = getCapacity(buffer);

            buffer = generalGridData.get("uComponent");
            uCapacity = getCapacity(buffer);

            buffer = generalGridData.get("vComponent");
            vCapacity = getCapacity(buffer);

        }

        private static int getCapacity(SmartInstance buffer) {
            if (buffer == null) {
                return 0;
            }
            try {
                return buffer.getInt("capacity");
            } catch (IllegalStateException e) {
                return buffer.get("buffer").getInt("capacity");
            }
        }

        public int getFloatCount() {
            return uCapacity + vCapacity + scalarCapacity + dirCapacity;
        }

        @Override
        public String toString() {
            StringBuilder str = new StringBuilder();
            str.append("    dimensions are " + width + "x" + height + "\n");
            if (scalarCapacity != 0) {
                str.append("    scalarData contains " + scalarCapacity
                        + " floats.\n");
            }
            if (dirCapacity != 0) {
                str.append("    direction contains " + dirCapacity
                        + " floats.\n");
            }
            if (uCapacity != 0) {
                str.append("    uComponent contains " + uCapacity
                        + " floats.\n");
            }
            if (vCapacity != 0) {
                str.append("    vComponent contains " + vCapacity
                        + " floats.\n");
            }
            str.append("    memory usage is " + getFloatCount() * 4 / 1024
                    + "KB");
            return str.toString();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + dirCapacity;
            result = prime * result + height;
            result = prime * result + scalarCapacity;
            result = prime * result + uCapacity;
            result = prime * result + vCapacity;
            result = prime * result + width;
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            GeneralGridDataInstance other = (GeneralGridDataInstance) obj;
            if (dirCapacity != other.dirCapacity)
                return false;
            if (height != other.height)
                return false;
            if (scalarCapacity != other.scalarCapacity)
                return false;
            if (uCapacity != other.uCapacity)
                return false;
            if (vCapacity != other.vCapacity)
                return false;
            if (width != other.width)
                return false;
            return true;
        }

    }

}
