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

    private final boolean useDataMap = false;

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
        if (useDataMap) {
            for (SmartInstance resource : resources) {
                int floats = 0;
                ConcurrentHashMap<SmartInstance, SmartInstance> dataMap = resource
                        .get("dataMap").toConcurrentHashMap();
                if (!dataMap.isEmpty()) {
                    println(resource + "{");
                    for (Entry<SmartInstance, SmartInstance> dataEntry : dataMap
                            .entrySet()) {
                        List<SmartInstance> dataList = dataEntry.getValue()
                                .toArrayList();
                        for (SmartInstance gridData : dataList) {
                            floats += outputGeneralGridData(gridData);
                        }
                    }
                    println("}");
                }
                sizes.put(resource, floats);
            }
        } else {
            for (SmartInstance resource : resources) {
                int floats = 0;
                ArrayList<SmartInstance> requests = resource.get("requestJob")
                        .get("requests").toArrayList();
                if (!requests.isEmpty()) {
                    println(resource + "{");
                    for (SmartInstance request : requests) {
                        SmartInstance data = request.get("gridData");
                        if (data != null) {
                            List<SmartInstance> dataList = data.toArrayList();
                            for (SmartInstance gridData : dataList) {
                                floats += outputGeneralGridData(gridData);
                            }
                        }
                    }
                    println("}");
                }
                sizes.put(resource, floats);
            }
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
            int floats = entry.getValue();
            int size = floats * 4 / 1024;
            String suffix = "KB";
            if (size > 1024) {
                size /= 1024;
                suffix = "MB";
            }
            println(resource + " uses is " + size + suffix);
            totalFloats += floats;
        }
        println("# Section 4 total size of all resources.");
        println("Total memory usage for " + resources.size()
                + " resources  is " + totalFloats * 4 / 1024 / 1024 + "MB");
    }

    protected int outputGeneralGridData(SmartInstance generalGridData)
            throws IOException {
        println("  " + generalGridData + "{");
        SmartInstance gridGeometry = generalGridData.get("gridGeometry");
        SmartInstance gridRange = gridGeometry.get("gridRange");
        int[] index = gridRange.getIntArray("index");
        int width = index[2] - index[0];
        int height = index[3] - index[1];
        println("    dimensions are " + width + "x" + height + "");

        int floats = 0;
        SmartInstance buffer = generalGridData.get("scalarData");
        if (buffer != null) {
            int capacity = buffer.getInt("capacity");
            println("    scalarData contains " + capacity + " floats.");
            floats += capacity;
        }
        buffer = generalGridData.get("direction");
        if (buffer != null) {
            int capacity = buffer.getInt("capacity");
            println("    direction contains " + capacity + " floats.");
            floats += capacity;
        }
        buffer = generalGridData.get("uComponent");
        if (buffer != null) {
            int capacity = buffer.getInt("capacity");
            println("    uComponent contains " + capacity + " floats.");
            floats += capacity;
        }
        buffer = generalGridData.get("vComponent");
        if (buffer != null) {
            int capacity = buffer.getInt("capacity");
            println("    vComponent contains " + capacity + " floats.");
            floats += capacity;
        }
        println("    memory usage is " + floats * 4 / 1024 + "KB");
        println("  }");
        return floats;
    }

}
