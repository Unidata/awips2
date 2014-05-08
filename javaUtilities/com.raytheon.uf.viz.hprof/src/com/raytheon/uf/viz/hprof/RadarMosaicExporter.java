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

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Export information about radar mosaic resources including the resources that
 * are mosaiced and memory information about each resource for finding memory
 * leaks.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 05, 2014  3093     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarMosaicExporter extends RequestableResourceExporter {

    public RadarMosaicExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory, null);
    }

    @Override
    protected String getFileName() {
        return "radarMosaicResources.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains information about RadarMosaicResources, there are 4 sections:\n");
        comment.append("#  1) Metadata maps for each resource. \n");
        comment.append("#  2) Metadata maps for each resource within each mosaic. \n");
        comment.append("#  3) Total size of each resource. \n");
        comment.append("#  4) Total size of all mosaics.\n");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for RadarMosaics...";
    }

    @Override
    protected void exportInternal() throws IOException {
        List<SmartInstance> resources = getInstances("com.raytheon.viz.radar.rsc.mosaic.RadarMosaicResource");
        if (resources.isEmpty()) {
            return;
        }
        println("# Section 1 metadata maps for each resource.");
        for (SmartInstance resource : resources) {
            outputResource(resource);
        }
        println("# Section 2 metadata maps for each resource within each mosaic.");
        for (SmartInstance resource : resources) {
            println(resource + "{");
            SmartInstance[] subrscs = resource.get("resourceData")
                    .get("resourceList").getObjectArray("array");
            println("  Resources to mosaic = " + subrscs.length);
            for (SmartInstance subrsc : subrscs) {
                outputResource(subrsc.get("resource"), "  ");
            }
            println("}");
        }
        println("# Section 3 total size of each resource.");
        long totalSize = 0;
        for (SmartInstance resource : resources) {
            println(resource + "{");
            SmartInstance[] subrscs = resource.get("resourceData")
                    .get("resourceList").getObjectArray("array");
            long rscSize = 0;
            for (SmartInstance subrsc : subrscs) {
                subrsc = subrsc.get("resource");
                if (subrsc == null) {
                    continue;
                }
                println("  " + subrsc + "{");
                HashMap<SmartInstance, SmartInstance> recordMap = subrsc
                        .get("radarRecords").get("m").toHashMap();
                println("    Total radar records = " + recordMap.size());
                long subrscSize = 0;
                for (SmartInstance record : recordMap.values()) {
                    subrscSize += record.get("cacheObject").getInt("size");
                }
                println("    Mosaiced resource memory used = "
                        + (subrscSize / 1024) + "KB");
                rscSize += subrscSize;
                println("  }");
            }
            println("  Mosaic memory used = " + (rscSize / 1024 / 1024) + "MB");
            totalSize += rscSize;
            println("}");
        }
        println("# Section 4 total size of all mosaics.");
        println("Total memory used = " + (totalSize / 1024 / 1024) + "MB");

    }


}
