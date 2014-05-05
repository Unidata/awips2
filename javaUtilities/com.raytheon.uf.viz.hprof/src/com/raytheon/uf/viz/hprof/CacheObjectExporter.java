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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Export information about cache objects.
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
public class CacheObjectExporter extends RequestableResourceExporter {

    public CacheObjectExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory, null);
    }

    @Override
    protected String getFileName() {
        return "cacheObjects.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains information about cache objects(used mostly by radar).\n");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for CacheObject...";
    }

    @Override
    protected void exportInternal() throws IOException {
        List<SmartInstance> cacheObjects = getInstances("com.raytheon.uf.viz.core.cache.CacheObject");
        if (cacheObjects.isEmpty()) {
            return;
        }
        Map<String, List<Integer>> byType = new HashMap<String, List<Integer>>();
        for(SmartInstance cacheObject : cacheObjects){
            String className = cacheObject.get("retriever").getClassName();
            List<Integer> sizes = byType.get(className);
            if (sizes == null) {
                sizes = new ArrayList<Integer>();
                byType.put(className, sizes);
            }
            sizes.add(cacheObject.getInt("size"));
        }
        for (Entry<String, List<Integer>> entry : byType.entrySet()) {
            println(entry.getKey() + "{");
            println("  total objects = " + entry.getValue().size());
            Map<Integer, Integer> countBySize = new HashMap<Integer, Integer>();
            long total = 0;
            for (Integer size : entry.getValue()) {
                total += size;
                Integer count = countBySize.get(size);
                if (count == null) {
                    count = 0;
                }
                count = count + 1;
                countBySize.put(size, count);
            }
            for (Entry<Integer, Integer> cse : countBySize.entrySet()) {
                println("  objects of size " + cse.getKey() + "  = "
                        + cse.getValue());

            }
            println("  total size(bytes) = " + (total / 1024 / 1024) + "MB");
            println("}");
        }
    }

}
