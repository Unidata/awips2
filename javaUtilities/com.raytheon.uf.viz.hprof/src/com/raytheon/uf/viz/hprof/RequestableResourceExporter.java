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
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Outputs metadata map for all requestable resource datas.
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
public class RequestableResourceExporter extends AbstractExporter {

    private final List<SmartInstance> resources;

    public RequestableResourceExporter(HprofFile hprof, File outputDirectory,
            List<SmartInstance> resources) {
        super(hprof, outputDirectory);
        this.resources = resources;
    }

    @Override
    protected String getFileName() {
        return "requestableResources.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment
                .append("# This file contains the metadataMap for all resources that are on containers found in\n");
        comment
                .append("# displayPaneContainers.txt. Keys and values which are interned constants are not always\n");
        comment.append("# saved to the heap and might not output pretty.");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for RequestableResources...";
    }

    @Override
    protected void exportInternal() throws IOException {
        for (SmartInstance resource : resources) {
            outputResource(resource);
        }
    }

    protected void outputResource(SmartInstance resource)
            throws IOException {
        if (resource == null) {
            return;
        }
        SmartInstance resourceData = resource.get("resourceData");
        if (resourceData == null) {
            println(resource + "{");
            println("  No resourceData available.");
            println("}");
            return;
        }
        SmartInstance metadataMap = resourceData.get(
                "metadataMap");
        if (metadataMap == null) {
            return;
        }
        println(resource + "{");
        outputMetadataMap(metadataMap);
        println("}");
    }

    protected void outputMetadataMap(SmartInstance metadataMap)
            throws IOException {
        for (Entry<String, SmartInstance> entry : metadataMap
                .toStringKeyedHashMap().entrySet()) {
            String key = entry.getKey();
            SmartInstance value = entry.getValue();
            SmartInstance constraintType = value.get("constraintType");
            int ordinal = constraintType.getInt("ordinal");
            String constraintValue = value.getString("constraintValue");
            String operand = "";
            switch (ordinal) {
            case 0:
                operand = "=";
                break;
            case 1:
                operand = "!=";
                break;
            case 2:
                operand = ">";
                break;
            case 3:
                operand = ">=";
                break;
            case 4:
                operand = "<";
                break;
            case 5:
                operand = "<=";
                break;
            case 6:
                operand = "between";
                break;
            case 7:
                operand = "in";
                break;
            case 8:
                operand = "like";
                break;
            case 9:
                operand = "ilike";
                break;
            case 10:
                operand = "isnull";
                break;
            case 11:
                operand = "isnotnull";
                break;
            }
            println("  " + key + " " + operand + " " + constraintValue);
        }
    }

}
