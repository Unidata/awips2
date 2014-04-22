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
import java.util.List;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Export information about all the resources in display pane containers.
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
public class DisplayedResourcesExporter extends DisplayPaneContainerExporter {

    private final List<SmartInstance> resources = new ArrayList<SmartInstance>();

    public DisplayedResourcesExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected String getFileName() {
        return "displayedResources.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains descriptors and resources of all the display pane containers\\n");
        comment.append("# (SideViews VizMapEditors, and VizXyEditors)\n");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for DisplayPaneContainer resources...";
    }

    @Override
    protected void outputPaneManager(SmartInstance paneManager)
            throws IOException {
        List<SmartInstance> displayPanes = paneManager.get("displayPanes")
                .toArrayList();
        for (SmartInstance dispalyPane : displayPanes) {
            SmartInstance renderableDisplay = dispalyPane
                    .get("renderableDisplay");
            try {
                String scale = renderableDisplay.getString("scaleName");
                println("  scaleName = " + scale);
            } catch (IllegalStateException e) {
                /* Not D2D or after 14.2 */
            }
            try {
                String scale = renderableDisplay.getString("scale");
                println("  scale = " + scale);
            } catch (IllegalStateException e) {
                /* Not D2D or before 14.2 */
            }
            SmartInstance descriptor = renderableDisplay.get("descriptor");
            SmartInstance resourceList = descriptor.get("resourceList");
            SmartInstance[] array = resourceList.getObjectArray("array");
            println("  " + descriptor + "{");
            for (SmartInstance resourcePair : array) {
                SmartInstance resource = resourcePair.get("resource");
                if (resource == null) {
                    SmartInstance resourceData = resourcePair
                            .get("resourceData");
                    if (resourceData == null) {
                        println("    Empty Resource Pair");
                    } else {
                        println("    " + resourceData + "(resource was null)");
                    }
                } else {
                    println("    " + resource);
                    resources.add(resource);
                }
            }
            println("  }");
        }
    }

    public List<SmartInstance> getResources() {
        return resources;
    }

}
