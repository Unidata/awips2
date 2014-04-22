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

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Outputs all input handlers for all pane managers. The most useful thing this
 * can be used for is checking if pan tool is active.
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
public class InputHandlerExporter extends
        DisplayPaneContainerExporter {

    public InputHandlerExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected String getFileName() {
        return "inputHandlers.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains inputHandlers of all the display pane containers\n");
        comment.append("# (SideViews VizMapEditors, and VizXyEditors). The editor with the most\n");
        comment.append("# handlers is probably the active editor.\n");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for DisplayPaneContainer input handlers...";
    }

    @Override
    protected void outputPaneManager(
            SmartInstance paneManager) throws IOException {
        List<SmartInstance> handlers = paneManager.get("inputManager")
                .get("handlers").toArrayList();

        if (!handlers.isEmpty()) {
            println("  InputManager{");
            for (SmartInstance phandler : handlers) {
                SmartInstance handler = phandler.get("handler");
                println("    " + handler);
            }
            println("  }");
        }
    }
}
