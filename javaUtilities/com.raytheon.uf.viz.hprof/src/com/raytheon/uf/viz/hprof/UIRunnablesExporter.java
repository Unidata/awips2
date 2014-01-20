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
 * Export information about the runnables waiting to execute on the UI Thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 20, 2014  2648     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class UIRunnablesExporter extends AbstractExporter {

    public UIRunnablesExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected String getFileName() {
        return "uiRunnables.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains runnables waiting for access to the SWT UI Thread. These are\n");
        comment.append("# often scheduled through VizApp.runAsync.");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for UI Runnables...";
    }

    @Override
    protected void exportInternal() throws IOException {
        List<SmartInstance> displays = getInstances("org.eclipse.swt.widgets.Display");
        if (displays.isEmpty()) {
            return;
        }
        println(displays.size() + " dispaly(s)");
        for (SmartInstance display : displays) {
            SmartInstance[] messages = display.get("synchronizer")
                    .getObjectArray("messages");
            println(messages.length + " message(s)");
            for (SmartInstance message : messages) {
                println(message.get("runnable").toString());
            }

        }
    }

}
