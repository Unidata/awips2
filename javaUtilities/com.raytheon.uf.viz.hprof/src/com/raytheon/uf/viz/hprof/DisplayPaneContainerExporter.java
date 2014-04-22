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
 * Base class for exporters that want to display information about things in
 * DispalyPaneContainers/PaneManagers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 8, 2014           bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class DisplayPaneContainerExporter extends AbstractExporter {

    public DisplayPaneContainerExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected void exportInternal() throws IOException {
        try {
            outputVizMultiPaneEditors("com.raytheon.uf.viz.core.maps.display.VizMapEditor");
        } catch (Throwable e) {
            e.printStackTrace();
        }
        try {
            outputVizMultiPaneEditors("com.raytheon.uf.viz.xy.VizXyEditor");
        } catch (Throwable e) {
            e.printStackTrace();
        }
        try {
            outputSideViews();
        } catch (Throwable e) {
            e.printStackTrace();
        }

    }

    protected void outputVizMultiPaneEditors(String className)
            throws IOException {
        List<SmartInstance> editors = getInstances(className);
        for (SmartInstance editor : editors) {
            outputVizMultiPaneEditor(editor);
        }
    }

    protected void outputSideViews() throws IOException {
        List<SmartInstance> views = getInstances("com.raytheon.uf.viz.d2d.ui.map.SideView");
        for (SmartInstance view : views) {
            outputSideView(view);
        }
    }

    protected void outputVizMultiPaneEditor(SmartInstance editor)
            throws IOException {
        SmartInstance paneManager = editor.get("editorInput")
                .get("paneManager");

        println(editor + "{");
        outputPaneManager(paneManager);
        println("}");
    }

    protected void outputSideView(SmartInstance view) throws IOException {
        SmartInstance paneManager = view.get("paneManager");

        println(view + "{");
        outputPaneManager(paneManager);
        println("}");
    }

    protected abstract void outputPaneManager(SmartInstance paneManager)
            throws IOException;

}
