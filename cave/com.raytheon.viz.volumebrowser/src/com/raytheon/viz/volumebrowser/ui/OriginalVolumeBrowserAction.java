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

package com.raytheon.viz.volumebrowser.ui;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.jobs.RequestJob;
import com.raytheon.viz.volumebrowser.Activator;

/**
 * Run from the volume browser.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 11/20/2006                   brockwoo    Initial Creation.
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
public class OriginalVolumeBrowserAction extends AbstractHandler {

    protected IMapDescriptor theDescriptor;

    protected AbstractEditor theEditor;

    protected IGraphicsTarget theTarget;

    // private String referenceTime;

    private RequestJob[] jobList;

    // private void createPlot(String refTime) {
    // theEditor = (AbstractEditor) VizApp.getCurrentEditor();
    // theTarget = theEditor.getActiveDisplayPane().getTarget();
    // theDescriptor = (IMapDescriptor) theEditor.getActiveDisplayPane()
    // .getRenderableDisplay();
    // // referenceTime = refTime;
    // Job j = new Job("Add Layer") {
    // /*
    // * (non-Javadoc)
    // *
    // * @see
    // * org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime
    // * .IProgressMonitor)
    // */
    // @Override
    // protected IStatus run(IProgressMonitor aMonitor) {
    // aMonitor.beginTask("Loading layer...", 1);
    // try {
    // AbstractVizResource plot = new PlotResource();
    // theDescriptor.getResourceList().add(plot);
    // } catch (WrongProjectionException e) {
    // return new Status(Status.ERROR, Activator.PLUGIN_ID,
    // Status.ERROR, "Projection does not match:: "
    // + e.getMessage(), e);
    // } catch (VizException e) {
    // return new Status(Status.ERROR, Activator.PLUGIN_ID,
    // Status.ERROR, "Error loading layer", e);
    // }
    // aMonitor.done();
    // VizApp.runSync(new Runnable() {
    // public void run() {
    // theEditor.refresh();
    // }
    // });
    // return Status.OK_STATUS;
    // }
    // };
    // j.setRule(new RequestJobSchedulingRule());
    // j.schedule();
    // }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = new Shell(SWT.DIALOG_TRIM); // Win32
        VolumeBrowserDialog id = null;
        try {
            id = new VolumeBrowserDialog(shell, "Volume Browser");

            if (id.open() == VolumeBrowserDialog.OK) {
                ArrayList<AbstractRequestableResourceData> entries = id
                        .getLayers();
                ArrayList<RequestJob.Request> imageLayers = new ArrayList<RequestJob.Request>();

                // Get the editor type for the data to load
                Map<String, String> editorConfig = id.getEditorConfig();

                // ArrayList<LayerProperty> contourLayers = new
                // ArrayList<LayerProperty>();
                // ArrayList<AbstractRequestableResourceData> plotLayers = new
                // ArrayList<AbstractRequestableResourceData>();
                jobList = new RequestJob[entries.size()];
                // Sort the various layers into their basic types
                for (int i = 0; i < entries.size(); i++) {
                    AbstractRequestableResourceData entry = entries.get(i);
                    RequestJob.Request r = new RequestJob.Request();
                    r.loadProperties = new LoadProperties();
                    r.resourceData = entry;
                    imageLayers.add(r);
                    // if (entry.getDesiredProduct().matches("Image")) {
                    // imageLayers.add(entry);
                    // } else if (entry.getDesiredProduct().matches("Contour"))
                    // {
                    // contourLayers.add(entry);
                    // } else if (entry.getDesiredProduct().matches("Plot")) {
                    // plotLayers.add(entry);
                    // }
                }
                // Now, schedule the layers per type with images first, then
                // contour, then plot
                int layerCounter = 0;
                // for (LayerProperty imageLayer : imageLayers) {
                // // String script =
                // // ScriptCreator.createImageScript(imageLayer);
                // jobList[layerCounter] = new RequestJob(imageLayer, 60000);
                // jobList[layerCounter].schedule();
                // layerCounter++;
                // }

                AbstractEditor editor;
                if (EditorUtil.getActiveEditor() != null
                        && EditorUtil.getActiveEditor().getClass().getName()
                                .equals(editorConfig.get("editor"))) {
                    editor = ((AbstractEditor) EditorUtil.getActiveEditor());
                } else {
                    EditorInput cont = new EditorInput(
                            (IRenderableDisplay) Class.forName(
                                    editorConfig.get("editorInput"))
                                    .newInstance());

                    editor = (AbstractEditor) PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .openEditor(cont, editorConfig.get("editor"), false);
                }

                jobList[layerCounter] = new RequestJob(60000, editor,
                        imageLayers.toArray(new RequestJob.Request[imageLayers
                                .size()]));
                jobList[layerCounter].schedule();
                layerCounter++;
                // for (LayerProperty plotLayer : plotLayers) {
                // createPlot(plotLayer.getSelectedEntryTime()[0].toString());
                // layerCounter++;
                // }
            }
        } catch (Exception e) {
            e.printStackTrace();
            Status status = new Status(Status.ERROR, Activator.PLUGIN_ID, 0,
                    "Error occurred during volume browser action.", e);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR", "Error occurred during volume browser action",
                    status);
            throw new ExecutionException(
                    "Error occurred volume browser action", e);
        }
        return null;
    }
}
