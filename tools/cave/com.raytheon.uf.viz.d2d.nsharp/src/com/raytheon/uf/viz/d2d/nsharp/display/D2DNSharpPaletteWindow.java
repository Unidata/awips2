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
package com.raytheon.uf.viz.d2d.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaletteWindow;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.raytheon.viz.ui.tools.AbstractModalTool;
import com.raytheon.viz.ui.tools.ModalToolManager;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class D2DNSharpPaletteWindow extends NsharpPaletteWindow {

    private static final String EDIT_TOOL_CATEGY = "com.raytheon.viz.ui.modalTool.nav";

    private class EditTool extends AbstractModalTool {

        public EditTool() {
            this.categoryId = EDIT_TOOL_CATEGY;
        }

        @Override
        protected void deactivateTool() {
            if (!graphEditBtn.isDisposed()) {
                editGraphOn = false;
                graphEditBtn.setText(EDIT_GRAPH_OFF);
                notifyRsc();
            }

        }

        @Override
        protected void activateTool() {
            editGraphOn = true;
            graphEditBtn.setText(EDIT_GRAPH_ON);
            notifyRsc();
        }

        private void notifyRsc() {
            NsharpSkewTResource rsc = getSkewTRsc();
            if (rsc == null)
                return;

            rsc.setEditGraphOn(editGraphOn);
            rsc.issueRefresh();
        }

    }

    private AbstractModalTool editTool = new EditTool();

    private AbstractModalTool lastTool = null;

    @Override
    public void createDataControlGp(Composite parent) {
        super.createDataControlGp(parent);
        unloadBtn.dispose();
        // remove any other listeners.
        for (Listener listener : loadBtn.getListeners(SWT.MouseUp)) {
            loadBtn.removeListener(SWT.MouseUp, listener);
        }
        loadBtn.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {
                D2DNsharpHandleArchiveFile.openArchiveFile(getViewSite()
                        .getShell());
            }
        });

        for (Listener listener : graphEditBtn.getListeners(SWT.MouseUp)) {
            graphEditBtn.removeListener(SWT.MouseUp, listener);
        }
        graphEditBtn.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {
                if (editGraphOn) {
                    disableEdit();
                } else {
                    enableEdit();
                }
            }
        });
        NsharpSkewTResource rsc = getSkewTRsc();
        if (rsc != null && rsc.isEditGraphOn()) {
            enableEdit();
        } else {
            disableEdit();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaletteWindow#dispose()
     */
    @Override
    public void dispose() {
        disableEdit();
        getSite().getPage().removePartListener(this);
    }

    private void enableEdit() {
        AbstractVizPerspectiveManager perspMgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (perspMgr == null) {
            return;
        }
        ModalToolManager mgr = perspMgr.getToolManager();
        lastTool = mgr.getSelectedModalTool(EDIT_TOOL_CATEGY);
        if (lastTool != editTool) {
            mgr.selectModalTool(editTool);
            editTool.activate();
        } else {
            lastTool = null;
        }
    }

    private void disableEdit() {
        AbstractVizPerspectiveManager perspMgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (perspMgr == null) {
            return;
        }
        ModalToolManager mgr = perspMgr.getToolManager();
        if (mgr.getSelectedModalTool(EDIT_TOOL_CATEGY) == editTool) {
            mgr.deselectModalTool(editTool);
            if (lastTool != null) {
                mgr.selectModalTool(lastTool);
                lastTool.activate();
                lastTool = null;
            }
        }
    }

}
