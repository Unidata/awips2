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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.raytheon.viz.ui.tools.AbstractModalTool;
import com.raytheon.viz.ui.tools.ModalToolManager;

import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPaletteWindow;

/**
 * 
 * Extends NsharpPaletteWindow but overide load to prevent opening ncmapeditor.
 * Also disable unload since loading and unloading is being handled by time
 * matched resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------------
 * May 12, 2011  9249     bsteffen  Initial creation
 * Apr 29, 2016  5607     bsteffen  Fix modal tool manipulation in eclipse 4.
 * May 05, 2017           mjames    Use NCEP NSHARP dialog.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class D2DNSharpPaletteWindow extends NsharpPaletteWindow {

    private static final String EDIT_TOOL_CATEGY = "com.raytheon.viz.ui.modalTool.nav";

    private AbstractModalTool lastTool = null;
    
    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    
    D2DNsharpLoadDialog loadDia = D2DNsharpLoadDialog.getInstance(shell);

    @Override
    public void init(IViewSite site) {
        super.init(site);
        AbstractVizPerspectiveManager perspMgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (perspMgr == null) {
            return;
        }
        ModalToolManager mgr = perspMgr.getToolManager();
        lastTool = mgr.getSelectedModalTool(EDIT_TOOL_CATEGY);
        if (lastTool != null) {
            mgr.deselectModalTool(lastTool);
            lastTool.deactivate();
        }
    }

    @Override
    public void dispose() {
        super.dispose();
        AbstractVizPerspectiveManager perspMgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (perspMgr == null) {
            return;
        }
        ModalToolManager mgr = perspMgr.getToolManager();
        if (lastTool != null
                && mgr.getSelectedModalTool(EDIT_TOOL_CATEGY) == null) {
            lastTool.setEditor(EditorUtil.getActiveVizContainer());
            mgr.selectModalTool(lastTool);
            lastTool.activate();
            lastTool = null;
        }
    }

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
            	if (loadDia != null) loadDia.open();
            }
        });

    }

}
