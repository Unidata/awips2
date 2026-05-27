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
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.d2d.nsharp.tool.D2DNsharpLoadDialog;
import com.raytheon.viz.ui.EditorUtil;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPaletteWindow;

/**
 * 
 * Extends NsharpPaletteWindow and overide load action for NSharp Sounding
 * Load Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#   Engineer  Description
 * ------------- --------  --------- ------------------------------------------
 * Apr 03,2020   73172     smanoj    Initial creation
 * Jun 25,2020   80230     smanoj    Fixing some errors and enhancements.
 * Nov 20,2020   85104     smanoj    Close and clear the NSHARP related artifacts
 *                                   (green diamonds and NSharp Editor graph) 
 *                                   when closing NSHARP palette.
 *
 * </pre>
 * 
 * @author smanoj
 */
public class D2DNSharpToolPaletteWindow extends NsharpPaletteWindow {

    private static final String MAP_EDITOR_ID = "com.raytheon.viz.ui.glmap.GLMapEditor";

    private static final String NSHARP_MAP_EDITOR_ID = "gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor";

    @Override
    public void init(IViewSite site) {
        super.init(site);

    }

    @Override
    public void dispose() {
        super.dispose();

        if (D2DNsharpLoadDialog.getAccess() != null) {
            D2DNsharpLoadDialog.getAccess().close();
        }
        // close and clear active editor
        clearEditors();

        // clear/close any non-active NSharp related Editors
        clearNonActiveNSharpEditors();
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
                clearEditors();

                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                D2DNsharpLoadDialog d2dLoadDlg = D2DNsharpLoadDialog
                        .getInstance(shell);
                if (d2dLoadDlg != null) {
                    // select first sounding type
                    d2dLoadDlg.setActiveLoadSoundingType(0);
                    d2dLoadDlg.open();
                }
            }
        });

    }

    private void clearEditors() {
        if (EditorUtil.getActiveEditor() instanceof NsharpEditor) {
            IEditorPart part = EditorUtil.getActiveEditor();
            part.getSite().getPage().closeEditor(part, false);
        }
        if (EditorUtil.getActiveEditor() instanceof VizMapEditor) {
            VizMapEditor mapEditor = (VizMapEditor) EditorUtil
                    .getActiveEditor();
            mapEditor.clear();
        }
    }

    private void clearNonActiveNSharpEditors() {
        if (EditorUtil.findEditor(NSHARP_MAP_EDITOR_ID) != null) {
            NsharpEditor nsharpMapEditor = ((NsharpEditor) EditorUtil
                    .findEditor(NSHARP_MAP_EDITOR_ID));
            nsharpMapEditor.getSite().getPage().closeEditor(nsharpMapEditor, false);
        }

        if (EditorUtil.findEditor(MAP_EDITOR_ID) != null) {
            VizMapEditor vmapEditor = ((VizMapEditor) EditorUtil
                    .findEditor(MAP_EDITOR_ID));
            if (vmapEditor.getTitle().contains("Map")) {
                vmapEditor.clear();
            }
        }
    }

}
