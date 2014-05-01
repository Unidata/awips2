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
package com.raytheon.viz.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Creates a new four panel map editor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NewFourPanelEditor extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewFourPanelEditor.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (event == null) {
            event = new ExecutionEvent();
        }
        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        IEditorPart part = VizWorkbenchManager.getInstance().getActiveEditor(
                window);
        if (part == null || part instanceof IMultiPaneEditor == false) {
            // If no open editor or current editor is not multi pane editor,
            // attempt to open new one
            AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                    .getInstance(window).getActivePerspectiveManager();
            if (mgr != null) {
                part = mgr.openNewEditor();
                if (part != null && part instanceof IMultiPaneEditor == false) {
                    // if new editor is not multi pane editor,
                    // close the new editor
                    window.getActivePage().closeEditor(part, false);
                    part = null;
                }
            } else {
                part = null;
            }
        } else {
            part = new NewAbstractEditor().execute(null);
        }
        if (part != null) {
            // at this point, we know we have a multi pane editor we can load as
            // 4 panel
            IMultiPaneEditor mpe = (IMultiPaneEditor) part;
            IDisplayPane[] panes = mpe.getDisplayPanes();
            if (panes != null && panes.length < 4) {
                IRenderableDisplay displayToClone = null;
                for (IDisplayPane pane : panes) {
                    displayToClone = pane.getRenderableDisplay();
                    if (displayToClone != null) {
                        break;
                    }
                }
                for (int i = panes.length; i < 4; ++i) {
                    mpe.addPane(displayToClone.createNewDisplay());
                }
            }
        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not open new four panel editor", new VizException());
        }
        return part;
    }

}
