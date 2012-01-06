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
package com.raytheon.viz.mpe.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.SaveBestEstimateProvider;
import com.raytheon.viz.mpe.ui.rsc.XmrgResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SplitScreen extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IEditorPart editor = EditorUtil.getActiveEditor();
        if (editor instanceof IMultiPaneEditor) {
            IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
            if (multiPane.getNumberofPanes() < 2) {
                IDisplayPane activePane = multiPane.getActiveDisplayPane();
                multiPane.addPane(activePane.getRenderableDisplay()
                        .createNewDisplay());

                SaveBestEstimateProvider.getProvider(arg0).setEnabled(true);

                IDisplayPane[] panes = multiPane.getDisplayPanes();
                for (IDisplayPane pane : panes) {
                    if (pane != activePane) {
                        MPEDisplayManager newMgr = MPEDisplayManager
                                .getInstance(pane);
                        MPEDisplayManager activeMgr = MPEDisplayManager
                                .getInstance(activePane);
                        newMgr.setCurrentDate(activeMgr.getCurrentDate());
                        XmrgResource xmrg = (XmrgResource) newMgr
                                .getDisplayedResource();
                        xmrg.updateXmrg(true);
                        break;
                    }
                }

            } else {
                for (IDisplayPane pane : multiPane.getDisplayPanes()) {
                    multiPane.showPane(pane);
                }
            }
        }
        return null;
    }
}
