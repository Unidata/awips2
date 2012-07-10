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

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.TransmitRFCBiasProvider;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ClearMPEData extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        IDisplayPane pane = null;
        MPEDisplayManager displayMgr = null;
        if (editor instanceof IMultiPaneEditor) {
            IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
            if (multiPane.getNumberofPanes() > 1) {
                for (int i = 0; i < multiPane.getNumberofPanes(); i++) {
                    pane = multiPane.getDisplayPanes()[i];
                    displayMgr = MPEDisplayManager.getInstance(pane);
                    displayMgr.clearMPEData();
                }
                displayMgr.setDisplayedResource(null);
            } else {
                pane = multiPane.getDisplayPanes()[0];
                displayMgr = MPEDisplayManager.getInstance(pane);
                displayMgr.clearMPEData();
            }
        } else if (editor != null) {
            pane = editor.getDisplayPanes()[0];
            displayMgr = MPEDisplayManager.getInstance(pane);
            displayMgr.clearMPEData();
            displayMgr.setDisplayedResource(null);
        }
        
        TransmitRFCBiasProvider.setEnabled(false);
        return null;
    }

}
