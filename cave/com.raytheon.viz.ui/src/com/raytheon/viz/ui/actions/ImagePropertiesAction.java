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

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.dialogs.ImagingDialog;

/**
 * Get imaging dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov 22, 2006             chammack    Initial Creation.
 * Oct 17, 2012 1229        rferrel     Changes for non-blocking ImagingDialog.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ImagePropertiesAction extends AbstractHandler {

    private static ImagingDialog dialog = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            IDisplayPaneContainer container = EditorUtil
                    .getActiveVizContainer();
            if (container != null) {
                dialog = new ImagingDialog(VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell(), container);
                // initalize
                dialog.open();
            } else {
                dialog = null;
            }
        } else {
            dialog.refreshComponents();
            dialog.bringToTop();
        }

        return null;
    }
}
