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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.d2d.ui.dialogs.DisplayPropertiesDialog;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            bgonzale     Initial creation
 * Oct 15, 2012 1229       rferrel     Changes for non-blocking DisplayPropertiesDialog.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class DisplayPropertiesAction extends AbstractTool {

    private DisplayPropertiesDialog dialog;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            dialog = new DisplayPropertiesDialog(VizWorkbenchManager
                    .getInstance().getCurrentWindow().getShell());
            dialog.open();
        } else {
            dialog.bringToTop();
        }

        return null;
    }

}
