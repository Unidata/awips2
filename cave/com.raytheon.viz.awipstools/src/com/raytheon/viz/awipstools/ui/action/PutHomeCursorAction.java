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
package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.awipstools.ui.dialog.PutHomeCursorDialog;
import com.raytheon.viz.awipstools.ui.layer.HomeToolLayer;
import com.raytheon.viz.ui.tools.map.AbstractMapTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  10-21-09    #1049      bsteffen   Synchronize with new Home Tool Layer
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class PutHomeCursorAction extends AbstractMapTool {

    private PutHomeCursorDialog putHomeCursorDialog;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        HomeToolLayer homeToolLayer = (HomeToolLayer) getResource(
                HomeToolLayer.class, HomeToolAction.class);

        if (putHomeCursorDialog == null
                || putHomeCursorDialog.getShell() == null) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            putHomeCursorDialog = new PutHomeCursorDialog(shell, homeToolLayer
                    .getResourceData());
            putHomeCursorDialog.open();
        }

        return null;
    }

}
