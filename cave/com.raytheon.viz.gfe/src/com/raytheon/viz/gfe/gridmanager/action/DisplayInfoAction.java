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
package com.raytheon.viz.gfe.gridmanager.action;

import java.util.Date;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.dialogs.GridInfoDialog;

/**
 * Stores information right click menu action DisplayInfoAction.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/2008              dfitch      Initial creation.
 * 11/12/2012   1298       rferrel     Changes for non-blocking GridInfoDialog.
 * 
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class DisplayInfoAction extends AbstractGridManagerAction {

    private Parm parm;

    private Date clickTime;

    public DisplayInfoAction(Parm parm, Date clickTime) {
        super("Display Info");
        this.parm = parm;
        this.clickTime = clickTime;
    }

    @Override
    public void run() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        // Allow multiple instance of this dialog.
        // Mimics A1 and allows user to display information on multiple grids.
        GridInfoDialog dialog = new GridInfoDialog(shell, parm, clickTime);
        dialog.setBlockOnOpen(false);
        dialog.open();
    }

}