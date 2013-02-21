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
package com.raytheon.viz.ghg;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ghg.monitor.GhgMonitorDlg;

/**
 * The GHG Action Class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 26, 2008 1033		lvenable	Initial creation
 * Nov 15, 2012 1298        rferrel     Changes for non-blocking GhgMonitorDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class GhgMonitorAction extends AbstractHandler {
    private GhgMonitorDlg monitorDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (monitorDlg == null) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            monitorDlg = new GhgMonitorDlg(shell);
        }
        monitorDlg.open();

        return null;
    }

}
