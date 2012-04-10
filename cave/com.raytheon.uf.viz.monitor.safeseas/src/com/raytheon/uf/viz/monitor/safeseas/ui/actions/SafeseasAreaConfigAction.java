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
package com.raytheon.uf.viz.monitor.safeseas.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.safeseas.ui.dialogs.SSMonitoringAreaConfigDlg;

/**
 * The SAFESEAS Action
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 28, 2009 3963       dhladky    Initial creation.
 * March 5, 2012 14413     zhao       Launch AreaConfigDlg w/o monitor 
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SafeseasAreaConfigAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
    	System.out.println("Activating/Action the Safeseas Area Config...");
        
        //SafeSeasMonitor monitor = SafeSeasMonitor.getInstance(); 
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        //monitor.launchDialog("area", shell);
        
        SSMonitoringAreaConfigDlg configDlg= new SSMonitoringAreaConfigDlg(shell, "Safe Seas Monitor Area Configuration");
        configDlg.open();
   
        return null;
    }

}
