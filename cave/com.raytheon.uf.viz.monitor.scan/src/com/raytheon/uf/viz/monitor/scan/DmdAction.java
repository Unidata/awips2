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
package com.raytheon.uf.viz.monitor.scan;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;

public class DmdAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        
        String icao = arg0.getParameter("icao");
     
        System.out.println("Activating/Action for DMD table...");
        final String ficao = icao;
        Display.getDefault().asyncExec(new Runnable() {

            public void run() {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                
                ScanMonitor scan = ScanMonitor.getInstance();

                // first time initialization, or re-init
                if (scan.icaos.size() == 0
                        || !scan.icaos.contains(ficao)) {
                    scan.launchSplash(shell);
                    scan.setup(ficao);
                }

                scan.launchDialog(shell, ficao, ScanTables.DMD);
         
            }
        });
    
        return null;
    }
}
