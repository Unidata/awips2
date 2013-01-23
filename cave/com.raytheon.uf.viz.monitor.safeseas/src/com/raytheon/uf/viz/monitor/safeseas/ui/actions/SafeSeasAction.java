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

/**
 * The SAFESEAS Action
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009 1999       grichard    Initial creation.
 * Nov 30, 2009 3424       zhao/Slav/wkwock launch safeseas from SafeseasMonitor.getInstance() 
 * Dec 30, 2009 3424       zhao        Launch SS monitor and SS zone/station table dialog separately here 
 * Feb 26, 2010 4282       zhao        Changed to follow the same dialog launch mechanism as in FOG
 * Nov 15, 2012 1297       skorolev    Cleaned code
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class SafeSeasAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        SafeSeasMonitor monitor = SafeSeasMonitor.getInstance();
        if (monitor.getZoneDialog() == null
                || monitor.getZoneDialog().isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            monitor.launchDialog("zone", shell);
        }
        return null;
    }

}
