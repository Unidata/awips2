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
package com.raytheon.uf.viz.monitor.snow.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.monitor.snow.SnowMonitor;

/**
 * The SNOW Action
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009 1999       grichard    Initial creation.
 * Oct 30, 2009 3424       zhao/wkwock Active snow from SnowMonitor.getINstance() 
 * Nov 30, 2009 3424       zhao/wkwock/slav zhao/wkwock/slav Automatically updates snow display. Display station data.
 * Dec 18, 2009 3424       zhao        Launch snow monitor and snow zone/station table dialog separately here 
 * Feb 26, 2010 4282       zhao        changed to follow the same launch mechanism in FOG
 * Nov.15, 2012 1297       skorolev    Cleaned code
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class SnowAction extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        SnowMonitor snow = SnowMonitor.getInstance();
        if (snow.getZoneDialog() == null || snow.getZoneDialog().isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            snow.launchDialog("zone", shell);
        }
        return null;
    }
}
