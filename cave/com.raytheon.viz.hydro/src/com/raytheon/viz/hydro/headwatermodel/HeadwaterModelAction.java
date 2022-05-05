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
package com.raytheon.viz.hydro.headwatermodel;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;

/**
 * Handles Hydro's Site Specific Headwater Model menu item. Note that it
 * essentially delegates launch duties to {@link AppLauncherHandler}, which
 * launches the Site Specific (SSHP) application.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 09, 2009  2227     mfegan    Initial creation
 * Oct 27, 2016  5969     randerso  Add support for locating hydroapps on the
 *                                  correct monitor
 * May 01, 2018  7027     mduff     Change how SSHP is launched, only a single
 *                                  window is ever opened.
 * Nov 26, 2018  7027     randerso  Moved lid check here.
 * 
 * </pre>
 *
 * @author mfegan
 */

public class HeadwaterModelAction extends AbstractHandler {

    /**
     * Constructor.
     */
    public HeadwaterModelAction() {
        // intentionally empty
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = HandlerUtil.getActiveShell(event);

        HydroDisplayManager mgr = HydroDisplayManager.getInstance();
        if (mgr.isCurrentLidSelected(shell)) {
            mgr.launchSshp(event.getParameter("bundleLocation"),
                    mgr.getCurrentLid());
        }

        return null;
    }
}
