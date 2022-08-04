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
package com.raytheon.viz.mpe.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.postanalysis.SummedHourlyMpeDlg;

/**
 * Action to launch the Summed Hourly MPE dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2011            lvenable     Initial creation
 * Feb 26, 2015   9554     cgobs        Enable button based on mpe_post_analysis token and MPE 1-hr vs DQC mode
 * May 10, 2018   7131     mduff        Added null check for MPEDisplayManager.
 * </pre>
 * 
 * @author lvenable
 */

public class SummedHourlyMpeAction extends AbstractHandler {

    private SummedHourlyMpeDlg summedHourlyDlg = null;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        if (summedHourlyDlg == null || summedHourlyDlg.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            summedHourlyDlg = new SummedHourlyMpeDlg(shell);
            summedHourlyDlg.open();
        } else {
            summedHourlyDlg.bringToTop();
        }

        return null;
    }

    @Override
    public boolean isEnabled() {

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        boolean isPostAnalysisOnByToken = appsDefaults
                .getBoolean("mpe_post_analysis", false);

        MPEDisplayManager mgr = MPEDisplayManager.getCurrent();
        if (mgr == null) {
            return false;
        }
        boolean isDailyQCOn = (mgr.isQpf() || mgr.isZflag() || mgr.isMaxmin());

        boolean isPostAnalysisAvailable = isDailyQCOn
                && isPostAnalysisOnByToken;

        return isPostAnalysisAvailable;

    }
}
