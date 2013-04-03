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

package com.raytheon.viz.gfe.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.dialogs.AutoSaveIntervalDialog;
import com.raytheon.viz.gfe.jobs.AutoSaveJob;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action for launching auto save dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 23, 2008             Eric Babin  Initial Creation
 * Jul 8, 2008              randerso    reworked
 * Oct 23, 2012 1287        rferrel     Changes for non-blocking AutoSaveIntervalDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */
public class ShowAutoSaveIntervalDialog extends AbstractHandler {
    private AutoSaveIntervalDialog dialog;

    /**
     * 
     */
    public ShowAutoSaveIntervalDialog() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            final IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();
            Shell shell = window.getShell();

            int interval = AutoSaveJob.getInterval();
            boolean autoSaveEnabled = interval > 0;
            if (!autoSaveEnabled) {
                interval = AutoSaveJob.MAX_INTERVAL;
            }
            dialog = new AutoSaveIntervalDialog(shell, interval,
                    autoSaveEnabled);
            dialog.setBlockOnOpen(false);
            dialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        int returnCode = (Integer) returnValue;
                        if (returnCode == Window.OK) {
                            DataManager dm = DataManagerUIFactory
                                    .findInstance(window);
                            // update
                            if (dialog.isAutoSaveEnabled()) {
                                int interval = dialog.getCurrentInterval();
                                AutoSaveJob.setInterval(interval);
                                if (dm != null) {
                                    dm.enableAutoSave();
                                }
                                DataManagerUIFactory.findInstance(window)
                                        .enableAutoSave();
                            } else {
                                AutoSaveJob.setInterval(0);
                                if (dm != null) {
                                    dm.disableAutoSave();
                                }
                            }

                        }
                    }
                }
            });
            dialog.open();
        } else {
            dialog.bringToTop();
        }
        return null;
    }

}
