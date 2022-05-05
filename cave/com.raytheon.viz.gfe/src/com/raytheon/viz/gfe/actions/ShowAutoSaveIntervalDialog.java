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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
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
 * Jul  8, 2008             randerso    reworked
 * Oct 23, 2012 1287        rferrel     Changes for non-blocking AutoSaveIntervalDialog.
 * Aug 27, 2013 2302        randerso    Code clean up for AutoSaveJob changes
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */
public class ShowAutoSaveIntervalDialog extends AbstractHandler {
    final private Map<IWorkbenchWindow, AutoSaveIntervalDialog> dialogMap = new HashMap<IWorkbenchWindow, AutoSaveIntervalDialog>();

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
        final IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        DataManager dm = DataManagerUIFactory.findInstance(window);
        if (dm == null) {
            return null;
        }

        AutoSaveIntervalDialog dialog = dialogMap.get(window);

        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {

            AutoSaveJob autoSaveJob = dm.getAutoSaveJob();

            Shell shell = window.getShell();
            dialog = new AutoSaveIntervalDialog(shell, autoSaveJob);
            dialogMap.put(window, dialog);
            dialog.setBlockOnOpen(false);
            dialog.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    dialogMap.remove(window);
                }
            });
            dialog.open();
        } else {
            dialog.bringToTop();
        }
        return null;
    }

}
