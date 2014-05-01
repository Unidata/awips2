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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.SampleSetDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action for launching delete samples dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 7, 2008					Eric Babin Initial Creation
 *  22JUL2008   #1275.      Eric Babin  Remove inadvertent dispose on parent shell.
 * Oct 24, 2012 #1287      rferrel     Changes for non-blocking SampleSetDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ShowDeleteSampleSetDialog extends AbstractHandler {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ShowDeleteSampleSetDialog.class);

    private SampleSetDialog dialog;

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
            ArrayList<SampleId> sampleIdList = DataManager.getCurrentInstance()
                    .getSampleSetManager().getInventoryAsList();

            Collections.sort(sampleIdList, new Comparator<SampleId>() {

                @Override
                public int compare(SampleId o1, SampleId o2) {
                    return o1.getName().compareTo(o2.getName());
                }

            });

            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            dialog = new SampleSetDialog(shell, sampleIdList,
                    SampleSetDialog.DELETE);

            dialog.setBlockOnOpen(false);
            dialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        int returnCode = (Integer) returnValue;
                        doDialogClose(returnCode);
                    }
                    dialog = null;
                }
            });
            dialog.open();
        } else {
            dialog.bringToTop();
        }

        return null;
    }

    private void doDialogClose(int returnCode) {
        if (returnCode != Window.CANCEL
                && dialog.getSelectedSampleIdIndexes() != null) {
            List<SampleId> sampleIdList = dialog.getSamples();
            SampleId id = sampleIdList
                    .get(dialog.getSelectedSampleIdIndexes()[0]);
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            if (MessageDialog.openConfirm(shell, "Delete sample set",
                    "Delete selected sample set?")) {
                if (DataManager.getCurrentInstance().getSampleSetManager()
                        .deleteSampleSet(id)) {
                    statusHandler.handle(Priority.EVENTA, id.getName()
                            + ", Sample set deleted successfully");
                } else {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error deleting sample set, " + id.getName());
                }
            }
        }
    }
}
