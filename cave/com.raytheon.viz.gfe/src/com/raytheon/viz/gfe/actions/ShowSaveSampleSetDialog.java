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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.dialogs.SampleSetDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action to show save sample set dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 7, 2008					Eric Babin Initial Creation
 * Oct 24, 2012 1287       rferrel     Changes for non-blocking SampleSetDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ShowSaveSampleSetDialog extends AbstractHandler {
    private SampleSetDialog dialog;

    private ISampleSetManager sampleMgr;

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
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            sampleMgr = DataManager.getCurrentInstance().getSampleSetManager();

            ArrayList<SampleId> sampleIdList = sampleMgr.getInventoryAsList();
            Collections.sort(sampleIdList, new Comparator<SampleId>() {

                @Override
                public int compare(SampleId o1, SampleId o2) {
                    return o1.getName().compareTo(o2.getName());
                }

            });

            dialog = new SampleSetDialog(shell, sampleIdList,
                    SampleSetDialog.SAVE);

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
        if ((returnCode == SampleSetDialog.OK)
                && (dialog.getSelectedSampleIdIndexes() != null)
                && (dialog.getSampleName() != null)) {
            sampleMgr.saveActiveSampleSet(new SampleId(dialog.getSampleName()));
        }
    }
}
