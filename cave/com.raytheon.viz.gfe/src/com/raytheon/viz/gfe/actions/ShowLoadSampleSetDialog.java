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
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.dialogs.SampleSetDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action to launch sampele set dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2008             Eric Babin  Initial Creation
 * Apr 9, 2009  1288       rjpeter     Removed explicit refresh of SpatialDisplayManager.
 * Oct 24, 2012 1287       rferrel     Changes for non-blocking SampleSetDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ShowLoadSampleSetDialog extends AbstractHandler {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ShowLoadSampleSetDialog.class);

    private SampleSetDialog dialog;

    private DataManager dm;

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
            dm = DataManager.getCurrentInstance();
            if (dm == null) {
                return null;
            }

            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            ArrayList<SampleId> sampleIdList = dm.getSampleSetManager()
                    .getInventoryAsList();

            Collections.sort(sampleIdList, new Comparator<SampleId>() {

                @Override
                public int compare(SampleId o1, SampleId o2) {
                    return o1.getName().compareTo(o2.getName());
                }

            });

            dialog = new SampleSetDialog(shell, sampleIdList,
                    SampleSetDialog.LOAD);

            dialog.setBlockOnOpen(false);
            dialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        int returnCode = (Integer) returnValue;
                        doDialogClosed(returnCode);
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

    private void doDialogClosed(int returnCode) {
        if (returnCode != Window.CANCEL
                && dialog.getSelectedSampleIdIndexes() != null) {
            List<SampleId> sampleIdList = dialog.getSamples();
            ISampleSetManager.SampleSetLoadMode mode = null;
            switch (returnCode) {
            case SampleSetDialog.OK:
                mode = ISampleSetManager.SampleSetLoadMode.ADD;
                break;
            case SampleSetDialog.REMOVE:
                mode = ISampleSetManager.SampleSetLoadMode.REMOVE;
                break;
            case SampleSetDialog.REPLACE:
                mode = ISampleSetManager.SampleSetLoadMode.REPLACE;
                break;
            default:
                statusHandler.handle(Priority.PROBLEM,
                        "Load unknow return code: " + returnCode);
                return;
            }

            for (int index : dialog.getSelectedSampleIdIndexes()) {
                SampleId id = sampleIdList.get(index);
                try {
                    dm.getSampleSetManager().loadSampleSet(id, mode);
                } catch (GFEException e) {
                    statusHandler.handle(Priority.ERROR,
                            "Load failed for mode: " + mode.toString()
                                    + ", sample id: " + id.toString(), e);
                }
            }
        }
    }
}
