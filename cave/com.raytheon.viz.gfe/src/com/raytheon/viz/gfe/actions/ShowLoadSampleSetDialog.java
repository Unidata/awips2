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
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.dialogs.SampleSetDialog;

/**
 * Action to launch sampele set dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2008             Eric Babin  Initial Creation
 * Apr 9, 2009  1288       rjpeter     Removed explicit refresh of SpatialDisplayManager.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ShowLoadSampleSetDialog extends AbstractHandler {

    // SampleRenderable sample;

    // public ShowLoadSampleSetDialog() {
    //
    // super();
    // this.sample = new SampleRenderable();
    // }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        DataManager dm = DataManager.getCurrentInstance();
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

        SampleSetDialog dialog = new SampleSetDialog(shell, sampleIdList,
                SampleSetDialog.LOAD);

        dialog.setBlockOnOpen(true);
        dialog.open();
        if (dialog.getReturnCode() != Window.CANCEL
                && dialog.getSelectedSampleIdIndexes() != null) {
            if (dialog.getReturnCode() == SampleSetDialog.OK) {
                if (dialog.getType() == SampleSetDialog.LOAD) {
                    for (int i = 0; i < dialog.getSelectedSampleIdIndexes().length; i++) {
                        SampleId id = sampleIdList.get(dialog
                                .getSelectedSampleIdIndexes()[i]);
                        try {
                            dm.getSampleSetManager().loadSampleSet(id,
                                    ISampleSetManager.SampleSetLoadMode.ADD);
                        } catch (GFEException e) {
                            e.printStackTrace();
                        }
                    }
                } else if (dialog.getType() == SampleSetDialog.SAVE) {
                    dm.getSampleSetManager().saveActiveSampleSet(
                            new SampleId(dialog.getSampleName()));
                }
                if (dialog.getType() == SampleSetDialog.DELETE) {
                    for (int i = 0; i < dialog.getSelectedSampleIdIndexes().length; i++) {
                        SampleId id = sampleIdList.get(dialog
                                .getSelectedSampleIdIndexes()[i]);
                        dm.getSampleSetManager().deleteSampleSet(id);
                    }
                }
            } else if (dialog.getReturnCode() == SampleSetDialog.REMOVE) {
                for (int i = 0; i < dialog.getSelectedSampleIdIndexes().length; i++) {
                    SampleId id = sampleIdList.get(dialog
                            .getSelectedSampleIdIndexes()[i]);
                    try {
                        dm.getSampleSetManager().loadSampleSet(id,
                                ISampleSetManager.SampleSetLoadMode.REMOVE);
                    } catch (GFEException e) {
                        e.printStackTrace();
                    }
                }

            } else if (dialog.getReturnCode() == SampleSetDialog.REPLACE) {
                for (int i = 0; i < dialog.getSelectedSampleIdIndexes().length; i++) {
                    SampleId id = sampleIdList.get(dialog
                            .getSelectedSampleIdIndexes()[i]);
                    try {
                        dm.getSampleSetManager().loadSampleSet(id,
                                ISampleSetManager.SampleSetLoadMode.REPLACE);
                    } catch (GFEException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        // refresh();
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#refresh()
     */
    // @Override
    // protected void refresh() {
    // IEditorPart part = VizApp.getCurrentEditor();
    // if (part instanceof AbstractEditor) {
    // ((AbstractEditor) part).refresh();
    // }
    // }
}
