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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.SaveDeleteSelectTRDialog;

/**
 * Action for launching delete timerange dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Jan 23, 2008					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ShowDeleteSelectionTimeRangeDialog extends AbstractHandler {

    protected static final String DEFAULT_MSG = "You have selected delete on a default period.  Only the user's entry of this item will be deleted.  That is, the item will be reset to the Base default value.  Continue?";

    /**
     * 
     */
    public ShowDeleteSelectionTimeRangeDialog() {
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
        DataManager dataManager = DataManager.getCurrentInstance();
        if (dataManager == null) {
            return null;
        }

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        SaveDeleteSelectTRDialog dialog = new SaveDeleteSelectTRDialog(shell,
                dataManager, "Delete");
        dialog.setBlockOnOpen(true);
        dialog.open();
        // String delName = null;
        // if (dialog.getReturnCode() == Window.OK) {
        // delName = dialog.getItemToDelete();
        // if (selectTRmgr.getRange(delName).getLevel() !=
        // LocalizationLevel.USER) {
        // // Confirm
        // if (!MessageDialog.openConfirm(shell,
        // "Default Time Period", DEFAULT_MSG)) {
        // returnCode = Window.CANCEL;
        // }
        // }
        // }
        //
        // if (returnCode == Window.OK) {
        // selectTRmgr.remove(delName);
        // TimeScaleDisplayedPeriodsPreference
        // .removeTimeScaleDisplayedPeriod(delName);
        // try {
        // selectTRmgr.save(LocalizationLevel.USER);
        // } catch (Exception e) {
        // throw new ExecutionException(
        // "Error saving SelectTimeRanges", e);
        // }
        // }
        // } else {
        // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
        // StatusConstants.CATEGORY_GFE, null,
        // "Error loading time ranges");
        // }
        return null;
    }
}
