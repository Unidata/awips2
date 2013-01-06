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

package com.raytheon.viz.gfe;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.dialogs.KillJobsOnExitDialog;
import com.raytheon.viz.gfe.dialogs.SaveParameterDialog;
import com.raytheon.viz.gfe.gridmanager.GridManager;
import com.raytheon.viz.gfe.procedures.ProcedureJob;
import com.raytheon.viz.gfe.smarttool.script.SmartToolJob;
import com.raytheon.viz.ui.DetachedViewListener;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;

/**
 * View for displaying GridManager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/19/2008              dfitch      Initial creation.
 * 06/11/2009   #1947      rjpeter     Updated to add saving of parms on close,
 *                                     adding cancel capability and if error on
 *                                     save then the close is cancelled.
 * 10/30/2012   #1298      rferrel     Must keep blocking dialogs to work with eclipse plugins.
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */
public class GridManagerView extends ViewPart implements ISaveablePart2 {

    public static final String ID = "com.raytheon.viz.gfe.GridManagerView";

    private Composite view;

    private IWorkbenchWindow window;

    private DataManager dataManager;

    @Override
    public void init(IViewSite site) throws PartInitException {
        new DetachedViewListener(this, site);
        super.init(site);
    }

    @Override
    public void dispose() {
        super.dispose();
    }

    public DataManager getDataManager() {
        return dataManager;
    }

    @Override
    public void createPartControl(Composite parent) {

        window = this.getSite().getWorkbenchWindow();
        view = parent;

        dataManager = DataManager.getInstance(window);
        if (dataManager == null) {
            return;
        }

        GridManager gm = new GridManager(view, dataManager);
        dataManager.setGridManager(gm);

        String colorName = GFEPreference.getPreference("bgColor");
        if (colorName.isEmpty()) {
            colorName = "black";
        }
        BackgroundColor.getInstance(getSite().getPage().getPerspective())
                .setColor(BGColorMode.EDITOR, RGBColors.getRGBColor(colorName));

        // Causes the GridManger to redraw on initialization.
        DataManager.fireChangeListener();
        refresh();
    }

    @Override
    public void setFocus() {
        // TODO Auto-generated method stub

    }

    public void refresh() {
        /**
         * The resize of the view is done so that view displays properly.
         * 
         */
        view.setSize(view.getBounds().width - 1, view.getBounds().height);
        view.setSize(view.getBounds().width + 1, view.getBounds().height);
    }

    public Composite getView() {
        return view;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISaveablePart2#promptToSaveOnClose()
     */
    @Override
    public int promptToSaveOnClose() {
        // Check for any running/queued jobs.
        if (ProcedureJob.haveJobs() || SmartToolJob.haveJobs()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            KillJobsOnExitDialog dialog = new KillJobsOnExitDialog(shell);
            // Must keep modal and blocking in order to work with eclipse
            // plugins.
            dialog.setBlockOnOpen(true);
            int returnCode = dialog.open();
            if (returnCode == IDialogConstants.CANCEL_ID) {
                return ISaveablePart2.CANCEL;
            }
        }

        // Get the parms that have been modified
        if (dataManager != null) {
            Parm[] parms = dataManager.getParmManager().getModifiedParms();

            if (parms.length > 0) {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();

                SaveParameterDialog dialog = new SaveParameterDialog(shell,
                        dataManager);
                // Must keep modal and blocking in order to work with eclipse
                // plugins.
                dialog.setBlockOnOpen(true);
                int returnCode = dialog.open();

                if (returnCode == IDialogConstants.CANCEL_ID) {
                    return ISaveablePart2.CANCEL;
                }
            }
        }

        return ISaveablePart2.YES;
    }

    @Override
    public void doSave(IProgressMonitor monitor) {
        //
    }

    @Override
    public void doSaveAs() {
    }

    @Override
    public boolean isDirty() {
        if ((dataManager != null && dataManager.getParmManager()
                .getModifiedParms().length > 0)
                || SmartToolJob.haveJobs()
                || ProcedureJob.haveJobs()) {
            return true;
        }
        return false;
    }

    @Override
    public boolean isSaveAsAllowed() {
        return false;
    }

    @Override
    public boolean isSaveOnCloseNeeded() {
        return isDirty();
    }
}