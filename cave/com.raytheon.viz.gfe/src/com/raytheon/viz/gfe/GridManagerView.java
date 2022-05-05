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
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.dialogs.KillJobsOnExitDialog;
import com.raytheon.viz.gfe.dialogs.SaveParameterDialog;
import com.raytheon.viz.gfe.gridmanager.GridManager;
import com.raytheon.viz.ui.DetachedViewListener;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;

/**
 * View for displaying GridManager
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 19, 2008           dfitch    Initial creation.
 * Jun 11, 2009  1947     rjpeter   Updated to add saving of parms on close,
 *                                  adding cancel capability and if error on
 *                                  save then the close is cancelled.
 * Oct 30, 2012  1298     rferrel   Must keep blocking dialogs to work with
 *                                  eclipse plugins.
 * Dec 10, 2013  2367     dgilling  Use new ProcedureJobePool and
 *                                  SmartToolJobPool.
 * Aug 13, 2015  4749     njensen   dispose() disposes of GridManager
 * Jan 14, 2016  5193     bsteffen  Fix NPE in isDirty
 * Jul 08, 2016  5641     njensen   setFocus() sets the focus on the view
 *                                  composite
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Jan 29, 2019  7749     randerso  Added updateDirtyStatus to notify the
 *                                  eclipse framework when the GridManagerView
 *                                  dirty status changes.
 *
 * </pre>
 *
 * @author dfitch
 */
public class GridManagerView extends ViewPart implements ISaveablePart2 {

    /**
     * GridMangerView part ID
     */
    public static final String ID = "com.raytheon.viz.gfe.GridManagerView";

    private Composite view;

    private DataManager dataManager;

    @Override
    public void init(IViewSite site) throws PartInitException {
        new DetachedViewListener(this, site);
        super.init(site);
    }

    @Override
    public void dispose() {
        super.dispose();
        getDataManager().getGridManager().dispose();
    }

    /**
     * @return the DataManager
     */
    public DataManager getDataManager() {
        return dataManager;
    }

    @Override
    public void createPartControl(Composite parent) {
        view = parent;

        IWorkbenchWindow window = this.getSite().getWorkbenchWindow();
        dataManager = DataManagerUIFactory.getInstance(window);
        if (dataManager == null) {
            return;
        }

        GridManager gm = new GridManager(view, dataManager);
        dataManager.setGridManager(gm);

        String colorName = GFEPreference.getString("bgColor", "black");
        BackgroundColor.getInstance(getSite().getPage().getPerspective())
                .setColor(BGColorMode.EDITOR, RGBColors.getRGBColor(colorName));

        // Causes the GridManger to redraw on initialization.
        refresh();
    }

    @Override
    public void setFocus() {
        view.setFocus();
    }

    /**
     * refresh the GridManagerView
     */
    public void refresh() {
        /**
         * The resize of the view is done so that view displays properly.
         *
         */
        view.setSize(view.getBounds().width - 1, view.getBounds().height);
        view.setSize(view.getBounds().width + 1, view.getBounds().height);
    }

    /**
     * @return the view
     */
    public Composite getView() {
        return view;
    }

    @Override
    public int promptToSaveOnClose() {
        // Check for any running/queued jobs.
        if (dataManager.getProcedureJobPool().isActive()
                || dataManager.getSmartToolJobPool().isActive()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            KillJobsOnExitDialog dialog = new KillJobsOnExitDialog(shell,
                    dataManager);
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
        //
    }

    @Override
    public boolean isDirty() {
        return (dataManager != null)
                && ((dataManager.getParmManager().getModifiedParms().length > 0)
                        || dataManager.getProcedureJobPool().isActive()
                        || dataManager.getSmartToolJobPool().isActive());
    }

    @Override
    public boolean isSaveAsAllowed() {
        return false;
    }

    @Override
    public boolean isSaveOnCloseNeeded() {
        return isDirty();
    }

    /**
     *
     */
    public void updateDirtyStatus() {
        firePropertyChange(ISaveablePart2.PROP_DIRTY);
    }
}