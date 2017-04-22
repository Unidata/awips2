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
package com.raytheon.viz.gfe.rsc.colorbar;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.colorbar.FitToData.FitToDataMode;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Action for right click menu for fitting colormap to data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FitToDataAction extends AbstractRightClickAction implements
        IMenuCreator {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(FitToDataAction.class);

    private Menu menu;

    private DataManager dataMgr;

    private Parm parm;

    public FitToDataAction(DataManager dataMgr, Parm parm) {
        super(SWT.DROP_DOWN);
        this.dataMgr = dataMgr;
        this.parm = parm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Control)
     */
    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }
        menu = new Menu(parent);

        for (FitToDataMode f : FitToDataMode.values()) {
            ActionContributionItem aci = new ActionContributionItem(
                    new FitToDataInternalAction(f));
            aci.fill(menu, -1);
        }
        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }
        menu = new Menu(parent);

        for (FitToDataMode f : FitToDataMode.values()) {
            ActionContributionItem aci = new ActionContributionItem(
                    new FitToDataInternalAction(f));
            aci.fill(menu, -1);
        }
        return menu;
    }

    @Override
    public String getText() {
        return "Fit to Data";
    }

    private class FitToDataInternalAction extends Action {

        private FitToDataMode fit;

        public FitToDataInternalAction(FitToDataMode fit) {
            this.fit = fit;
        }

        @Override
        public void run() {
            FitToData util = new FitToData(dataMgr, parm);
            try {
                switch (fit) {
                case ALL_GRIDS:
                    util.fitToData();
                    break;
                case ALL_GRIDS_OVER_AREA:
                    util.fitToData(dataMgr.getRefManager().getActiveRefSet());
                    break;
                case SINGLE_GRID:
                    util
                            .fitToData(new GridID(parm, dataMgr
                                    .getSpatialDisplayManager()
                                    .getSpatialEditorTime()));
                    break;
                case SINGLE_GRID_OVER_AREA:
                    util.fitToData(
                            new GridID(parm, dataMgr.getSpatialDisplayManager()
                                    .getSpatialEditorTime()), dataMgr
                                    .getRefManager().getActiveRefSet());
                    break;
                }
            } catch (GFEOperationFailedException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error fitting colormap to data", e);
            }
        }

        @Override
        public String getText() {
            return fit.menu;
        }
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

}
