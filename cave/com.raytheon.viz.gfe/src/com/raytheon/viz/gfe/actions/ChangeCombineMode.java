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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.parm.ParmState.CombineMode;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Provides an interface to change the combine mode for discrete/wx parms
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 17, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ChangeCombineMode extends AbstractRightClickAction implements
        IMenuCreator {
    private Menu menu;

    private final Parm parm;

    public ChangeCombineMode(Parm parm) {
        super(SWT.DROP_DOWN);
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

        CombineMode[] modes = ParmState.CombineMode.values();

        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        for (CombineMode mode : modes) {
            ActionContributionItem aci = new ActionContributionItem(
                    new CombineModeChange(mode));
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
        CombineMode[] modes = ParmState.CombineMode.values();

        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        for (CombineMode mode : modes) {
            ActionContributionItem aci = new ActionContributionItem(
                    new CombineModeChange(mode));
            aci.fill(menu, -1);

        }
        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        // no op
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        if (this.parm.getGridInfo().getGridType().equals(GridType.WEATHER)) {
            return "Weather Edit Mode";
        } else {
            return "Discrete Edit Mode";
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    private class CombineModeChange extends Action {
        private final ParmState.CombineMode combineMode;

        public CombineModeChange(ParmState.CombineMode combineMode) {
            this.combineMode = combineMode;
            this.setChecked(parm.getParmState().getCombineMode() == combineMode);
        }

        @Override
        public void run() {
            DataManager dm = DataManager.getCurrentInstance();
            if (dm != null) {
                dm.getParmOp().setCombineMode(this.combineMode);
            }
        }

        @Override
        public String getText() {
            return combineMode.toString();
        }

    }

}
