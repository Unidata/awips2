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
package com.raytheon.uf.viz.localization.perspective.view.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.localization.perspective.view.FileTreeView;

/**
 * Action to change the filtering of localization context names (ie only this
 * user or all users, sites, workstations, etc)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ShowAllAction extends Action implements IMenuCreator {

    private FileTreeView view;

    private Menu menu;

    public ShowAllAction(FileTreeView view) {
        super("Show All", IAction.AS_DROP_DOWN_MENU);
        this.view = view;
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
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

        fillMenu(menu);

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

        fillMenu(menu);
        return menu;
    }

    /**
     * 
     */
    private void fillMenu(Menu menu) {
        // Use of LocalizationLevels.values() in this case should be okay since
        // we are setting a property to display all context names for the level,
        // doesn't matter if our local context for the level is set
        for (LocalizationLevel level : PathManagerFactory.getPathManager()
                .getAvailableLevels()) {
            if (level.isSystemLevel() == false) {
                ActionContributionItem aci = new ActionContributionItem(
                        new ShowAllInternalAction(level));
                aci.fill(menu, -1);
            }
        }
    }

    private class ShowAllInternalAction extends Action {

        private LocalizationLevel level;

        public ShowAllInternalAction(LocalizationLevel level) {
            super(LocalizationUtil.getProperName(level) + "s",
                    IAction.AS_CHECK_BOX);
            this.level = level;
            setChecked(view.isAllShown(level));
        }

        @Override
        public void run() {
            view.toggleShowAllLevel(level);
            setChecked(view.isAllShown(level));
        }
    }
}
