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
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.localization.perspective.view.FileTreeView;

/**
 * Action for generating menu items for hiding/showing localization levels in
 * the {@link FileTreeView}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ShowLevelsAction extends Action implements IMenuCreator {

    private FileTreeView view;

    private Menu menu;

    public ShowLevelsAction(FileTreeView view) {
        super("Show", IAction.AS_DROP_DOWN_MENU);
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
            ActionContributionItem aci = new ActionContributionItem(
                    new ShowLevelInternalAction(level));
            aci.fill(menu, -1);
        }
    }

    private class ShowLevelInternalAction extends Action {

        private LocalizationLevel level;

        public ShowLevelInternalAction(LocalizationLevel level) {
            super(level.name(), IAction.AS_CHECK_BOX);
            this.level = level;
            setChecked(view.isShown(level));
        }

        @Override
        public void run() {
            view.toggleShowLevel(level);
            setChecked(view.isShown(level));
        }
    }
}
