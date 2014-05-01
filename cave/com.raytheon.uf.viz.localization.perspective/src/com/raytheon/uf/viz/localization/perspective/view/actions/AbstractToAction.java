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
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Abstract action that will apply to a localization level
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

public abstract class AbstractToAction extends Action implements IMenuCreator {

    private Menu menu;

    protected LocalizationFile file;

    public AbstractToAction(String text, LocalizationFile file) {
        super(text, IAction.AS_DROP_DOWN_MENU);
        this.file = file;
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {

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

    @Override
    public Menu getMenu(Control parent) {
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
    protected void fillMenu(Menu menu) {
        LocalizationLevel[] levels = PathManagerFactory.getPathManager()
                .getAvailableLevels();
        for (int i = 0; i < levels.length; ++i) {
            LocalizationLevel level = levels[i];
            if (level.isSystemLevel() == false) {
                new ActionContributionItem(new AbstractToInternalAction(level))
                        .fill(menu, -1);
            }
        }
    }

    /**
     * Determines if the action for this level is enabled. By default, checks if
     * the level is the same as the file level
     * 
     * @param level
     * @return
     */
    protected boolean isLevelEnabled(LocalizationLevel level) {
        if (level == file.getContext().getLocalizationLevel()) {
            String fileCtxName = file.getContext().getContextName();
            String levelCtxName = LocalizationManager.getContextName(level);
            if ((fileCtxName == null && levelCtxName == null)
                    || (fileCtxName != null && fileCtxName.equals(levelCtxName))) {
                // same context name
                return false;
            }
        }
        return true;
    }

    protected abstract void run(LocalizationLevel level);

    private class AbstractToInternalAction extends Action {

        private LocalizationLevel level;

        public AbstractToInternalAction(LocalizationLevel level) {
            this.level = level;
            this.setEnabled(isLevelEnabled(level));
        }

        @Override
        public String getText() {
            String name = LocalizationUtil.getProperName(level);
            String context = LocalizationManager.getContextName(level);
            if (context != null) {
                name += " (" + context + ")";
            }
            return name;
        }

        @Override
        public void run() {
            AbstractToAction.this.run(level);
        }

    }

}
