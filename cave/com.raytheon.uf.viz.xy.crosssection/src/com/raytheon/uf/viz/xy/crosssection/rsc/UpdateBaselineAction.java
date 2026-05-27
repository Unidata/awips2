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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionRenderableDisplay;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Update Baseline Action
 *
 * Provides a menu to allow the user to select or update the baseline associated
 * with a cross section display.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 17, 2021  8452     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class UpdateBaselineAction extends AbstractRightClickAction
        implements IMenuCreator {
    private Menu menu;

    /**
     * Constructor
     */
    public UpdateBaselineAction() {
        super(Action.AS_DROP_DOWN_MENU);
    }

    @Override
    public String getText() {
        return "Update Baseline";
    }

    @Override
    public void run() {

    }

    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    @Override
    public Menu getMenu(Control parent) {
        createMenu(new Menu(parent));
        return menu;
    }

    @Override
    public Menu getMenu(Menu parent) {
        createMenu(parent);
        return menu;
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    private void createMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }
        menu = new Menu(parent);

        String selectedBaseline = "";
        IDescriptor descriptor = getDescriptor();
        if (descriptor instanceof CrossSectionDescriptor) {
            selectedBaseline = ((CrossSectionDescriptor) descriptor)
                    .getBaseLine();
        }

        for (String baseline : ToolsDataManager.getInstance()
                .getBaselineNames()) {

            ActionContributionItem aci = new ActionContributionItem(
                    new SetBaselineInternalAction(baseline) {
                    });
            aci.getAction().setChecked(selectedBaseline.equals(baseline));
            aci.fill(menu, -1);
        }
    }

    private class SetBaselineInternalAction extends Action {
        private String baseline;

        public SetBaselineInternalAction(String baseline) {
            super("Line " + baseline, IAction.AS_RADIO_BUTTON);
            this.baseline = baseline;
        }

        @Override
        public void run() {
            if (isChecked()) {
                for (IDisplayPane pane : getContainer().getDisplayPanes()) {
                    IRenderableDisplay renderableDisplay = pane
                            .getRenderableDisplay();
                    if (renderableDisplay instanceof CrossSectionRenderableDisplay) {
                        ((CrossSectionRenderableDisplay) renderableDisplay)
                                .updateBaseline(baseline, null);
                    }
                }
            }
        }
    }

}
