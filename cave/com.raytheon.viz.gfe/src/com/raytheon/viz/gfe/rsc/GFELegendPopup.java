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
package com.raytheon.viz.gfe.rsc;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.gfe.rsc.GFELegendResource.LegendMode;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Displays the legend popup for selecting the legend mode
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 24, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GFELegendPopup extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    /**
     * Constructor
     */
    public GFELegendPopup() {
        super(SWT.DROP_DOWN);

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
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Legends";
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

        fillMenu();
        return menu;
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

        fillMenu();
        return menu;
    }

    /**
     * 
     */
    private void fillMenu() {
        GFELegendResource decorator = null;
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            List<GFELegendResource> gfeld = container.getActiveDisplayPane()
                    .getDescriptor().getResourceList()
                    .getResourcesByTypeAsType(GFELegendResource.class);
            for (GFELegendResource dec : gfeld) {
                decorator = dec;
                break;
            }

            if (decorator != null) {
                for (LegendMode mode : GFELegendResource.LegendMode.values()) {
                    if (!mode.equals(decorator.getLegendMode())) {
                        ActionContributionItem aci = new ActionContributionItem(
                                new ChangeLegendMode(mode, decorator));
                        aci.fill(menu, -1);
                    }
                }
            }
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

    private class ChangeLegendMode extends Action {
        private final LegendMode mode;

        private final GFELegendResource decorator;

        public ChangeLegendMode(LegendMode mode, GFELegendResource decorator) {
            this.mode = mode;
            this.decorator = decorator;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return this.mode.toString();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            this.decorator.setLegendMode(mode);
        }

    }

}
