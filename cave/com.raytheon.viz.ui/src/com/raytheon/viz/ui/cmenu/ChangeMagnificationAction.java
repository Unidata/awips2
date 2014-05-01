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
package com.raytheon.viz.ui.cmenu;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2010            bsteffen     Initial creation
 * Aug 10, 2011           njensen      Added runWithEvent
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ChangeMagnificationAction extends AbstractRightClickAction
        implements IMenuCreator {

    private Menu menu;

    /**
     * Constructor
     */
    public ChangeMagnificationAction() {
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
        return "Magnification";
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
        createMenu(menu);

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

        createMenu(parent);

        return menu;
    }

    private void createMenu(Menu parent) {
        menu = new Menu(parent);
        for (double magnification : getTopMostSelectedResource().getCapability(
                MagnificationCapability.class).getMagnificationValues()) {
            ActionContributionItem aci = new ActionContributionItem(
                    new SetMagnificationInternalAction(magnification));
            aci.fill(menu, -1);
        }
    }

    private class SetMagnificationInternalAction extends Action {
        final double magnification;

        public SetMagnificationInternalAction(double magnification) {
            super("" + magnification, Action.AS_RADIO_BUTTON);
            this.magnification = magnification;
            this.setChecked(magnification == getTopMostSelectedResource()
                    .getCapability(MagnificationCapability.class)
                    .getMagnification());
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            getTopMostSelectedResource().getCapability(
                    MagnificationCapability.class).setMagnification(
                    magnification);

            getContainer().refresh();
        }

        @Override
        public void runWithEvent(Event event) {
            if (((MenuItem) event.widget).getSelection()) {
                run();
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return MagnificationCapability
                    .getMagnificationAsString(magnification);
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

}
