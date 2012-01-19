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
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;

/**
 * Action to select the label field for a resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2010      #2941 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class LabelAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    /**
     * Constructor
     */
    public LabelAction() {
        super("Label", SWT.DROP_DOWN);
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
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
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

        menu = new Menu(parent);
        createMenu(menu);

        return menu;
    }

    private void createMenu(Menu parent) {
        menu = new Menu(parent);

        ActionContributionItem aci = new ActionContributionItem(
                new SetLabelInternalAction(null));
        aci.fill(menu, -1);

        String[] labelFields = getSelectedRsc().getCapability(
                LabelableCapability.class).getAvailableLabelFields();

        if (labelFields != null) {
            for (String labelField : labelFields) {
                aci = new ActionContributionItem(new SetLabelInternalAction(
                        labelField));
                aci.fill(menu, -1);
            }
        }
    }

    private class SetLabelInternalAction extends Action {
        private String field;

        public SetLabelInternalAction(String field) {
            super(field, Action.AS_RADIO_BUTTON);
            this.field = field;
            String currentField = getSelectedRsc().getCapability(
                    LabelableCapability.class).getLabelField();

            boolean checked = false;
            if (field == null) {
                checked = currentField == null;
            } else {
                checked = field.equals(currentField);
            }

            this.setChecked(checked);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            getSelectedRsc().getCapability(LabelableCapability.class)
                    .setLabelField(field);
            getContainer().refresh();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            if (field == null) {
                return "<NoLabel>";
            }

            if ("gid".equals(field)) {
                return "recnum";
            }

            return field.toUpperCase();
        }

    }
}
