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
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ShadeableCapability;
import com.raytheon.viz.ui.dialogs.SetOpacityDialog;
import com.raytheon.viz.ui.dialogs.SetOpacityDialog.IOpacityChangedListener;

/**
 * Action to set the shading field for a resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ShadedAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    /**
     * Constructor
     */
    public ShadedAction() {
        super("Shade", SWT.DROP_DOWN);
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
                new SetShadingInternalAction(null));
        aci.fill(menu, -1);

        String[] shadingFields = getTopMostSelectedResource().getCapability(
                ShadeableCapability.class).getAvailableShadingFields();

        if (shadingFields != null) {
            for (String shadingField : shadingFields) {
                aci = new ActionContributionItem(new SetShadingInternalAction(
                        shadingField));
                aci.fill(menu, -1);
            }
        }
        new Separator().fill(menu, -1);

        aci = new ActionContributionItem(new SetAlphaInternalAction());
        aci.fill(menu, -1);
    }

    private class SetShadingInternalAction extends Action {
        private String field;

        public SetShadingInternalAction(String field) {
            super(field, Action.AS_RADIO_BUTTON);
            this.field = field;
            String currentField = getTopMostSelectedResource().getCapability(
                    ShadeableCapability.class).getShadingField();

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
            AbstractVizResource<?, ?> rsc = getTopMostSelectedResource();
            rsc.getCapability(ShadeableCapability.class).setShadingField(field);
            rsc.issueRefresh();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            if (field == null) {
                return "<NoShading>";
            }

            if ("gid".equals(field)) {
                return "recnum";
            }

            return field.toUpperCase();
        }

    }

    private class SetAlphaInternalAction extends Action {

        public SetAlphaInternalAction() {
            super("Set Opacity", Action.AS_PUSH_BUTTON);
            String currentField = getTopMostSelectedResource().getCapability(
                    ShadeableCapability.class).getShadingField();
        }

        @Override
        public String getText() {
            int opacity = (int) (getTopMostSelectedResource().getCapability(
                    ShadeableCapability.class).getOpacity() * 100);
            return String.format("Set Opacity (%d%%)", opacity);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            final AbstractVizResource<?, ?> rsc = getTopMostSelectedResource();
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            float opacity = rsc.getCapability(ShadeableCapability.class)
                    .getOpacity();
            RGB rgb;
            if (rsc.hasCapability(ColorableCapability.class)) {
                rgb = rsc.getCapability(ColorableCapability.class).getColor();
            } else {
                rgb = new RGB(0, 255, 255);
            }
            SetOpacityDialog dlg = new SetOpacityDialog(shell, opacity, rgb);
            dlg.addOpacityChangedListener(new IOpacityChangedListener() {
                @Override
                public void opacityChanged(float opacity) {
                    rsc.getCapability(ShadeableCapability.class).setOpacity(
                            opacity);
                }
            });
            dlg.open();
        }
    }
}
