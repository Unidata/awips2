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

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.ColorEditDialogAction;
import com.raytheon.viz.ui.dialogs.ColormapComp;

/**
 * Action for right click menu for setting the colormap
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            njensen     Initial creation
 * Aug 11, 2010           wldougher
 * Aug 28, 2014  3516     rferrel     Add separator after color map component.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ChangeColorTableAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    private Parm parm;

    public ChangeColorTableAction(Parm parm) {
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
        if (menu != null) {
            menu.dispose();
        }

        throw new UnsupportedOperationException("Menu can only be a submenu");
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

        GridType type = parm.getGridInfo().getGridType();
        if (GridType.WEATHER.equals(type)) {
            menu = new Menu(parent);

            ActionContributionItem aci = new ActionContributionItem(
                    new AbstractRightClickAction("Weather") {
                    });
            aci.fill(menu, -1);
        } else {
            // need ColormapParms, ColorapCapability
            ColorMapCapability cap = getSelectedRsc().getCapability(
                    ColorMapCapability.class);
            ColorMapParameters parms = cap.getColorMapParameters();

            // Build a ColormapComp to get the menu from
            ColormapComp comp = new ColormapComp(parent, parms, cap);
            menu = comp.getMenu();
            new MenuItem(menu, SWT.SEPARATOR);
        }

        ActionContributionItem aci = new ActionContributionItem(
                new ChangeBrightnessAction(parm));
        aci.fill(menu, -1);

        AbstractRightClickAction action = new ColorEditDialogAction(
                "Edit Color Table...");
        action.setSelectedRsc(selectedRsc);
        action.setContainer(getContainer());
        aci = new ActionContributionItem(action);
        aci.fill(menu, -1);
        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Change Color Table To";
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
