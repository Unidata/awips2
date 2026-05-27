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

package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.awipstools.capabilities.DistanceScaleUnitsCapability;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Allows for the modification of the units used to display the Distance Scale
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Oct 19, 2018 56957       edebebe    Initial Creation.
 * 
 * </pre>
 *
 *
 */
public class DistanceScaleUnitsAction extends AbstractRightClickAction implements
        IMenuCreator {

    protected final IUFStatusHandler logger = UFStatus.getHandler(getClass());

    private Menu menu;

    /**
     * Initialize the selectable Distance Scale Units to:
     *   SM - Statute Miles
     *   NM - Nautical Miles
     *   KM - Kilometers
     */
    private static String[] selectableDistanceScaleUnits = new String[] { "SM", "NM", "KM" };

    /**
     * Constructor
     */
    public DistanceScaleUnitsAction() {
        super(SWT.DROP_DOWN);
    }

    @Override
    public void run() {

    }

    @Override
    public String getText() {
        return "Units";
    }

    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    @Override
    public Menu getMenu(Control parent) {

        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);
        createMenu(menu);

        return menu;
    }

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
        for (String distanceScaleUnit : selectableDistanceScaleUnits) {
            ActionContributionItem aci = new ActionContributionItem(
                    new SetDistanceScaleUnitsAction(distanceScaleUnit));
            aci.fill(menu, -1);
        }
    }

    private class SetDistanceScaleUnitsAction extends Action {
        private String distanceScaleUnit;

        public SetDistanceScaleUnitsAction(String distanceScaleUnit) {
            super(distanceScaleUnit);
            this.distanceScaleUnit = distanceScaleUnit;
        }

        @Override
        public void run() {

            if (distanceScaleUnit != null) {
                DistanceScaleUnitsCapability capability = getTopMostSelectedResource()
                        .getCapability(DistanceScaleUnitsCapability.class);
                capability.setDistanceScaleUnit(distanceScaleUnit);
            }

            logger.debug("Inside SetDistanceScaleUnitsAction.run() - distanceScaleUnit = " + distanceScaleUnit);
            getContainer().refresh();
        }
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

}