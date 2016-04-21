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
/**
 * 
 */
package com.raytheon.viz.gfe.actions;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.smarttool.SmartToolConstants;
import com.raytheon.viz.gfe.smarttool.SmartUtil;

/**
 * A submenu Action that presents a menu of pickup values, from a group of
 * discrete WxValues. This class was split off from SetDiscretePickupAction. It
 * avoids parsing strings into WxValues, curing some errors (especially with
 * complex discrete keys), at the cost of allowing only simple menus.
 * 
 * @author wldougher
 * 
 */
public class SetDiscreteWxPickupAction extends
        BaseSetDiscretePickupAction<WxValue> {

    /**
     * Constructor.
     * 
     * @param title
     *            The title of the menu
     * @param parm
     *            the Parm to which pickup values will be applied
     * @param menuItemValues
     */
    public SetDiscreteWxPickupAction(String title, Parm parm,
            WxValue[] menuItemValues) {
        super(title, parm, menuItemValues);
    }

    /**
     * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Control)
     */
    @Override
    public Menu getMenu(Control parent) {
        topMenu = new Menu(parent);
        buildMenu();
        return topMenu;
    }

    /**
     * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        topMenu = new Menu(parent);
        buildMenu();
        return topMenu;
    }

    /**
     * Build the body of the menu.
     */
    protected void buildMenu() {
        MenuItem menuItem;
        for (WxValue value : menuItemValues) {
            if (value instanceof DiscreteWxValue
                    && value.getParm().getParmID().equals(parm.getParmID())) {
                menuItem = new MenuItem(topMenu, SWT.CASCADE);
                final DiscreteWxValue dvalue = (DiscreteWxValue) value;
                menuItem.setText(dvalue.toString());
                menuItem.addSelectionListener(new SelectionAdapter() {
                    /**
                     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        updatePickupValue(dvalue);
                    }
                });
            } else if (value instanceof WeatherWxValue
                    && value.getParm().getParmID().equals(parm.getParmID())) {
                menuItem = new MenuItem(topMenu, SWT.CASCADE);
                final WeatherWxValue wvalue = (WeatherWxValue) value;
                menuItem.setText(wvalue.toString());
                menuItem.addSelectionListener(new SelectionAdapter() {
                    /**
                     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        updatePickupValue(wvalue);
                    }
                });
            }
        }
    }

    /**
     * 
     * @see com.raytheon.viz.gfe.actions.BaseSetDiscretePickupAction#process()
     */
    @Override
    protected void process() {
        SmartUtil.runTool(SmartToolConstants.ASSIGN);
    }

    /**
     * Assign value to every point in the current edit area, or the entire grid
     * if no edit area is active.
     * 
     * @param value
     *            The value to assign to the current edit area
     */
    protected void updatePickupValue(DiscreteWxValue value) {
        this.parm.getParmState().setPickUpValue(value);
        process();
    }

    /**
     * Assign value to every point in the current edit area, or the entire grid
     * if no edit area is active.
     * 
     * @param value
     *            The value to assign to the current edit area
     */
    protected void updatePickupValue(WeatherWxValue value) {
        this.parm.getParmState().setPickUpValue(value);
        process();
    }
}
