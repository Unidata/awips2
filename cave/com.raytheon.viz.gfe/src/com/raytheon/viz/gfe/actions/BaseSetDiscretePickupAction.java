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

import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * An abstract base class for all the right-click Actions that set discrete
 * pickup values in DiscreteColorBar and the temporal editor.
 * 
 * @author wldougher
 * 
 */
public abstract class BaseSetDiscretePickupAction<T> extends
        AbstractRightClickAction implements IMenuCreator {
    protected Menu topMenu;

    protected Parm parm;

    protected T[] menuItemValues;

    /**
     * Constructor.
     * 
     * @param title
     *            The text of the Action.
     * @param parm
     *            The parm into which pickup values will be set.
     * @param menuItemValues
     *            The values to set
     */
    public BaseSetDiscretePickupAction(String title, Parm parm,
            T[] menuItemValues) {
        super(title);
        this.menuItemValues = menuItemValues;
        this.parm = parm;
        setMenuCreator(this);
    }

    /**
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (topMenu != null && !topMenu.isDisposed()) {
            topMenu.dispose();
        }
    }

    /**
     * Override this method to set a value in the grid.
     */
    abstract protected void process();
}
