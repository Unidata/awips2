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
package com.raytheon.viz.gfe.temporaleditor.actions;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * A version of SetDiscreteWxPickupAction for the temporal editor.
 * 
 * @author wldougher
 * 
 */
public class SetDiscreteWxPickupTEAction extends
        com.raytheon.viz.gfe.actions.SetDiscreteWxPickupAction {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SetDiscreteWxPickupTEAction.class);

    protected Date date;

    /**
     * @param title
     *            The text of the top-level menu of this action.
     * @param parm
     *            The parm against which the values should be applied
     * @param menuItemValues
     *            The weather values to select with the menu
     * @param date
     *            The date of the edit action
     */
    public SetDiscreteWxPickupTEAction(String title, Parm parm,
            WxValue[] menuItemValues, Date date) {
        super(title, parm, menuItemValues);
        this.date = date;
    }

    /**
     * Assigns a pickup value to the active edit area. Differs from the base
     * class version by using the date value passed to the constructor, and
     * doing the work from Java instead of through the smart tool.
     * 
     * @see com.raytheon.viz.gfe.actions.SetDiscreteWxPickupAction#process()
     */
    @Override
    protected void process() {
        Grid2DBit gridArea = DataManager.getCurrentInstance().getRefManager()
                .getActiveRefSet().getGrid();
        DataManager.getCurrentInstance().getParmOp().clearUndoParmList();

        try {
            IGridData gridData = parm.startParmEdit(date);
            gridData.setValue(parm.getParmState().getPickUpValue(), gridArea);
            parm.endParmEdit();
        } catch (GFEOperationFailedException exc) {
            statusHandler.handle(Priority.PROBLEM, "Grid edit failed", exc);
        }
    }
}
