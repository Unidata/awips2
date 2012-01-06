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
package com.raytheon.viz.gfe.temporaleditor.actions;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.actions.AbstractSetDiscretePickupAction;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Used by the right click menu on a WEATHER grid type, to set a discrete
 * weather element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * May 28, 2009 #2159      Richard Peter Initial Creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class SetDiscretePickupAction extends AbstractSetDiscretePickupAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(SetDiscretePickupAction.class);
    private Date date;

    public SetDiscretePickupAction(String title, String[] menuitemValues,
            Parm parm, Date date) {
        super(title, parm, menuitemValues);
        this.date = date;
    }

    protected void process() {
        Grid2DBit gridArea = DataManager.getCurrentInstance().getRefManager()
                .getActiveRefSet().getGrid();
        DataManager.getCurrentInstance().getParmOp().clearUndoParmList();

        try {
            IGridData gridData = parm.startParmEdit(date);
            gridData.setValue(parm.getParmState().getPickUpValue(), gridArea);
            parm.endParmEdit();
        } catch (GFEOperationFailedException exc) {
            statusHandler.handle(Priority.PROBLEM,
                            "Grid edit failed", exc);
        }
    }
}
