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
package com.raytheon.viz.gfe.gridmanager.action;

import java.util.Date;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.Parm.CreateFromScratchMode;

/**
 * Stores information right click menu action CreateFromScratch.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/2008              dfitch      Initial creation.
 * 
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */
public class CreateFromScratchAction extends AbstractGridManagerAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CreateFromScratchAction.class);

    Date clickTime;

    Parm parm;

    public CreateFromScratchAction(Parm parm, Date clickTime) {
        super("Create From Scratch");

        this.parm = parm;
        this.clickTime = clickTime;
    }

    @Override
    public void run() {

        try {
            TimeRange timeRange = parm.getGridInfo().getTimeConstraints()
                    .constraintTime(clickTime);

            parm.insertNewGrid(new TimeRange[] { timeRange },
                    CreateFromScratchMode.DEFAULT);

            DataManager dm = DataManager.getCurrentInstance();
            dm.getGridManager().setSelectedTime(clickTime);

            dm.getSpatialDisplayManager().makeVisible(parm, true, true);
            dm.getSpatialDisplayManager().activateParm(parm);

            parm.getParmState().setSelected(true);
            dm.getParmOp().setSelectionTimeRange(timeRange);

        } catch (GFEOperationFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to insert a new Grid. ", e);
        }

    }
}
