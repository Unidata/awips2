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
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Stores information right click menu action PasteAction.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/17/2008              dfitch      Initial creation.
 * 04/09/2009   1288       rjpeter     Removed explicit refresh of SpatialDisplayManager.
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class PasteAction extends AbstractGridManagerAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PasteAction.class);

    Parm parm;

    Date pasteAt;

    public PasteAction(Parm parm, Date pasteAt) {
        super("Paste Grid");

        this.parm = parm;

        this.pasteAt = pasteAt;

    }

    @Override
    public void run() {
        // Paste the Grid
        DataManager dm = DataManager.getCurrentInstance();

        if (dm.getParmOp().pasteGrid(parm, pasteAt)) {
            try {
                dm.getSpatialDisplayManager().activateParm(parm);

                dm.getGridManager().setSelectedTime(pasteAt);
                IGridData grid = parm.overlappingGrid(pasteAt);
                dm.getParmOp().setSelectionTimeRange(grid.getGridTime());

                dm.getSpatialDisplayManager().makeVisible(parm, true, true);
            } catch (GFEOperationFailedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }
}
