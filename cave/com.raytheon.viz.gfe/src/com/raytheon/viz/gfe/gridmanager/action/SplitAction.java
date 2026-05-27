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

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Stores information right click menu action SplitAction.
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

public class SplitAction extends AbstractGridManagerAction {

    Parm parm;

    Date splitAt;

    public SplitAction(Parm parm, Date splitAt) {
        super("Split Grid");

        this.parm = parm;

        this.splitAt = splitAt;

    }

    @Override
    public void run() {

        IGridData gd = parm.overlappingGrid(splitAt);

        // Split the grid
        if (gd != null) {
            TimeRange tr = parm.getGridInfo().getTimeConstraints()
                    .constraintTime(splitAt);
            Date t1 = tr.getStart();
            Date t2 = tr.getEnd();
            if (splitAt.getTime() - t1.getTime() < t2.getTime()
                    - splitAt.getTime()) {
                tr = new TimeRange(gd.getGridTime().getEnd(), t1);
            } else {
                tr = new TimeRange(t2, gd.getGridTime().getEnd());
            }

            if (gd.getGridTime() != tr) {
                parm.splitTR(tr);
            }
        }

    }
}