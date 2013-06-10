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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;

/**
 * Implements a Virtual Parm, a parm that is not persisted, used during the
 * duration of processing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 17, 2008				chammack	Initial creation
 * Jul 29, 2008             wdougherty  Allow null for data in ctor
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class VParm extends Parm {

    public VParm(ParmID parmID, GridParmInfo gridInfo, boolean mutable,
            boolean displayable, DataManager manager, IGridSlice[] data) {
        super(parmID, gridInfo, mutable, displayable, manager);

        if (data != null) {
            ArrayList<IGridData> newGrids = new ArrayList<IGridData>(
                    data.length);
            for (IGridSlice gs : data) {
                IGridData gd = AbstractGridData.makeGridData(this, gs);
                newGrids.add(gd);
            }
            this.grids.acquireWriteLock();
            try {
                this.grids.addAll(newGrids);
            } finally {
                this.grids.releaseWriteLock();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.Parm#inventoryArrived(com.raytheon.uf.
     * common.time.TimeRange, com.raytheon.uf.common.time.TimeRange[])
     */
    @Override
    public void inventoryArrived(TimeRange affectedTimeRange,
            Map<TimeRange, List<GridDataHistory>> histories) {
        // no op
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.Parm#populateGrid(com.raytheon.viz.gfe
     * .core.griddata.IGridData)
     */
    @Override
    public void populateGrid(IGridData grid) {
        // no op
    }

    @Override
    public void populateGrids(List<IGridData> grid) {
        // no op
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.Parm#requestLock(java.util.List)
     */
    @Override
    protected boolean requestLock(List<LockRequest> lreq) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.Parm#saveParameterSubClass(java.util.List)
     */
    @Override
    protected boolean saveParameterSubClass(List<TimeRange> tr) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.Parm#deallocateUnusedGrids(int)
     */
    @Override
    public void deallocateUnusedGrids(int seconds) {
        // no op
    }

    @Override
    public boolean revertParameter() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void looseLocks() {
        // simply replace the lock table with one without any locks
        this.lockTable.setLocks(new ArrayList<Lock>(0));
    }

}
