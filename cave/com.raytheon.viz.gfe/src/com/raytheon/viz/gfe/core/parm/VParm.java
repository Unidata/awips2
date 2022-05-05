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
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Jun 17, 2008           chammack    Initial creation
 * Jul 29, 2008           wdougherty  Allow null for data in ctor
 * Jan 04, 2018  7178     randerso    Changes to support IDataObject. Code
 *                                    cleanup
 *
 * </pre>
 *
 * @author chammack
 */

public class VParm extends Parm {

    /**
     * Constructor
     *
     * @param parmID
     * @param gridInfo
     * @param mutable
     * @param displayable
     * @param manager
     * @param data
     */
    public VParm(ParmID parmID, GridParmInfo gridInfo, boolean mutable,
            boolean displayable, DataManager manager, IGridSlice[] data) {
        super(parmID, gridInfo, mutable, displayable, manager);

        if (data != null) {
            List<IGridData> newGrids = new ArrayList<>(data.length);
            for (IGridSlice gs : data) {
                IGridData gd = AbstractGridData.makeGridData(this, gs, true);
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

    @Override
    public void inventoryArrived(TimeRange affectedTimeRange,
            Map<TimeRange, List<GridDataHistory>> histories) {
        // no op
    }

    @Override
    public void populateGrid(IGridData grid) {
        // no op
    }

    @Override
    public void populateGrids(List<IGridData> grid) {
        // no op
    }

    @Override
    protected boolean requestLock(List<LockRequest> lreq) {
        return true;
    }

    @Override
    protected boolean saveParameterSubClass(List<TimeRange> tr) {
        return true;
    }

    @Override
    public boolean revertParameter() {
        // VParms cannot be reverted
        return false;
    }

    @Override
    public void looseLocks() {
        // simply replace the lock table with one without any locks
        this.lockTable.setLocks(new ArrayList<Lock>(0));
    }

}
