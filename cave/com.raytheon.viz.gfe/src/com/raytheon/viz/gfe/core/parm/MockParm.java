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

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;

/**
 * Parm for testing
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 12, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class MockParm extends Parm {

    public MockParm(ParmID parmID, GridParmInfo gridInfo, boolean mutable,
            boolean displayable, DataManager dataMgr) {
        super(parmID, gridInfo, mutable, displayable, dataMgr);

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
        // TODO Auto-generated method stub

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
        // TODO Auto-generated method stub

    }

    @Override
    public void populateGrids(List<IGridData> grids) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.Parm#requestLock(java.util.List)
     */
    @Override
    protected boolean requestLock(List<LockRequest> lreq) {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.Parm#saveParameterSubClass(java.util.List)
     */
    @Override
    protected boolean saveParameterSubClass(List<TimeRange> tr) {
        // TODO Auto-generated method stub
        return false;
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
        // TODO Auto-generated method stub

    }

}
