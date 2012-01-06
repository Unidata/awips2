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
package com.raytheon.viz.gfe.core.internal;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ITopoManager;

/**
 * GFETopoMgr manages the set of topography data sets available from the
 * ifpServer.
 * 
 * All parms now use the same topo data, since the assumption is that all parms
 * in a database have the same exact GridLocation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 2, 2008		#1160	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFETopoManager implements ITopoManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GFETopoManager.class);

    private DataManager dataManager;

    private ScalarGridSlice compositeTopo;

    private ParmID compositeTopoId;

    private ParmID editTopoId;

    // private GridLocation gloc;
    //
    // private GridParmInfo gpi;

    private GridDataHistory[] gdh;

    private TimeRange validTime;

    /**
     * Constructor taking a pointer to the DataManager
     * 
     * @param dataManager
     */
    public GFETopoManager(DataManager dataManager) {
        this.dataManager = dataManager;

        // this.gloc =
        // this.dataManager.getParmManager().compositeGridLocation();

        this.compositeTopoId = new ParmID("Topography", new DatabaseID(
                this.dataManager.getSiteID(), DataType.GRID, "Topo", "Topo"),
                "SFC");

        this.editTopoId = new ParmID("Topo", new DatabaseID(this.dataManager
                .getSiteID(), DataType.GRID, "EditTopo", "Topo"), "SFC");

        // this.gpi = new GridParmInfo(this.compositeTopoId, this.gloc,
        // GridType.SCALAR, "Feet MSL", "Topography", -16404.0f, 16404.0f,
        // 0, true, new TimeConstraints(0, 0, 0), false);

        this.validTime = TimeRange.allTimes();

        this.gdh = new GridDataHistory[] { new GridDataHistory(
                OriginType.OTHER, this.compositeTopoId, this.validTime) };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ITopoManager#getCompositeTopo()
     */
    @Override
    public synchronized ScalarGridSlice getCompositeTopo() {
        if (compositeTopo == null) {
            List<TimeRange> trs = new ArrayList<TimeRange>();
            trs.add(TimeRange.allTimes());
            try {
                List<IGridSlice> slice = dataManager.getClient().getGridData(
                        editTopoId, trs);
                GridParmInfo gpi = dataManager.getClient().getGridParmInfo(
                        editTopoId);
                gpi.setParmID(compositeTopoId);

                if (slice != null) {
                    compositeTopo = new ScalarGridSlice(this.validTime, gpi,
                            this.gdh, ((ScalarGridSlice) slice.get(0))
                                    .getScalarGrid());
                }
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to load topography", e);
            }
        }
        return compositeTopo;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ITopoManager#getCompositeParmID()
     */
    @Override
    public ParmID getCompositeParmID() {
        return this.compositeTopoId;
    }
}
