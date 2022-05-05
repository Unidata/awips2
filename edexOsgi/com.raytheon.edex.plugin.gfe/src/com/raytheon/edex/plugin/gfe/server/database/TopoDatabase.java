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
package com.raytheon.edex.plugin.gfe.server.database;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmStorageInfo;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * A complete encapsulation of the TopoMgr data.
 * <p>
 * Implements the logic needed to make the TopoMgr data appear like another
 * database.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 19, 2008  1160     randerso  Initial creation
 * Jul 10, 2009  2590     njensen   Support for multiple sites.
 * May 04, 2012  574      dgilling  Re-port to better match AWIPS1.
 * Apr 23, 2013  1949     rjpeter   Removed unused method.
 * Nov 20, 2013  2331     randerso  Changed return type of getTopoData
 * Jan 15, 2015  3955     randerso  Changed TopoDatabase to extend
 *                                  IFPGridDatabase to work with ISC for
 *                                  Standard Terrain WA
 * Sep 23, 2021  8608     mapeters  Add metadata id handling
 * Jan 18, 2022  8740     randerso  Log exceptions in saveGridsToHdf5()
 *
 * </pre>
 *
 * @author randerso
 */
public class TopoDatabase extends IFPGridDatabase {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoDatabase.class);

    private TopoDatabaseManager topoMgr;

    public TopoDatabase(DatabaseID dbId, GridDbConfig gridDbConfig,
            TopoDatabaseManager topoMgr)
            throws PluginException, DataAccessLayerException {
        super(dbId, gridDbConfig);
        this.topoMgr = topoMgr;
    }

    @Override
    protected ServerResponse<?> remapAllGrids(
            Map<String, ParmStorageInfo> parmStorageInfoUser) {
        // Domain changes are handled differently for TopoDatabse.
        // See TopoDatbaseManager.createDiskCache()
        return new ServerResponse<>();
    }

    @Override
    protected List<GFERecord> saveGridsToHdf5(List<GFERecord> dataObjects,
            ParmStorageInfo parmStorageInfo) throws GfeException {
        List<GFERecord> failedRecords = new ArrayList<>(1);

        if (dataObjects.size() > 1) {
            throw new GfeException("Can (and must) save only one topo grid");
        }

        ParmID pid = this.getParmList().getPayload().get(0);

        if (parmStorageInfo == null) {
            parmStorageInfo = findStorageInfo(pid);
        }

        if (dataObjects.isEmpty()) {
            this.topoMgr.revertTopoData(
                    parmStorageInfo.getGridParmInfo().getGridLoc());
        } else {
            GFERecord record = dataObjects.get(0);
            try {
                this.topoMgr.saveTopoData(
                        parmStorageInfo.getGridParmInfo().getGridLoc(),
                        (IGridSlice) record.getMessageData(), record);
            } catch (Exception e) {
                statusHandler.error("Error saving Topo data for " + pid, e);
                failedRecords.add(record);
            }
        }

        return failedRecords;
    }

    @Override
    protected FloatDataRecord[] retrieveFromHDF5(ParmID parmId,
            List<TimeRange> times, ParmStorageInfo parmStorageInfo)
            throws GfeException {

        return this.topoMgr
                .getTopoRecord(parmStorageInfo.getGridParmInfo().getGridLoc());
    }
}
