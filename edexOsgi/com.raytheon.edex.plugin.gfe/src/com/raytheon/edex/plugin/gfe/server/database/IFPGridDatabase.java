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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmStorageInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.exception.UnknownParmIdException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * GFE Grid database containing IFP Grid data. All public methods that take a
 * ParmID or DatabaseID must first look up the corresponding version from the
 * database for use with calling private and dao methods.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 06/18/08                njensen     Added discrete/wx to getGridData()
 * 05/04/12     #574       dgilling    Restructure class to better match AWIPS1.
 * 07/11/12     15162      ryu         No raising exception in c'tor
 * 02/10/12     #1603      randerso    Implemented deleteDb, moved methods down from 
 *                                     GridDatabase that belonged here.
 *                                     Removed unnecessary conversion from Lists to/from arrays
 *                                     Added performance logging
 * 02/12/13     #1608      randerso    Changed to explicitly call deleteGroups
 * 03/07/13     #1737      njensen      Logged getGridData times
 * 03/15/13     #1795      njensen      Added updatePublishTime()
 * 03/20/13     #1774      randerso    Cleanup code to use proper constructors
 * 04/08/13     #1949      rjpeter     Updated to work with normalized database.
 * 05/02/13     #1969      randerso    Removed updateDbs from parent class
 * 06/13/13     #2044      randerso    Pass in GridDbConfig as construction parameter
 * 07/30/13     #2057      randerso    Added a static deleteDatabase method
 * 08/05/13     #1571      randerso    Refactored to store GridParmInfo and ParmStorageinfo in postgres database
 * 10/31/2013   #2508      randerso    Change to use DiscreteGridSlice.getKeys()
 * 12/10/13     #2611      randerso    Change saveGridData to set update time when saving grids
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class IFPGridDatabase extends GridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPGridDatabase.class);

    // separate logger for GFE performance logging
    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    private static final float VECTOR_DIR_DATA_MULTIPLIER = 0.5f;

    private static final float VECTOR_DIR_DATA_OFFSET = 0.0f;

    private Map<String, ParmStorageInfo> parmStorageInfo = new HashMap<String, ParmStorageInfo>();

    /** The grid configuration for this database */
    protected GridDbConfig gridDbConfig;

    private GFEDao dao;

    /**
     * Creates a new IFPGridDatabase
     * 
     * @param dbId
     *            The database ID for this database
     * @param gridDbConfig
     *            the database configuration
     */
    public IFPGridDatabase(DatabaseID dbId, GridDbConfig gridDbConfig) {
        super(dbId);
        this.gridDbConfig = gridDbConfig;
        this.valid = true;
        ServerResponse<Object> failResponse = new ServerResponse<Object>();

            try {
                // lookup actual database id row from database
                // if it doesn't exist, it will be created at this point
            this.dao = new GFEDao();

            // Make a DatabaseID and save it.
                this.dbId = dao.getDatabaseId(dbId);
            } catch (Exception e) {
            String msg = "Unable to look up database id for ifp database: "
                    + dbId;
            statusHandler.handle(Priority.PROBLEM, msg, e);
            failResponse.addMessage(msg);
            }
        if (!failInitCheck(failResponse)) {
            return;
    }

        // Get the current database configuration and store the information
        // in private data _parmInfo, _parmStorageInfo, and _areaStorageInfo
        failResponse.addMessages(getDBConfiguration());
        if (!failInitCheck(failResponse)) {
            return;
        }

        // Get the current user configuration file
        ServerResponse<Map<String, ParmStorageInfo>> ssr = getUserConfiguration();
        failResponse.addMessages(ssr);
        if (!failInitCheck(failResponse)) {
            return;
        }
        Map<String, ParmStorageInfo> parmStorageInfoUser = ssr.getPayload();

        List<String> toBeAddedParms = new ArrayList<String>();
        List<String> toBeRemovedParms = new ArrayList<String>();
        List<String> toBeChangedParms = new ArrayList<String>();

        // Compare the variables in the database with config
        compareParmInfoWithDB(parmStorageInfoUser, toBeAddedParms,
                toBeRemovedParms, toBeChangedParms);

        // handle any projection and domain change
        failResponse
                .addMessages(applyProjectionAndDomainChanges(parmStorageInfoUser));
        if (!failInitCheck(failResponse)) {
            return;
        }

        // Add in new weather elements
        failResponse.addMessages(addNewParms(toBeAddedParms,
                parmStorageInfoUser));
        if (!failInitCheck(failResponse)) {
            return;
        }

        // Remove all old parms
        removeOldParms(toBeRemovedParms);

        // accommodate changes
        changeParmCharacteristics(toBeChangedParms, parmStorageInfoUser);
    }

    private boolean failInitCheck(ServerResponse<?> failResponse) {
        if (!failResponse.isOkay()) {
            statusHandler.error("DatabaseFAIL: " + this.dbId + "\n"
                    + failResponse.getMessages());
            this.valid = false;
                }
        return this.valid;
    }

    private ServerResponse<?> addNewParms(List<String> newParms,
            Map<String, ParmStorageInfo> parmStorageUser) {
        ServerResponse<?> sr = new ServerResponse<Object>();
        if (newParms.isEmpty()) {
            return sr;
        }
        statusHandler.handle(Priority.INFO, "Creating new weather elements...");
        try {
            List<ParmStorageInfo> psis = new ArrayList<ParmStorageInfo>(
                    newParms.size());
            for (String item : newParms) {
                String[] nameLevel = splitNameAndLevel(item);
                ParmID parmId = dao.getParmId(new ParmID(nameLevel[0], dbId,
                        nameLevel[1]));
                statusHandler.debug("Adding: " + item + " to the " + this.dbId
                        + " database.");
                ParmStorageInfo psi = parmStorageUser.get(item);
                psi.getGridParmInfo().resetParmID(parmId);
                parmStorageInfo.put(item, psi);
                psis.add(psi);
            }
            dao.saveParmStorageInfo(psis);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error adding new parms", e);
            sr.addMessage("Error adding new parms");
        }
        return sr;
    }

    /**
     * Changes the parm characteristics
     * 
     * @param toBeChangedParms
     *            The parms whose info has changed
     * @param parmStorageUser
     *            The ParmStorageInfo to update to
     */
    private void changeParmCharacteristics(List<String> toBeChangedParms,
            Map<String, ParmStorageInfo> parmStorageUser) {
        for (int i = 0; i < toBeChangedParms.size(); i++) {

            String compositeName = toBeChangedParms.get(i);

            // get the current database information
            ParmStorageInfo dbPSI = this.parmStorageInfo.get(compositeName);
            GridParmInfo dbGPI = dbPSI.getGridParmInfo();

            // get the desired configuration
            ParmStorageInfo userPSI = parmStorageUser.get(compositeName);
            GridParmInfo userGPI = userPSI.getGridParmInfo();

            // update the parm info in postgres
            try {
                userGPI.setParmID(dbGPI.getParmID());
                userPSI.setId(dbPSI.getId());
                dao.updateParmStorageInfo(userPSI);
            } catch (DataAccessLayerException e) {
                statusHandler.error("Error updating parm info for "
                        + userPSI.getGridParmInfo().getParmID(), e);
                continue;
            }

            // data type change
            if (!userGPI.getGridType().equals(dbGPI.getGridType())) {
                statusHandler.handle(Priority.INFO, "Changing Data Type: "
                        + dbGPI.getParmID() + " from " + dbGPI.getGridType()
                        + " to " + userGPI.getGridType());

                changeDataType(dbGPI.getParmID());
            }

            // unit change
            if (!userGPI.getUnitString().equals(dbGPI.getUnitString())) {
                statusHandler.handle(
                        Priority.INFO,
                        "Changing Units: " + dbGPI.getParmID() + " from "
                                + dbGPI.getUnitString() + " to "
                                + userGPI.getUnitString());
                // Only changes stored unit string, does not convert values
                // This is intended to correct incorrectly labeled units not
                // change stored data.
            }

            // rate parm change
            if (userGPI.isRateParm() != dbGPI.isRateParm()) {
                statusHandler.handle(Priority.INFO, "Changing RateParm: "
                        + dbGPI.getParmID() + " from " + dbGPI.isRateParm()
                        + " to " + userGPI.isRateParm());
                // No special handling necessary
            }

            // time constraint changes
            if (!userGPI.getTimeConstraints()
                    .equals(dbGPI.getTimeConstraints())) {
                statusHandler.handle(Priority.INFO,
                        "Changing TimeConstraints: " + dbGPI.getParmID()
                                + " from " + dbGPI.getTimeConstraints()
                                + " to " + userGPI.getTimeConstraints());
                adjustTimeConstraints(dbGPI.getParmID(),
                        userGPI.getTimeConstraints());
            }

            // descriptive name change
            if (!userGPI.getDescriptiveName()
                    .equals(dbGPI.getDescriptiveName())) {
                statusHandler.handle(Priority.INFO,
                        "Changing DescriptiveName: " + dbGPI.getParmID()
                                + " from " + dbGPI.getDescriptiveName()
                                + " to " + userGPI.getDescriptiveName());
                // No special handling necessary
            }

            // precision
            if (userGPI.getPrecision() != dbGPI.getPrecision()) {
                statusHandler.handle(Priority.INFO, "Changing Precision: "
                        + dbGPI.getParmID() + " from " + dbGPI.getPrecision()
                        + " to " + userGPI.getPrecision());
                // No special handling necessary
            }

            // max/min changes
            if ((userGPI.getMaxValue() != dbGPI.getMaxValue())
                    || (userGPI.getMinValue() != dbGPI.getMinValue())
                    || !userPSI.getStorageType().equals(dbPSI.getStorageType())
                    || !userPSI.getDataType().equals(dbPSI.getDataType())
                    || (userPSI.getDataMultiplier() != dbPSI
                            .getDataMultiplier())
                    || (userPSI.getDataOffset() != dbPSI.getDataOffset())) {
                if ((userGPI.getMaxValue() != dbGPI.getMaxValue())
                        || (userGPI.getMinValue() != dbGPI.getMinValue())) {
                    statusHandler.handle(
                            Priority.INFO,
                            "Changing Max/Min: " + dbGPI.getParmID()
                                    + " from (" + dbGPI.getMinValue() + ","
                                    + dbGPI.getMaxValue() + ") to ("
                                    + userGPI.getMinValue() + ","
                                    + userGPI.getMaxValue() + ")");
                }
                if (userPSI.getDataOffset() != dbPSI.getDataOffset()) {
                    statusHandler.handle(Priority.INFO,
                            "Changing DataOffset: " + dbGPI.getParmID()
                                    + " from " + dbPSI.getDataOffset() + " to "
                                    + userPSI.getDataOffset());
                }
                if (userPSI.getDataMultiplier() != dbPSI.getDataMultiplier()) {
                    statusHandler.handle(Priority.INFO,
                            "Changing DataMultiplier: " + dbGPI.getParmID()
                                    + " from " + dbPSI.getDataMultiplier()
                                    + " to " + userPSI.getDataMultiplier());
                }
                if (!userPSI.getStorageType().equals(dbPSI.getStorageType())) {
                    statusHandler.handle(Priority.INFO,
                            "Changing StorageType: " + dbGPI.getParmID()
                                    + " from " + dbPSI.getStorageType()
                                    + " to " + userPSI.getStorageType());
                }
                changeMinMaxValues(compositeName, dbPSI, userPSI);
            }
            this.parmStorageInfo.put(compositeName, userPSI);
        }

    }

    /**
     * Changes the min/max values for a parm. This method also updates any
     * information in the HDF5 to reflect the changed values
     * 
     * @param compositeName
     *            The parm name for which the min/max values have changed
     * @param oldPSI
     *            The old ParmStorageInfo
     * @param newPSI
     *            The new ParmStorageInfo
     */
    private void changeMinMaxValues(String compositeName,
            ParmStorageInfo oldPSI, ParmStorageInfo newPSI) {

        GridParmInfo newGPI = newPSI.getGridParmInfo();

        // Make sure the specified parm is of type Scalar or Vector
        GridType gridType = newGPI.getGridType();
        if (!(gridType.equals(GridType.SCALAR) || gridType
                .equals(GridType.VECTOR))) {
            return;
        }

        ParmID parmId = null;
        List<GFERecord> records = null;

        try {
            parmId = getCachedParmID(compositeName);
            records = dao.queryByParmID(parmId);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error changing min/max values.  Error querying GFE table",
                    e);
            return;
        }

        List<GFERecord> updatedRecords = new ArrayList<GFERecord>();
        for (GFERecord rec : records) {
            switch (gridType) {
            case SCALAR:
                FloatDataRecord scalarRecord = null;
                List<TimeRange> scalarTimes = new ArrayList<TimeRange>();
                scalarTimes.add(rec.getTimeRange());
                ServerResponse<List<IGridSlice>> scalarResult = this
                        .getGridData(parmId, scalarTimes);
                if (!scalarResult.isOkay()) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error changing min/max values.  Unable to retrieve data from HDF5: "
                                    + scalarResult.toString());
                    continue;
                }
                try {
                    scalarRecord = this.retrieveFromHDF5(parmId,
                            rec.getTimeRange(), oldPSI);
                } catch (GfeException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error changing min/max values.  Unable to retrieve data from HDF5 "
                                    + e);
                    continue;
                }
                this.applyNewMinMax(scalarRecord.getFloatData(),
                        newGPI.getMinValue(), newGPI.getMaxValue());
                rec.setMessageData(scalarRecord);
                updatedRecords.add(rec);
                break;
            case VECTOR:
                List<TimeRange> vectorTimes = new ArrayList<TimeRange>();
                vectorTimes.add(rec.getTimeRange());
                FloatDataRecord[] vectorRecord = null;
                VectorGridSlice vSlice = null;

                ServerResponse<List<IGridSlice>> result = this.getGridData(
                        parmId, vectorTimes);
                if (!result.isOkay()) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error changing min/max values.  Unable to retrieve data from HDF5: "
                                    + result.toString());
                    continue;
                }
                vSlice = (VectorGridSlice) result.getPayload().get(0);
                try {
                    vectorRecord = this.retrieveVectorFromHDF5(parmId,
                            rec.getTimeRange(), oldPSI);
                } catch (GfeException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error changing min/max values.  Unable to retrieve data from HDF5",
                                    e);
                    continue;
                }
                this.applyNewMinMax(vectorRecord[0].getFloatData(),
                        newGPI.getMinValue(), newGPI.getMaxValue());
                Grid2DFloat rawData = new Grid2DFloat(newGPI.getGridLoc()
                        .getNx(), newGPI.getGridLoc().getNy(),
                        vectorRecord[0].getFloatData());
                Grid2DFloat rawData2 = new Grid2DFloat(newGPI.getGridLoc()
                        .getNx(), newGPI.getGridLoc().getNy(),
                        vectorRecord[1].getFloatData());
                vSlice.setMagGrid(rawData);
                vSlice.setDirGrid(rawData2);
                rec.setMessageData(vSlice);
                updatedRecords.add(rec);
                break;

            default:
                // do nothing
            }
        }

        if (!updatedRecords.isEmpty()) {
            try {
                this.saveGridsToHdf5(updatedRecords, newPSI);
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Error changing min/max values.  Unable to update data.",
                                e);
            }
        }

    }

    /**
     * Adjusts the time constraints for a given parm. This method simply deletes
     * any grids that are in the db for the modified ParmID
     * 
     * @param parmId
     *            The parm that needs to be modified
     * @param timeConstraints
     *            The new time constraints
     */
    private void adjustTimeConstraints(ParmID parmId,
            TimeConstraints timeConstraints) {

        // TODO: re-implement from A1

        try {
            dao.removeParmData(parmId);
        } catch (DataAccessLayerException e) {
            statusHandler.error("Error removing data for " + parmId, e);
        }
    }

    /**
     * Clamps the data values in a float array to the given min and max values
     * 
     * @param data
     *            The data to clamp
     * @param newMin
     *            The minimum allowed value
     * @param newMax
     *            The maximum allowed value
     */
    private void applyNewMinMax(float[] data, float newMin, float newMax) {
        for (int i = 0; i < data.length; i++) {
            if (data[i] < newMin) {
                data[i] = newMin;
            } else if (data[i] > newMax) {
                data[i] = newMax;
            }
        }
    }

    /**
     * Changes the data type in the database for a given parm
     * 
     * Removes the data for the specified parm
     * 
     * @param parmId
     *            The affected parm
     */
    private void changeDataType(ParmID parmId) {
        try {
            dao.removeParmData(parmId);
        } catch (DataAccessLayerException e) {
            statusHandler.error("Error removing data for " + parmId, e);
        }
    }

    /**
     * Removes the parm in the database and HDF5 repository for the list of
     * parms.
     * 
     * @param parms
     *            The list of parms to delete
     */
    private void removeOldParms(List<String> parms) {
            for (String item : parms) {
                statusHandler.handle(Priority.INFO, "Removing: " + item
                        + " from the " + this.dbId + " database.");
                try {
                    // Remove the entire data structure for the parm
                dao.removeParm(parmStorageInfo.get(item).getParmID());
                // parmIdMap.remove(item);
                    this.parmStorageInfo.remove(item);
                } catch (DataAccessLayerException e) {
                    statusHandler.handle(Priority.PROBLEM, "Error removing: "
                            + item + " from the database");
                }
            }
        }

    @Override
    public ServerResponse<List<ParmID>> getParmList() {
        // List<ParmID> parmIds = new ArrayList<ParmID>(parmIdMap.values());
        List<ParmID> parmIds = new ArrayList<ParmID>(parmStorageInfo.size());
        for (ParmStorageInfo psi : parmStorageInfo.values()) {
            parmIds.add(psi.getParmID());
        }
        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();
        sr.setPayload(parmIds);
        return sr;
    }

    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id) {

        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        try {
            sr.setPayload(dao.getTimes(getCachedParmID(id)));
        } catch (Exception e) {
            sr.setPayload(new ArrayList<TimeRange>(0));
            statusHandler.handle(Priority.PROBLEM, "Unable to get times for: "
                    + id.getParmName() + "_" + id.getParmLevel(), e);
            sr.addMessage("Unable to get times for: " + id.getParmName() + "_"
                    + id.getParmLevel());
        }
        return sr;
    }

    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id,
            TimeRange tr) {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        try {
            sr.setPayload(dao.getOverlappingTimes(getCachedParmID(id), tr));
        } catch (Exception e) {
            sr.setPayload(new ArrayList<TimeRange>(0));
            statusHandler.handle(Priority.PROBLEM, "Unable to get times for: "
                    + id.getParmName() + "_" + id.getParmLevel(), e);
            sr.addMessage("Unable to get times for: " + id.getParmName() + "_"
                    + id.getParmLevel());
        }
        return sr;
    }

    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> trs) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();
        try {
            Map<TimeRange, List<GridDataHistory>> history = dao.getGridHistory(
                    getCachedParmID(id), trs);
            sr.setPayload(history);
        } catch (DataAccessLayerException e) {
            sr.addMessage("Error getting grid history for: " + id + "\n"
                    + e.getLocalizedMessage());
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting grid history for: " + id, e);
        } catch (UnknownParmIdException e) {
            statusHandler.handle(Priority.PROBLEM, "Unknown parmId: " + id, e);
        }

        return sr;
    }

    /**
     * Save the specified gridSlices over the time period specified by
     * originalTimeRange in the grid database.
     * 
     * Make sure that the database is valid and that the specified parm is known
     * to this database. If not return an error ServerResponse. Call calculate
     * overlap to see where the new grids fit amongst the old grids. Make sure
     * that the gridTimes for each grid is valid and that there is enough room
     * in the database to store the grids. If not, return an error
     * ServerResponse. Next remove any grids that need to be removed by setting
     * Their useTable entries to false. Then call storeGridSlices to put the
     * grids in the database. The SeqOf<TimeInfo> must then be inserted into the
     * timeInfo array. Write the new timeInfo array to the database and return.
     * 
     * @param id
     *            The parm ID to save
     * @param originalTimeRange
     *            The time range to save
     * @param records
     *            The records to save
     * @param requesterId
     *            who requested to save the grids
     * @return The server response
     */
    @Override
    public ServerResponse<?> saveGridData(ParmID id,
            TimeRange originalTimeRange, List<GFERecord> records,
            WsId requesterId) {
        ServerResponse<?> sr = new ServerResponse<String>();
        ParmID dbParmId = null;
        try {
            dbParmId = getCachedParmID(id);
        } catch (UnknownParmIdException e) {
            sr.addMessage(e.getLocalizedMessage());
            return sr;
        }

        return this.saveGridData(dbParmId, originalTimeRange, records,
                requesterId, null);
    }

    /**
     * Save the specified gridSlices over the time period specified by
     * originalTimeRange in the grid database.
     * 
     * Make sure that the database is valid and that the specified parm is known
     * to this database. If not return an error ServerResponse. Call calculate
     * overlap to see where the new grids fit amongst the old grids. Make sure
     * that the gridTimes for each grid is valid and that there is enough room
     * in the database to store the grids. If not, return an error
     * ServerResponse. Next remove any grids that need to be removed by setting
     * Their useTable entries to false. Then call storeGridSlices to put the
     * grids in the database. The SeqOf<TimeInfo> must then be inserted into the
     * timeInfo array. Write the new timeInfo array to the database and return.
     * 
     * @param id
     *            The database parm ID to save
     * @param originalTimeRange
     *            The time range to save
     * @param recordsToSave
     *            The records to save, records are not db records at this point.
     * @param requesterId
     *            who requested to save the grids
     * @param skipDelete
     *            time ranges to not delete
     * @return The server response
     */
    private ServerResponse<?> saveGridData(ParmID id,
            TimeRange originalTimeRange, List<GFERecord> recordsToSave,
            WsId requesterId, List<TimeRange> skipDelete) {
        ServerResponse<?> sr = dbIsValid();

        if (!sr.isOkay()) {
            return sr;
        }

        long t0 = System.currentTimeMillis();
        Map<TimeRange, GFERecord> existingMap = null;

        try {
            // Grab the current records for the general time range in question
            existingMap = dao.getOverlappingRecords(id, originalTimeRange);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to lookup existing GFE Records", e);
            sr.addMessage("Unable to lookup existing GFE Records.  Data not saved");
            return sr;
        }

        long t1 = System.currentTimeMillis();
        perfLog.logDuration("Save: Retrieve overlapping grids", (t1 - t0));

        // Determine records/times to delete
        Set<TimeRange> timesToDelete = new HashSet<TimeRange>(
                existingMap.keySet());
        if (skipDelete != null) {
            timesToDelete.removeAll(skipDelete);
        }

        for (GFERecord recToSave : recordsToSave) {
            timesToDelete.remove(recToSave.getTimeRange());
        }

        if (!timesToDelete.isEmpty()) {
            try {
                List<GFERecord> recsToDelete = new ArrayList<GFERecord>(
                        timesToDelete.size());
                for (TimeRange timeToRemove : timesToDelete) {
                    recsToDelete.add(existingMap.get(timeToRemove));
                }

                dao.deleteRecords(recsToDelete);
                removeFromHDF5(id, new ArrayList<TimeRange>(timesToDelete));
                existingMap.keySet().removeAll(timesToDelete);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to delete existing GFE Records", e);
                sr.addMessage("Unable to delete existing GFE Records.  Data not saved");
                return sr;
            }
        }

        long t2 = System.currentTimeMillis();
        perfLog.logDuration("Save: Removing " + timesToDelete.size()
                + " existing grids", (t2 - t1));

        // begin consolidation of new vs update
        List<TimeRange> timesToBeSaved = new ArrayList<TimeRange>(
                recordsToSave.size());
        for (GFERecord recToSave : recordsToSave) {
            timesToBeSaved.add(recToSave.getTimeRange());
        }

        // theoretically shouldn't be needed, but skipDelete list requires it
        // since the delete list may have differed
        existingMap.keySet().retainAll(timesToBeSaved);
        List<GFERecord> newRecords = new ArrayList<GFERecord>(
                timesToBeSaved.size() - existingMap.size());

        // track merge with existing records or add to new list
        for (GFERecord recToSave : recordsToSave) {
            // modify update time for non ISC/Official db
            if (!this.dbId.getModelName().equals("ISC")
                    && !this.dbId.getModelName().equals("Official")) {
                Date nowTime = SimulatedTime.getSystemTime().getTime();
                for (GridDataHistory history : recToSave.getGridHistory()) {
                    history.setUpdateTime(nowTime);
                }
            }
            TimeRange tr = recToSave.getTimeRange();
            GFERecord existing = existingMap.get(tr);
            if (existing != null) {
                existing.setMessageData(recToSave.getMessageData());
                existing.consolidateHistory(recToSave.getGridHistory());
            } else {
                // set the parmId to the cache'd instance
                recToSave.setParmId(id);
                newRecords.add(recToSave);
            }
        }

        boolean hdf5SaveSuccess = false;
        List<GFERecord> failedGrids = Collections.emptyList();
        Map<TimeRange, List<GridDataHistory>> histories = new HashMap<TimeRange, List<GridDataHistory>>(
                recordsToSave.size(), 1);

        try {
            List<GFERecord> recList = new ArrayList<GFERecord>(
                    existingMap.size() + newRecords.size());
            recList.addAll(newRecords);
            recList.addAll(existingMap.values());
            failedGrids = saveGridsToHdf5(recList, null);
            hdf5SaveSuccess = true;
        } catch (GfeException e) {
            statusHandler.handle(Priority.PROBLEM, "Error saving to hdf5", e);
            sr.addMessage("Error accessing HDF5.  Data not saved.");
        }

        long t3 = System.currentTimeMillis();
        perfLog.logDuration(
                "Saving " + (existingMap.size() + newRecords.size()) + " "
                        + id.getParmName() + " grids to hdf5", (t3 - t2));

        if (hdf5SaveSuccess) {
            // Save off the individual failures (if any), and then save what we
            // can
            for (GFERecord gfeRecord : failedGrids) {
                sr.addMessage("Failed to save grid to HDF5: " + gfeRecord);
                GFERecord rec = existingMap.remove(gfeRecord.getTimeRange());

                if (rec == null) {
                    newRecords.remove(gfeRecord);
                }
            }

            if (!newRecords.isEmpty()) {
                try {
                    dao.save(newRecords);
                    for (GFERecord rec : newRecords) {
                        histories.put(rec.getTimeRange(), rec.getGridHistory());
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error saving new grids to database", e);
                    for (GFERecord rec : newRecords) {
                        // already logged at a lower level
                        String msg = "Error saving new grid to database: "
                                + rec.toString();
                        sr.addMessage(msg);
                        removeFromHDF5(rec);
                    }
                }
            }

            if (!existingMap.isEmpty()) {
                try {
                    dao.update(existingMap.values());

                    for (GFERecord rec : existingMap.values()) {
                        histories.put(rec.getTimeRange(), rec.getGridHistory());
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating existing grids in database", e);
                    for (GFERecord rec : existingMap.values()) {
                        // already logged at a lower level
                        String msg = "Error updating grid in database: "
                                + rec.toString();
                        sr.addMessage(msg);
                    }
                }
            }
        }
        long t4 = System.currentTimeMillis();
        perfLog.logDuration(
                "Saving " + (existingMap.size() + newRecords.size()) + " "
                        + id.getParmName() + " grids to database", (t4 - t3));

        sr.addNotifications(new GridUpdateNotification(id, originalTimeRange,
                histories, requesterId, id.getDbId().getSiteId()));
        return sr;
    }

    @Override
    public ServerResponse<?> saveGridSlices(ParmID parmId,
            TimeRange originalTimeRange, List<IGridSlice> slices,
            WsId requesterId, List<TimeRange> skipDelete) {
        ServerResponse<?> sr = new ServerResponse<String>();
        ParmID dbParmId = null;

        try {
            dbParmId = getCachedParmID(parmId);
        } catch (UnknownParmIdException e) {
            sr.addMessage(e.getLocalizedMessage());
            return sr;
        }

        List<GFERecord> records = new ArrayList<GFERecord>();
        for (IGridSlice slice : slices) {
            GFERecord rec = new GFERecord(dbParmId, slice.getValidTime());
            rec.setGridHistory(slice.getHistory());
            rec.setMessageData(slice);
            records.add(rec);
        }

        sr.addMessages(this.saveGridData(dbParmId, originalTimeRange, records,
                requesterId, skipDelete));
        return sr;
    }

    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        String compositeName = id.getCompositeName();
        ParmStorageInfo psi = parmStorageInfo.get(compositeName);
        if (psi == null) {
            sr.addMessage("GridParmInfoNotFound - " + compositeName
                    + " was not found in the database");
        } else {
            GridParmInfo gpi = psi.getGridParmInfo();
            sr.setPayload(gpi);
        }
        return sr;
    }

    private ServerResponse<List<IGridSlice>> getGridData(ParmID parmId,
            List<TimeRange> timeRanges, GridLocation location) {
        List<IGridSlice> data = new ArrayList<IGridSlice>(timeRanges.size());
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        ServerResponse<?> ssr = this.dbIsValid();
        sr.addMessages(ssr);

        ParmStorageInfo psi = this.parmStorageInfo.get(parmId
                .getCompositeName());
        GridParmInfo gpi = psi.getGridParmInfo().clone();
        gpi.setGridLoc(location);
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> ssr2 = getGridHistory(
                parmId, timeRanges);
        if (!ssr2.isOkay()) {
            sr.addMessages(ssr2);
            return sr;
        }
        Map<TimeRange, List<GridDataHistory>> historyMap = ssr2.getPayload();

        String siteId = parmId.getDbId().getSiteId();
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        IGridSlice slice = null;
        Grid2DFloat rawData = null;
        Grid2DFloat rawData2 = null;
        int i = 0;
        switch (gpi.getGridType()) {
        case SCALAR:
            FloatDataRecord[] records = null;
            try {
                records = this.retrieveFromHDF5(parmId, timeRanges, psi);
            } catch (GfeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving SCALAR data from HDF5", e);
                sr.addMessage("Error retrieving SCALAR data from HDF5");
                return sr;
            }
            i = 0;
            for (TimeRange time : timeRanges) {
                List<GridDataHistory> history = historyMap.get(time);
                rawData = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                        .getGridLoc().getNy(), records[i++].getFloatData());
                slice = new ScalarGridSlice(time, gpi,
                        history.toArray(new GridDataHistory[history.size()]),
                        rawData);
                data.add(slice);
            }
            break;
        case VECTOR:
            FloatDataRecord[][] vecRecords = null;
            try {
                vecRecords = this.retrieveVectorFromHDF5(parmId, timeRanges,
                        psi);
            } catch (GfeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving VECTOR data from HDF5", e);
                sr.addMessage("Error retrieving VECTOR data from HDF5");
                return sr;
            }
            i = 0;
            for (TimeRange time : timeRanges) {
                List<GridDataHistory> history = historyMap.get(time);
                rawData = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                        .getGridLoc().getNy(), vecRecords[i][0].getFloatData());
                rawData2 = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                        .getGridLoc().getNy(), vecRecords[i][1].getFloatData());
                slice = new VectorGridSlice(time, gpi,
                        history.toArray(new GridDataHistory[history.size()]),
                        rawData, rawData2);
                data.add(slice);
                i++;
            }
            break;
        case DISCRETE:
            ByteDataRecord[][] discreteRecords = null;
            try {
                discreteRecords = this.retrieveDiscreteFromHDF5(parmId,
                        timeRanges);
            } catch (GfeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving DISCRETE data from HDF5", e);
                sr.addMessage("Error retrieving DISCRETE data from HDF5");
                return sr;
            }
            i = 0;
            for (TimeRange time : timeRanges) {
                List<GridDataHistory> history = historyMap.get(time);
                Grid2DByte rawDiscrete = new Grid2DByte(gpi.getGridLoc()
                        .getNx(), gpi.getGridLoc().getNy(),
                        discreteRecords[i][0].getByteData());
                byte[] discreteKeyData = discreteRecords[i][1].getByteData();
                String discreteKeysString = new String(discreteKeyData);
                String[] discreteSplit = discreteKeysString.split(Pattern
                        .quote(GfeUtil.KEY_SEPARATOR));

                DiscreteKey[] discreteKeyArray = new DiscreteKey[discreteSplit.length];
                for (int j = 0; j < discreteSplit.length; j++) {
                    discreteKeyArray[j] = new DiscreteKey(siteId,
                            discreteSplit[j], parmId);
                }
                slice = new DiscreteGridSlice(time, gpi,
                        history.toArray(new GridDataHistory[history.size()]),
                        rawDiscrete, discreteKeyArray);
                data.add(slice);
                i++;
            }
            break;
        case WEATHER:
            ByteDataRecord[][] wxRecords = null;
            try {
                wxRecords = this.retrieveDiscreteFromHDF5(parmId, timeRanges);
            } catch (GfeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving WEATHER data from HDF5", e);
                sr.addMessage("Error retrieving WEATHER data from HDF5");
                return sr;
            }
            i = 0;
            for (TimeRange time : timeRanges) {
                List<GridDataHistory> history = historyMap.get(time);
                Grid2DByte rawWx = new Grid2DByte(gpi.getGridLoc().getNx(), gpi
                        .getGridLoc().getNy(), wxRecords[i][0].getByteData());
                byte[] wxKeyData = wxRecords[i][1].getByteData();
                String wxKeysString = new String(wxKeyData);
                String[] wxSplit = wxKeysString.split(Pattern
                        .quote(GfeUtil.KEY_SEPARATOR));
                WeatherKey[] keyArray = new WeatherKey[wxSplit.length];
                for (int j = 0; j < wxSplit.length; j++) {
                    keyArray[j] = new WeatherKey(siteId, wxSplit[j]);
                }
                slice = new WeatherGridSlice(time, gpi,
                        history.toArray(new GridDataHistory[history.size()]),
                        rawWx, keyArray);
                data.add(slice);
                i++;
            }
            break;
        default:
            break;
        }
        timer.stop();
        perfLog.logDuration("Retrieving " + timeRanges.size()
                + " records from hdf5", timer.getElapsedTime());
        sr.setPayload(data);
        return sr;
    }

    @Override
    public ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges) {
        ParmID dbParmId = null;
        try {
            dbParmId = getCachedParmID(id);
        } catch (UnknownParmIdException e) {
            ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
            sr.addMessage(e.getLocalizedMessage());
            return sr;
        }

        return this.getGridData(dbParmId, timeRanges, getGridParmInfo(dbParmId)
                .getPayload().getGridLoc());

    }

    @Override
    public String getProjectionId() {
        return this.gridDbConfig.projectionData().getProjectionID();
    }

    /**
     * Examines the user vs. the database projection and domains, and determines
     * if the database needs to be remapped to be in line with the desired
     * projection and domain. Performs the remapping of all data as needed.
     * 
     * @param parmStorageInfoUser
     * @return ServerResponse containing status only
     */
    private ServerResponse<?> applyProjectionAndDomainChanges(
            Map<String, ParmStorageInfo> parmStorageInfoUser) {
        ServerResponse<?> sr = new ServerResponse<Object>();

        // get database GridLocation, if any
        GridLocation glocDb = null;
        if (!parmStorageInfo.isEmpty()) {
            glocDb = parmStorageInfo.values().iterator().next()
                    .getGridParmInfo().getGridLoc();
        }

        // get user GridLocation
        GridLocation glocUser = parmStorageInfoUser.values().iterator().next()
                .getGridParmInfo().getGridLoc();

        // get the hibernate ids set correctly
        glocUser.setDbId(this.dbId);
        if (glocDb != null) {
            glocUser.setId(glocDb.getId());
        }

        // if GridLocation has changed
        if (!glocUser.equals(glocDb)) {

            // save/update the database GridLocation
        try {
                dao.saveOrUpdateGridLocation(glocUser);

                // remap the actual gridded data to the new gridLocation
                statusHandler.info("Remapping Grids to new domain: "
                        + this.dbId);
                sr.addMessages(remapAllGrids(parmStorageInfoUser));
            } catch (DataAccessLayerException e) {
                String msg = "Error storing gridLocation for " + this.dbId;
                statusHandler.error(msg, e);
                sr.addMessage(msg);
                return sr;
            }
        }

        // update parmStorageInfoUser with databased GridLocation
        Iterator<ParmStorageInfo> iter = parmStorageInfoUser.values()
                .iterator();
        while (iter.hasNext()) {
            ParmStorageInfo psi = iter.next();
            psi.getGridParmInfo().setGridLoc(glocUser);
        }

        return sr;
    }

    private ServerResponse<?> remapAllGrids(
            Map<String, ParmStorageInfo> parmStorageInfoUser) {
        ServerResponse<?> sr = new ServerResponse<Object>();

        for (Entry<String, ParmStorageInfo> entry : this.parmStorageInfo
                .entrySet()) {
            String compositeName = entry.getKey();

            ParmStorageInfo oldPSI = entry.getValue();
            GridParmInfo oldGPI = oldPSI.getGridParmInfo();
            ParmStorageInfo newPSI = parmStorageInfoUser.get(compositeName);
            if (newPSI == null) {
                continue; // this parm not in new database, so skip
                    }

            GridParmInfo newGPI = newPSI.getGridParmInfo();

            GridLocation oldGL = oldGPI.getGridLoc();
            GridLocation newGL = newGPI.getGridLoc();
            RemapGrid remapper = new RemapGrid(oldGL, newGL);

            List<GFERecord> records;
            try {
                ParmID currentParm = getCachedParmID(compositeName);
                records = dao.queryByParmID(currentParm);
            } catch (UnknownParmIdException e) {
                statusHandler.error("Unable to retrieve ParmID for "
                        + compositeName, e);
                continue;
            } catch (DataAccessLayerException e) {
                statusHandler.error("Unable to retrieve GFERecords for "
                        + compositeName, e);
                continue;
                    }

            // process each grid
                    for (GFERecord rec : records) {
                        List<TimeRange> times = new ArrayList<TimeRange>();
                        times.add(rec.getTimeRange());
                ServerResponse<List<IGridSlice>> ssr = this.getGridData(
                        rec.getParmId(), times, oldGL);
                sr.addMessages(ssr);
                if (!sr.isOkay()) {
                    statusHandler
                            .error("Unable to retrieve data in remapAllGrids() "
                                    + sr.getMessages());
                    continue;
                }
                IGridSlice slice = ssr.getPayload().get(0);
                        IGridSlice newSlice = null;
                        try {
                            switch (slice.getGridInfo().getGridType()) {
                            case NONE:
                                break;
                            case SCALAR:
                                ScalarGridSlice scalarSlice = (ScalarGridSlice) slice;
                        Grid2DFloat newGrid = remapper.remap(scalarSlice
                                .getScalarGrid(), scalarSlice.getGridInfo()
                                .getMinValue(), scalarSlice.getGridInfo()
                                .getMaxValue(), scalarSlice.getGridInfo()
                                .getMinValue(), scalarSlice.getGridInfo()
                                                        .getMinValue());
                                scalarSlice.setScalarGrid(newGrid);
                                newSlice = scalarSlice;
                                break;
                            case VECTOR:
                                VectorGridSlice vectorSlice = (VectorGridSlice) slice;
                        Grid2DFloat magOutput = new Grid2DFloat(newGL.getNx(),
                                newGL.getNy());
                        Grid2DFloat dirOutput = new Grid2DFloat(newGL.getNx(),
                                newGL.getNy());
                        remapper.remap(vectorSlice.getMagGrid(), vectorSlice
                                .getDirGrid(), vectorSlice.getGridInfo()
                                .getMinValue(), vectorSlice.getGridInfo()
                                .getMaxValue(), vectorSlice.getGridInfo()
                                .getMinValue(), vectorSlice.getGridInfo()
                                .getMinValue(), magOutput, dirOutput);
                                vectorSlice.setDirGrid(dirOutput);
                                vectorSlice.setMagGrid(magOutput);
                                newSlice = vectorSlice;
                                break;
                            case WEATHER:
                                WeatherGridSlice weatherSlice = (WeatherGridSlice) slice;
                        Grid2DByte newWeatherGrid = remapper.remap(
                                        weatherSlice.getWeatherGrid(), 0, 0);
                                weatherSlice.setWeatherGrid(newWeatherGrid);
                                newSlice = weatherSlice;
                                break;
                            case DISCRETE:
                                DiscreteGridSlice discreteSlice = (DiscreteGridSlice) slice;
                        Grid2DByte newDiscreteGrid = remapper.remap(
                                        discreteSlice.getDiscreteGrid(), 0, 0);
                                discreteSlice.setDiscreteGrid(newDiscreteGrid);
                                newSlice = discreteSlice;
                                break;
                            }
                    newSlice.setGridInfo(newGPI);
                            rec.setMessageData(newSlice);
                            this.removeFromHDF5(rec);
                    this.saveGridsToHdf5(Arrays.asList(rec), newPSI);
                        } catch (Exception e) {
                            statusHandler.handle(Priority.PROBLEM,
                            "Error remapping data for record [" + rec + "]", e);
                        }
                    }
                }

        return sr;
            }

    private ServerResponse<?> getDBConfiguration() {
        ServerResponse<?> sr = new ServerResponse<Object>();

        try {
            List<ParmStorageInfo> parmInfoList = dao.getParmStorageInfo(dbId);
            parmStorageInfo = new HashMap<String, ParmStorageInfo>(
                    parmInfoList.size(), 1.0f);
            // parmIdMap = new HashMap<String, ParmID>(parmInfoList.size(),
            // 1.0f);

            for (ParmStorageInfo psi : parmInfoList) {
                ParmID pid = psi.getParmID();
                String compositeName = pid.getCompositeName();
                // parmIdMap.put(compositeName, pid);
                parmStorageInfo.put(compositeName, psi);
            }
        } catch (DataAccessLayerException e) {
            parmStorageInfo = Collections.emptyMap();
            // parmIdMap = Collections.emptyMap();
            String msg = "Error retrieving parm info from Database: "
                    + e.getLocalizedMessage();
            statusHandler.error(msg, e);
            sr.addMessage(msg);
            }
        return sr;
        }

    private void compareParmInfoWithDB(
            Map<String, ParmStorageInfo> parmStorageInfoUser,
            List<String> toBeAdded, List<String> toBeDeleted,
            List<String> toBeChanged) {

        // reset the returned list
        toBeAdded.clear();
        toBeDeleted.clear();
        toBeChanged.clear();

        // Check for entries to be added
        for (String key : parmStorageInfoUser.keySet()) {
            if (!this.parmStorageInfo.containsKey(key)) {
                toBeAdded.add(key);
            }
        }

        // check for entries to be deleted
        for (String key : this.parmStorageInfo.keySet()) {
            if (!parmStorageInfoUser.containsKey(key)) {
                toBeDeleted.add(key);
            }
        }

        // Check for entries to modified
        for (String key : parmStorageInfoUser.keySet()) {
            ParmStorageInfo dbPsi, userPsi = null;
            if (this.parmStorageInfo.containsKey(key)
                    && !toBeChanged.contains(key)) {
                dbPsi = this.parmStorageInfo.get(key);
                userPsi = parmStorageInfoUser.get(key);
                if (!dbPsi.equals(userPsi)) {
                    toBeChanged.add(key);
                }
            }
        }
    }

    /**
     * IFPGridDatabase::getUserConfiguration() This function, which is called at
     * construction time, reads all the information from the supplied
     * GridDbConfig class and creates information in the calling arguments
     * parmStorageInfo. These dictionaries are used later by other functions in
     * this class.
     * 
     * @return ServerResponse containing the user configuration
     */
    private ServerResponse<Map<String, ParmStorageInfo>> getUserConfiguration() {
        ServerResponse<Map<String, ParmStorageInfo>> sr = new ServerResponse<Map<String, ParmStorageInfo>>();
        Map<String, ParmStorageInfo> parmStorageInfoUser = new HashMap<String, ParmStorageInfo>();

        if (this.gridDbConfig == null) {
            sr.addMessage("No gridDbConfig found for " + this.dbId);
            return sr;
        }

        // For each variable get the info and stuff it in the correct object
        for (String nameLevel : this.gridDbConfig.parmAndLevelList()) {
            ParmStorageInfo psi = getUserConfigForWE(nameLevel);

            if (psi != null) {
                parmStorageInfoUser.put(nameLevel, psi);
            } else {
                String msg = "ParmStorageInfo not found in configuration for "
                        + nameLevel;
                statusHandler.error(msg);
                sr.addMessage("msg");
                return sr;
            }
        }
        sr.setPayload(parmStorageInfoUser);
        return sr;
    }

    /**
     * This function reads the ParmStorageInfo from the GridDBConfig for a
     * single weather element (compositeName).
     * 
     * @param compositeName
     *            the desired weather element
     * @return the ParmStorageInfo
     */
    private ParmStorageInfo getUserConfigForWE(String compositeName) {

        String[] nameLevel = splitNameAndLevel(compositeName);
        ParmStorageInfo psi = null;

        if (gridDbConfig == null) {
            statusHandler.handle(Priority.DEBUG, toString()
                    + " has no GridParmInfo config");
            return null;
        } else {
            psi = this.gridDbConfig.getParmStorageInfo(nameLevel[0],
                nameLevel[1]);
        if (psi == null) {
            statusHandler.handle(Priority.DEBUG, compositeName
                    + " not found in ParmStorageInfo config");
                return null;
        }
        }

        psi.getGridParmInfo().resetParmID(
                new ParmID(nameLevel[0], this.dbId, nameLevel[1]));

        return psi;
    }

    /**
     * @param parmAndLevel
     * @return string array containing parm name and level
     */
    public String[] splitNameAndLevel(String parmAndLevel) {

        String[] retValue = parmAndLevel.split("_");

        if (retValue.length == 1) {
            return new String[] { retValue[0], ParmID.defaultLevel() };
        } else {
            return retValue;
        }
    }

    private ServerResponse<?> dbIsValid() {
        ServerResponse<?> sr = new ServerResponse<String>();
        if ((dbId == null) || !dbId.isValid()) {
            sr.addMessage("DBInvalid - The database is not valid.");
        }
        return sr;
    }

    /**
     * Saves GFERecords to the HDF5 repository
     * 
     * @param dataObjects
     *            The GFERecords to be saved
     * @param parmStorageInfo
     *            the parameter storage info
     * @return Returns records that failed to store
     * @throws GfeException
     *             If errors occur during the interaction with the HDF5
     *             repository
     */
    private List<GFERecord> saveGridsToHdf5(List<GFERecord> dataObjects,
            ParmStorageInfo parmStorageInfo) throws GfeException {
        List<GFERecord> failedGrids = new ArrayList<GFERecord>();
        try {
            StorageProperties sp = null;
            String compression = PluginRegistry.getInstance()
                    .getRegisteredObject("gfe").getCompression();
            if (compression != null) {
                sp = new StorageProperties();
                sp.setCompression(StorageProperties.Compression
                        .valueOf(compression));
            }

            Map<File, List<GFERecord>> recordMap = new HashMap<File, List<GFERecord>>();

            for (GFERecord rec : dataObjects) {
                File file = GfeUtil.getHdf5File(gfeBaseDataDir,
                        rec.getParmId(), rec.getTimeRange());
                List<GFERecord> recList = recordMap.get(file);
                if (recList == null) {
                    recList = new ArrayList<GFERecord>();
                    recordMap.put(file, recList);
                }
                recList.add(rec);
            }

            for (Entry<File, List<GFERecord>> entry : recordMap.entrySet()) {
                IDataStore dataStore = DataStoreFactory.getDataStore(entry
                        .getKey());
                // The correlation map is used to detect which gferecord had
                // a problem if an error occurs
                Map<IDataRecord, GFERecord> correlationMap = new HashMap<IDataRecord, GFERecord>();
                for (GFERecord rec : entry.getValue()) {
                    Object data = rec.getMessageData();
                    String groupName = GfeUtil.getHDF5Group(rec.getParmId(),
                            rec.getTimeRange());

                    if (parmStorageInfo == null) {
                        parmStorageInfo = findStorageInfo(rec.getParmId());
                    }
                    // Get storage info (for float and vector data)
                    String storageType = parmStorageInfo.getStorageType();

                    if ((data instanceof FloatDataRecord)
                            && !"float".equals(storageType)) {
                        storeConvertedFloatRecord((FloatDataRecord) data,
                                dataStore, sp, groupName, parmStorageInfo,
                                correlationMap, rec);
                    } else if (data instanceof IDataRecord) {
                        // store without conversion
                        ((IDataRecord) data).setGroup(groupName);
                        dataStore.addDataRecord((IDataRecord) data, sp);
                        correlationMap.put(((IDataRecord) data), rec);
                    } else if (data instanceof VectorGridSlice) {
                        storeVectorGridSlice((VectorGridSlice) data, dataStore,
                                sp, groupName, parmStorageInfo, correlationMap,
                                rec);
                    } else if (data instanceof ScalarGridSlice) {
                        storeScalarGridSlice((ScalarGridSlice) data, dataStore,
                                sp, groupName, parmStorageInfo, correlationMap,
                                rec);
                    } else if (data instanceof DiscreteGridSlice) {
                        storeDiscreteGridSlice((DiscreteGridSlice) data,
                                dataStore, sp, groupName, parmStorageInfo,
                                correlationMap, rec);
                    } else if (data instanceof WeatherGridSlice) {
                        storeWeatherGridSlice((WeatherGridSlice) data,
                                dataStore, sp, groupName, parmStorageInfo,
                                correlationMap, rec);
                    }
                }

                long t0 = System.currentTimeMillis();
                StorageStatus ss = dataStore.store(StoreOp.REPLACE);
                long t1 = System.currentTimeMillis();
                perfLog.logDuration("Storing " + entry.getValue().size()
                        + " records to hdf5", (t1 - t0));
                StorageException[] exceptions = ss.getExceptions();
                if ((exceptions != null) && (exceptions.length > 0)) {
                    // Describe the errors, then
                    // only log the first one, don't flood the log with
                    // duplicates.
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Storage exceptions occurred during hdf5 save.  "
                                            + exceptions.length
                                            + " errors occurred.  The first failure will be logged.");
                    boolean first = true;
                    for (StorageException se : exceptions) {
                        IDataRecord recordWithException = se.getRecord();
                        GFERecord rec = correlationMap.get(recordWithException);
                        failedGrids.add(rec);
                        if (first) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Failed to save grid: ", se);
                            first = false;
                        }
                    }
                }

            }

        } catch (StorageException e) {
            throw new GfeException("Error storing to HDF5", e);
        }
        return failedGrids;
    }

    /**
     * Store a grid slice of scalar data.
     * 
     * @param slice
     *            The scalar grid slice to store.
     * @param dataStore
     *            The data store in which to put the slice.
     * @param sp
     *            The storage properties
     * @param groupName
     *            The group name under which to store the slice.
     * @param parmStorageInfo
     *            A structure that describes conversions on the data.
     * @param correlationMap
     *            Used for failed storage diagnostics.
     * @param rec
     *            The GFERecord the slice is associated with.
     * @throws StorageException
     */
    protected void storeScalarGridSlice(ScalarGridSlice slice,
            IDataStore dataStore, StorageProperties sp, String groupName,
            ParmStorageInfo parmStorageInfo,
            Map<IDataRecord, GFERecord> correlationMap, GFERecord rec)
            throws StorageException {
        if (slice.getScalarGrid() != null) {
            float[] rawData = slice.getScalarGrid().getFloats();
            FloatDataRecord rawRecord = new FloatDataRecord("Data", groupName,
                    rawData, 2, new long[] {
                            slice.getGridInfo().getGridLoc().getNx(),
                            slice.getGridInfo().getGridLoc().getNy() });
            this.storeConvertedFloatRecord(rawRecord, dataStore, sp, groupName,
                    parmStorageInfo, correlationMap, rec);
        }
    }

    /**
     * Store a grid slice of vector data.
     * 
     * @param slice
     *            The vector grid slice
     * @param dataStore
     *            The data store in which to save the slice
     * @param sp
     *            The storage properties
     * @param groupName
     *            The group name under which to save the slice
     * @param parmStorageInfo
     *            A structure which describes conversions to be done at
     *            storage/retrieval time
     * @param correlationMap
     *            Used for failed save diagnostics
     * @param rec
     *            The GFERecord associated with the slice
     * @throws StorageException
     */
    protected void storeVectorGridSlice(VectorGridSlice slice,
            IDataStore dataStore, StorageProperties sp, String groupName,
            ParmStorageInfo parmStorageInfo,
            Map<IDataRecord, GFERecord> correlationMap, GFERecord rec)
            throws StorageException {
        if ((slice.getMagGrid() != null) || (slice.getDirGrid() != null)) {
            float[] rawMagData = slice.getMagGrid().getFloats();
            float[] rawDirData = slice.getDirGrid().getFloats();
            FloatDataRecord rawMagRecord = new FloatDataRecord("Mag",
                    groupName, rawMagData, 2, new long[] {
                            slice.getGridInfo().getGridLoc().getNx(),
                            slice.getGridInfo().getGridLoc().getNy() });

            FloatDataRecord rawDirRecord = new FloatDataRecord("Dir",
                    groupName, rawDirData, 2, new long[] {
                            slice.getGridInfo().getGridLoc().getNx(),
                            slice.getGridInfo().getGridLoc().getNy() });

            // Direction grids use the same storage type as the mag grid,
            // but offset is always 0 and multiplier is always 0.5.
            ParmStorageInfo dirStorageInfo = new ParmStorageInfo(
                    parmStorageInfo.getDataType(),
                    parmStorageInfo.getGridParmInfo(), VECTOR_DIR_DATA_OFFSET,
                    VECTOR_DIR_DATA_MULTIPLIER,
                    parmStorageInfo.getStorageType());
            this.storeConvertedFloatRecord(rawMagRecord, dataStore, sp,
                    groupName, parmStorageInfo, correlationMap, rec);
            this.storeConvertedFloatRecord(rawDirRecord, dataStore, sp,
                    groupName, dirStorageInfo, correlationMap, rec);
        }
    }

    /**
     * Store a grid slice of discrete data.
     * 
     * @param slice
     *            The discrete grid slice
     * @param dataStore
     *            The data store in which to save the slice
     * @param sp
     *            The storage properties for the slice
     * @param groupName
     *            The group name under which to save the slice
     * @param parmStorageInfo
     *            A structure which describes conversions to be done at
     *            storage/retrieval time
     * @param correlationMap
     *            Used for failed save diagnostics
     * @param rec
     *            The GFERecord associated with the slice
     * @throws StorageException
     */
    protected void storeDiscreteGridSlice(DiscreteGridSlice slice,
            IDataStore dataStore, StorageProperties sp, String groupName,
            ParmStorageInfo parmStorageInfo,
            Map<IDataRecord, GFERecord> correlationMap, GFERecord rec)
            throws StorageException {
        if (slice.getDiscreteGrid() != null) {
            byte[] rawData = slice.getDiscreteGrid().getBuffer().array();
            ByteDataRecord rawRecord = new ByteDataRecord("Data", groupName,
                    rawData, 2, new long[] {
                            slice.getGridInfo().getGridLoc().getNx(),
                            slice.getGridInfo().getGridLoc().getNy() });
            dataStore.addDataRecord(rawRecord, sp);

            StringBuffer sb = new StringBuffer();
            boolean first = true;
            for (DiscreteKey key : slice.getKeys()) {
                if (first) {
                    first = false;
                } else {
                    sb.append(GfeUtil.KEY_SEPARATOR);
                }
                sb.append(key.toString());
            }
            byte[] keyBytes = sb.toString().getBytes();
            ByteDataRecord keyRecord = new ByteDataRecord("Keys", groupName,
                    keyBytes, 1, new long[] { keyBytes.length });
            dataStore.addDataRecord(keyRecord, sp);
            correlationMap.put(rawRecord, rec);
            correlationMap.put(keyRecord, rec);
        }
    }

    /**
     * Store a grid slice of weather data.
     * 
     * @param slice
     *            The weather grid slice
     * @param dataStore
     *            The data store in which to save the slice
     * @param sp
     *            The storage properties
     * @param groupName
     *            The group name under which to save the slice
     * @param parmStorageInfo
     *            A structure which describes conversions to be done at
     *            storage/retrieval time
     * @param correlationMap
     *            Used for failed save diagnostics
     * @param rec
     *            The GFERecord associated with the slice
     * @throws StorageException
     */
    protected void storeWeatherGridSlice(WeatherGridSlice slice,
            IDataStore dataStore, StorageProperties sp, String groupName,
            ParmStorageInfo parmStorageInfo,
            Map<IDataRecord, GFERecord> correlationMap, GFERecord rec)
            throws StorageException {
        if (slice.getWeatherGrid() != null) {
            byte[] rawData = slice.getWeatherGrid().getBuffer().array();
            ByteDataRecord rawRecord = new ByteDataRecord("Data", groupName,
                    rawData, 2, new long[] {
                            slice.getGridInfo().getGridLoc().getNx(),
                            slice.getGridInfo().getGridLoc().getNy() });
            dataStore.addDataRecord(rawRecord, sp);

            StringBuffer sb = new StringBuffer();
            boolean first = true;
            for (WeatherKey key : slice.getKeys()) {
                if (first) {
                    first = false;
                } else {
                    sb.append(GfeUtil.KEY_SEPARATOR);
    }
                sb.append(key.toString());
            }
            byte[] keyBytes = sb.toString().getBytes();
            ByteDataRecord keyRecord = new ByteDataRecord("Keys", groupName,
                    keyBytes, 1, new long[] { keyBytes.length });
            dataStore.addDataRecord(keyRecord, sp);
            correlationMap.put(rawRecord, rec);
            correlationMap.put(keyRecord, rec);
        }
    }

    /**
     * Write a FloatDataRecord to the data store, converting it to another
     * format if parmStorageInfo indicates that it should be.
     * 
     * @param data
     *            the FloatDataRecord to convert
     * @param dataStore
     *            The data store in which to put the data
     * @param sp
     *            the storage properties
     * @param groupName
     *            The group name to assign to the stored record
     * @param parmStorageInfo
     *            Information on how the data should be converted
     * @param correlationMap
     *            Used to look up failed saves
     * @param rec
     *            The GFE record being stored
     * @throws StorageException
     */
    protected void storeConvertedFloatRecord(FloatDataRecord data,
            IDataStore dataStore, StorageProperties sp, String groupName,
            ParmStorageInfo parmStorageInfo,
            Map<IDataRecord, GFERecord> correlationMap, GFERecord rec)
            throws StorageException {

        float[] fdata = data.getFloatData();
        String storageType = parmStorageInfo.getStorageType();
        float offset = parmStorageInfo.getDataOffset();
        float multiplier = parmStorageInfo.getDataMultiplier();
        float fcvt;
        IDataRecord storeDataRec = null;
        if ("short".equals(storageType) && (multiplier != 0.0f)) {
            short[] converted = new short[fdata.length];
            for (int i = 0; i < fdata.length; i++) {
                fcvt = (fdata[i] - offset) * multiplier;
                converted[i] = (short) ((fcvt >= 0.0f) ? fcvt + 0.5
                        : fcvt - 0.5);
            }
            storeDataRec = new ShortDataRecord(data.getName(), data.getGroup(),
                    converted, data.getDimension(), data.getSizes().clone());
        } else if ("byte".equals(storageType) && (multiplier != 0.0f)) {
            byte[] converted = new byte[fdata.length];
            for (int i = 0; i < fdata.length; i++) {
                fcvt = (fdata[i] - offset) * multiplier;
                converted[i] = (byte) ((fcvt >= 0.0f) ? fcvt + 0.5 : fcvt - 0.5);
            }
            storeDataRec = new ByteDataRecord(data.getName(), data.getGroup(),
                    converted, data.getDimension(), data.getSizes().clone());
        } else {
            storeDataRec = data;
        }

        storeDataRec.setGroup(groupName);
        dataStore.addDataRecord(storeDataRec, sp);
        correlationMap.put(storeDataRec, rec);
    }

    private FloatDataRecord[] retrieveFromHDF5(ParmID parmId,
            List<TimeRange> times, ParmStorageInfo parmStorageInfo)
            throws GfeException {
        FloatDataRecord[] scalarData = null;
        Map<IDataStore, Pair<List<TimeRange>, String[]>> dsAndGroups = getDataStoreAndGroups(
                parmId, times);

        try {
            Map<TimeRange, FloatDataRecord> records = new HashMap<TimeRange, FloatDataRecord>(
                    (int) (1.25 * times.size()) + 1);

            // loop over the dataStores and their respective groups to pull all
            // data, stored into records to reorder requests by times
            for (Map.Entry<IDataStore, Pair<List<TimeRange>, String[]>> entry : dsAndGroups
                    .entrySet()) {
                Pair<List<TimeRange>, String[]> pair = entry.getValue();
                String[] groups = pair.getSecond();

                IDataRecord[] rawData = entry.getKey().retrieveGroups(groups,
                        Request.ALL);

                if (rawData.length != groups.length) {
                    throw new IllegalArgumentException(
                            "Invalid number of dataSets returned expected 1 per group, received: "
                                    + ((double) rawData.length / groups.length));
                }

                int count = 0;
                for (TimeRange timeRange : pair.getFirst()) {
                    IDataRecord rec = rawData[count++];

                    if (rec instanceof FloatDataRecord) {
                        records.put(timeRange, (FloatDataRecord) rec);
                    } else if (gridDbConfig == null) {
                        throw new IllegalArgumentException("Data array for "
                                + parmId.getParmName() + " "
                                + parmId.getParmLevel()
                                + " is not a float array, but database "
                                + toString()
                                + " does not contain a grid configuration.");
                    } else {
                        // Convert to a FloatDataRecord for internal use
                        records.put(timeRange,
                                storageToFloat(rec, parmStorageInfo));
                    }
                }
            }

            scalarData = new FloatDataRecord[times.size()];
            int count = 0;
            for (TimeRange timeRange : times) {
                scalarData[count++] = records.get(timeRange);
            }
        } catch (Exception e) {
            throw new GfeException("Unable to get data from HDF5 for ParmID: "
                    + parmId + " TimeRange: " + times, e);
        }

        return scalarData;
    }

    private FloatDataRecord[][] retrieveVectorFromHDF5(ParmID parmId,
            List<TimeRange> times, ParmStorageInfo magStorageInfo)
            throws GfeException {
        FloatDataRecord[][] vectorData = null;
        Map<IDataStore, Pair<List<TimeRange>, String[]>> dsAndGroups = getDataStoreAndGroups(
                parmId, times);

        try {
            Map<TimeRange, FloatDataRecord[]> records = new HashMap<TimeRange, FloatDataRecord[]>(
                    (int) (1.25 * times.size()) + 1);

            // loop over the dataStores and their respective groups to pull all
            // data, stored into records to reorder requests by times
            for (Map.Entry<IDataStore, Pair<List<TimeRange>, String[]>> entry : dsAndGroups
                    .entrySet()) {
                Pair<List<TimeRange>, String[]> pair = entry.getValue();
                String[] groups = pair.getSecond();

                IDataRecord[] rawData = entry.getKey().retrieveGroups(groups,
                        Request.ALL);

                if (rawData.length != (groups.length * 2)) {
                    throw new IllegalArgumentException(
                            "Invalid number of dataSets returned expected  per group, received: "
                                    + ((double) rawData.length / groups.length));
                }

                // iterate over the data from this dataStore adding it records
                int count = 0;
                for (TimeRange timeRange : pair.getFirst()) {
                    IDataRecord magRec = null;
                    IDataRecord dirRec = null;

                    // Should be vector data and each group should have had a
                    // Dir and Mag dataset
                    for (int i = 0; i < 2; i++) {
                        IDataRecord rec = rawData[(count * 2) + i];
                        if ("Mag".equals(rec.getName())) {
                            magRec = rec;
                        } else if ("Dir".equals(rec.getName())) {
                            dirRec = rec;
                        } else {
                            throw new IllegalArgumentException(
                                    "Unknown dataset retrieved for vector data.  Valid values: Mag, Dir  Received: "
                                            + rec.getName());
                        }
                    }

                    FloatDataRecord[] recs = new FloatDataRecord[2];

                    if (magRec.getClass() == dirRec.getClass()) {
                        if (magRec instanceof FloatDataRecord) {
                            recs[0] = (FloatDataRecord) magRec;
                            recs[1] = (FloatDataRecord) dirRec;
                        } else if (gridDbConfig == null) {
                            throw new IllegalArgumentException(
                                    "Data array for "
                                            + parmId.getParmName()
                                            + " "
                                            + parmId.getParmLevel()
                                            + " is not a float array, but database "
                                            + toString()
                                            + " does not contain a grid configuration.");
                        } else {
                            ParmStorageInfo dirStorageInfo = new ParmStorageInfo(
                                    magStorageInfo.getDataType(),
                                    magStorageInfo.getGridParmInfo(),
                                    VECTOR_DIR_DATA_OFFSET,
                                    VECTOR_DIR_DATA_MULTIPLIER,
                                    magStorageInfo.getStorageType());
                            recs[0] = storageToFloat(magRec, magStorageInfo);
                            recs[1] = storageToFloat(dirRec, dirStorageInfo);
                        }

                        records.put(timeRange, recs);
                        count++;
                    } else {
                        throw new IllegalArgumentException(
                                "Magnitude and direction grids are not of the same type.");
                    }
                }
            }

            vectorData = new FloatDataRecord[times.size()][2];
            int count = 0;
            for (TimeRange timeRange : times) {
                vectorData[count++] = records.get(timeRange);
            }
        } catch (Exception e) {
            throw new GfeException("Unable to get data from HDF5 for ParmID: "
                    + parmId + " TimeRange: " + times, e);
        }

        return vectorData;
    }

    /**
     * Convert a data record to an equivalent FloatDataRecord. Assumes that
     * rawData and parmStorageInfo are non-null, rawData is not already a
     * FloatDataRecord, and parmStorageInfo.dataMultiplier() is nonzero.
     * 
     * @param rawData
     *            The data as stored in the HDF5 file.
     * @param parmStorageInfo
     *            Describes how data is converted in storage.
     * @return rawData, converted to a FloatDataRecord according to the
     *         conversion in parmStorageInfo.
     */
    protected FloatDataRecord storageToFloat(IDataRecord rawData,
            ParmStorageInfo parmStorageInfo) {
        FloatDataRecord data;
        String storageType = parmStorageInfo.getStorageType();
        float multiplier = parmStorageInfo.getDataMultiplier();
        float offset = parmStorageInfo.getDataOffset();
        float[] floats = null;
        if ("byte".equals(storageType)) {
            byte[] rawBytes = ((ByteDataRecord) rawData).getByteData();
            floats = new float[rawBytes.length];
            for (int idx = 0; idx < rawBytes.length; idx++) {
                // hex mask to treat bytes as unsigned
                floats[idx] = ((rawBytes[idx] & 0xff) / multiplier) + offset;
            }
        } else if ("short".equals(storageType)) {
            short[] rawShorts = ((ShortDataRecord) rawData).getShortData();
            floats = new float[rawShorts.length];
            for (int idx = 0; idx < rawShorts.length; idx++) {
                // shorts are stored as signed, no masking!
                floats[idx] = (rawShorts[idx] / multiplier) + offset;
            }
        } else if ("float".equals(storageType)) {
            throw new IllegalArgumentException(
                    "Expected FloatDataRecord, but got "
                            + rawData.getClass().getName());
        } else {
            throw new IllegalArgumentException("Unknown data type '"
                    + storageType + "'.");
        }
        data = new FloatDataRecord(rawData.getName(), rawData.getGroup(),
                floats, rawData.getDimension(), rawData.getSizes());
        return data;
    }

    /**
     * Retrieve the ParmStorageInfo for a parm
     * 
     * @param parmID
     *            the desired parmID
     * @return the ParmStorageInfo
     */
    protected ParmStorageInfo findStorageInfo(ParmID parmID) {
        ParmStorageInfo parmStorageInfo = this.parmStorageInfo.get(parmID
                .getCompositeName());

        if (parmStorageInfo == null) {
            // Default to a no-conversion float storage
            GridParmInfo gpi = getGridParmInfo(parmID).getPayload();
            parmStorageInfo = new ParmStorageInfo("float", gpi, 0.0f, 1.0f,
                    "float");
        }
        return parmStorageInfo;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.GridDatabase#deleteDb()
     */
    @Override
    public void deleteDb() {
        DatabaseID id = getDbId();
        deleteDatabase(id);
    }

    /**
     * Delete a database
     * 
     * @param id
     *            the DatabaseID of the datbase to be deleted
     */
    public static void deleteDatabase(DatabaseID id) {
        try {
            GFEDao gfeDao = new GFEDao();
            gfeDao.purgeGFEGrids(id);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to delete model database: " + id, e);
        }

        deleteModelHDF5(id);
    }

    /**
     * Removes a record from the HDF5 repository. If the record does not exist
     * in the HDF5, the operation is ignored
     * 
     * @param record
     *            The record to remove
     */
    private void removeFromHDF5(GFERecord record) {
        File hdf5File = GfeUtil.getHdf5File(gfeBaseDataDir, record.getParmId(),
                record.getDataTime().getValidPeriod());

        /*
         * Remove the grid from HDF5
         */
        String groupName = GfeUtil.getHDF5Group(record.getParmId(),
                record.getTimeRange());

        IDataStore dataStore = DataStoreFactory.getDataStore(hdf5File);

        try {
            dataStore.deleteGroups(groupName);
            statusHandler.handle(Priority.DEBUG, "Deleted: " + groupName
                    + " from " + hdf5File.getName());

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting hdf5 record " + record.toString(), e);
        }
    }

    /**
     * Removes records from the HDF5 repository. If the records do not exist in
     * the HDF5, the operation is ignored
     * 
     * @param parmId
     *            the parmID of the records to be removed
     * @param times
     *            the timeRanges of the records to be removed
     */
    private void removeFromHDF5(ParmID parmId, List<TimeRange> times) {
        Map<File, Pair<List<TimeRange>, String[]>> fileMap = GfeUtil
                .getHdf5FilesAndGroups(GridDatabase.gfeBaseDataDir, parmId,
                        times);
        for (Map.Entry<File, Pair<List<TimeRange>, String[]>> entry : fileMap
                .entrySet()) {
            File hdf5File = entry.getKey();
            IDataStore dataStore = DataStoreFactory.getDataStore(hdf5File);
            String[] groupsToDelete = entry.getValue().getSecond();

            try {
                dataStore.deleteGroups(groupsToDelete);

                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.handle(Priority.DEBUG,
                            "Deleted: " + Arrays.toString(groupsToDelete)
                                    + " from " + hdf5File.getName());
                }
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.WARN,
                        "Error deleting hdf5 record(s) from file: "
                                + hdf5File.getPath(), e);
            }
        }
    }

    private static void deleteModelHDF5(DatabaseID dbId) {
        File hdf5File = GfeUtil.getHdf5Dir(GridDatabase.gfeBaseDataDir, dbId);
        IDataStore ds = DataStoreFactory.getDataStore(hdf5File);
        try {
            ds.deleteFiles(null);
        } catch (Exception e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error deleting GFE model data from hdf5 for "
                            + dbId.toString(), e);
        }
    }

    /**
     * Updates the history times for the associated history objects. History
     * objects must have already been retrieved from database.
     */
    @Override
    public ServerResponse<?> updatePublishTime(List<GridDataHistory> history,
            Date publishTime) {
        ServerResponse<?> sr = new ServerResponse<String>();

        try {
            dao.updatePublishTime(history, publishTime);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to update grid history!", e);
            sr.addMessage("Error updating history");
        }

        return sr;
    }

    /**
     * Retrieve the cached ParmID database object
     * 
     * @param parmNameAndLevel
     * @return the cached ParmID
     * @throws UnknownParmIdException
     */
    public ParmID getCachedParmID(String parmNameAndLevel)
            throws UnknownParmIdException {
        // ParmID rval = parmIdMap.get(parmNameAndLevel);
        ParmID rval = this.parmStorageInfo.get(parmNameAndLevel).getParmID();

        if (rval == null) {
            throw new UnknownParmIdException("ParmId: " + parmNameAndLevel
                    + ":" + dbId.getModelId() + " doesn't exist");
        }

        return rval;
    }

    @Override
    public ParmID getCachedParmID(ParmID parmId) throws UnknownParmIdException {
        // ParmID rval = parmIdMap.get(parmId.getCompositeName());
        ParmID rval = this.parmStorageInfo.get(parmId.getCompositeName())
                .getParmID();

        if (rval == null) {
            throw new UnknownParmIdException("ParmId: " + parmId.toString()
                    + " doesn't exist");
        }

        return rval;
    }

    /**
     * Updates the sent time for all histories of passed parmId during the
     * timeRange. The histories are then returned in a map by timeRange.
     * 
     * @param parmId
     *            the parmId to use
     * @param tr
     *            the time range to update sent time for
     * @param sentTime
     *            the sent time to update to
     * @return map containing updated grid histories
     */
    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> updateSentTime(
            final ParmID parmId, TimeRange tr, Date sentTime) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();
        try {
            ParmID dbParmId = getCachedParmID(parmId);
            sr.setPayload(dao.updateSentTime(dbParmId, tr, sentTime));
        } catch (UnknownParmIdException e) {
            sr.addMessage(e.getLocalizedMessage());
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to update grid history last sent time", e);
            sr.addMessage("Unable to update grid history last sent time");
        }

        return sr;
    }

    /**
     * Retrieves the ByteDataRecord for a Discrete grid for multiple time ranges
     * from HDF5
     * 
     * @param parmId
     * @param times
     * @return array containing the data and keys for the specified times
     * @throws GfeException
     */
    public ByteDataRecord[][] retrieveDiscreteFromHDF5(ParmID parmId,
            List<TimeRange> times) throws GfeException {
        ByteDataRecord[][] byteRecords = null;
        Map<IDataStore, Pair<List<TimeRange>, String[]>> dsAndGroups = getDataStoreAndGroups(
                parmId, times);

        try {
            // loop over the dataStores and their respective groups to pull all
            // data
            Map<TimeRange, ByteDataRecord[]> records = new HashMap<TimeRange, ByteDataRecord[]>(
                    (int) (1.25 * times.size()) + 1);

            for (Map.Entry<IDataStore, Pair<List<TimeRange>, String[]>> entry : dsAndGroups
                    .entrySet()) {
                Pair<List<TimeRange>, String[]> pair = entry.getValue();
                String[] groups = pair.getSecond();

                IDataRecord[] rawData = entry.getKey().retrieveGroups(groups,
                        Request.ALL);

                if (rawData.length != (groups.length * 2)) {
                    throw new IllegalArgumentException(
                            "Invalid number of dataSets returned expected 2 per group, received: "
                                    + ((double) rawData.length / groups.length));
                }

                // iterate over the data from this dataStore adding it records
                int count = 0;
                for (TimeRange timeRange : pair.getFirst()) {
                    ByteDataRecord[] recs = new ByteDataRecord[2];
                    for (int i = 0; i < 2; i++) {
                        IDataRecord rec = rawData[(count * 2) + i];

                        if ("Data".equals(rec.getName())) {
                            recs[0] = (ByteDataRecord) rec;
                        } else if ("Keys".equals(rec.getName())) {
                            recs[1] = (ByteDataRecord) rec;
                        } else {
                            throw new IllegalArgumentException(
                                    "Unknown dataset retrieved for vector data.  Valid values: Data, Keys  Received: "
                                            + rec.getName());
                        }
                    }
                    records.put(timeRange, recs);
                    count++;
                }
            }

            byteRecords = new ByteDataRecord[times.size()][2];
            int count = 0;
            for (TimeRange timeRange : times) {
                byteRecords[count++] = records.get(timeRange);
            }
        } catch (Exception e) {
            throw new GfeException("Unable to get data from HDF5 for ParmID: "
                    + parmId + " TimeRange: " + times, e);
        }
        return byteRecords;
    }

    /**
     * Retrieve the FloatDataRecord for a grid from HDF5
     * 
     * @param parmId
     * @param time
     * @param parmStorageInfo
     * @return the FloatDataRecord
     * @throws GfeException
     */
    private FloatDataRecord retrieveFromHDF5(ParmID parmId, TimeRange time,
            ParmStorageInfo parmStorageInfo) throws GfeException {
        return retrieveFromHDF5(parmId,
                Arrays.asList(new TimeRange[] { time }), parmStorageInfo)[0];
    }

    /**
     * Retrieve the magnitude and direction grids for a vector parm from HDF5
     * 
     * @param parmId
     * @param time
     * @param magStorageInfo
     * @return the the magnitude and direction grids
     * @throws GfeException
     */
    private FloatDataRecord[] retrieveVectorFromHDF5(ParmID parmId,
            TimeRange time, ParmStorageInfo magStorageInfo) throws GfeException {
        return retrieveVectorFromHDF5(parmId,
                Arrays.asList(new TimeRange[] { time }), magStorageInfo)[0];
    }
}
