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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.Pair;

/**
 * Base class for GFE grid databases. This class maintains the location of the
 * HDF5 files and methods for manipulation of the data contained therein.<br>
 * <p>
 * This class is responsible for creating the HDF5 repositories for the raw
 * data. For singleton databases, data is stored according to the end time of
 * the grid. For model databases, data is stored according to the model time.
 * <p>
 * This class was partially ported from the original GFE source but contains
 * major differences due architecture differences
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 06/19/08                njensen     Added retrieval of discrete
 * 05/04/12     #574       dgilling    Update class to better match AWIPS1.
 * 01/14/13     #1469      bkowal      The hdf5 data directory is no longer included
 *                                     in the gfeBaseDataDir.
 * 02/10/13     #1603      randerso    Moved removeFromDb, removeFromHDF5 and deleteModelHDF5
 *                                     methods down to IFPGridDatabase
 * 03/15/13     #1795      njensen     Added updatePublishTime()
 * 04/23/13     #1949      rjpeter     Added default implementations of history by time range
 *                                     and cachedParmId
 * 05/02/13     #1969      randerso    Removed unnecessary updateDbs method
 * 06/13/13     #2044      randerso    Code cleanup
 * 08/13/13     #1571      randerso    Moved retrieveFromHDF5 methods into IFPGridDatabase
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public abstract class GridDatabase {

    /**
     * The base directory where the GFE HDF5 data is stored
     */
    public static final String gfeBaseDataDir;

    /** The database id for this grid database */
    protected DatabaseID dbId;

    /** Flag denoting whether this database is valid */
    protected boolean valid;

    static {
        gfeBaseDataDir = "gfe" + File.separator;
    }

    /**
     * Creates a new GridDatabase
     */
    protected GridDatabase() {

    }

    /**
     * Creates anew GridDatabase
     * 
     * @param dbId
     *            The database ID for this database
     */
    protected GridDatabase(DatabaseID dbId) {
        this.dbId = dbId;
    }

    /**
     * Returns the mapping of ParmIds and TimeRanges to DataStores and groups
     * where the data should be stored.
     * 
     * @param parmId
     * @param times
     * @return mapping
     */
    protected Map<IDataStore, Pair<List<TimeRange>, String[]>> getDataStoreAndGroups(
            ParmID parmId, List<TimeRange> times) {
        Map<File, Pair<List<TimeRange>, String[]>> fileMap = GfeUtil
                .getHdf5FilesAndGroups(GridDatabase.gfeBaseDataDir, parmId,
                        times);
        // size hashMap accounting for load factor
        Map<IDataStore, Pair<List<TimeRange>, String[]>> rval = new HashMap<IDataStore, Pair<List<TimeRange>, String[]>>(
                (int) (fileMap.size() * 1.25) + 1);
        for (Map.Entry<File, Pair<List<TimeRange>, String[]>> entry : fileMap
                .entrySet()) {
            rval.put(DataStoreFactory.getDataStore(entry.getKey()),
                    entry.getValue());
        }
        return rval;
    }

    /**
     * Denotes if this database is valid and is able to be written to
     * 
     * @return True if valid, else false
     */
    public boolean databaseIsValid() {
        return valid;
    }

    /**
     * Delete the database and HDF5 records for this database
     */
    public abstract void deleteDb();

    /**
     * Returns the list of ParmIDs that are contained in this database.
     * 
     * Make sure the database is valid and check the size of the parmInfo
     * dictionary. If it's zero, return a bad ServerResponse. Loop through each
     * entry in the parmInfo dictionary and extract the compositeName
     * parmName_level which is the dictionary key. Then make a new ParmID and
     * append it to parmIDs.
     * 
     * @return The server response
     */
    public abstract ServerResponse<List<ParmID>> getParmList();

    /**
     * Gets the inventory of time ranges currently for the specified ParmID
     * 
     * @param id
     *            The parmID to get the inventory for
     * @return The server response
     */
    public abstract ServerResponse<List<TimeRange>> getGridInventory(ParmID id);

    /**
     * Gets the inventory of time ranges currently for the specified ParmID that
     * overlap the given time range.
     * 
     * @param id
     *            The parmID to get the inventory for
     * @param tr
     *            the time range
     * @return The server response
     */
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id,
            TimeRange tr) {
        // default to prior behavior with removing the extra inventories
        ServerResponse<List<TimeRange>> sr = getGridInventory(id);
        List<TimeRange> trs = sr.getPayload();
        ListIterator<TimeRange> iter = trs.listIterator(trs.size());
        while (iter.hasPrevious()) {
            TimeRange curTr = iter.previous();
            if (!curTr.overlaps(tr)) {
                iter.remove();
            }
        }
        return sr;
    }

    /**
     * Retrieves a sequence gridSlices from the database based on the specified
     * parameters and stores them in the data parameter. TimeRanges of the grids
     * stored in the database must exactly match those in timeRanges or no
     * GridSlice will be returned. If the ServerResponse is not good, it should
     * be assumed that all of the GridSlices are invalid even though this may
     * not always be the case. Any error returned in the ServerResponse should
     * be considered a bug.
     * 
     * Make sure that the database is valid and get the timeInfo. If the
     * timeInfo was not found return an error Server Response. Then get the
     * GridParmInfo. Set the length of the output array to correct size. Loop
     * through each TimeRange and get one GridSlice at a time. If any errors
     * occurs while getting GridSlices, return an error ServerResponse.
     * 
     * @param id
     *            The parmID of the data to retrieve
     * @param timeRanges
     *            The times to retrieve
     * @return The server response
     */
    public abstract ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges);

    /**
     * Gets the grid parameter information for this parameter through the
     * calling argument
     * 
     * @param id
     *            The grid parm info
     * @return The server status
     */
    public abstract ServerResponse<GridParmInfo> getGridParmInfo(ParmID id);

    /**
     * Gets the grid history for a list of times for a given ParmID
     * 
     * @param id
     *            The parmID to get the history for
     * @param trs
     *            The time ranges to get the history for
     * @return The server status
     */
    public abstract ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> trs);

    /**
     * get the projection ID for this database
     * 
     * @return the projection ID
     */
    public abstract String getProjectionId();

    /**
     * Retrieve the DatabaseID for this database
     * 
     * @return the DatabaseID
     */
    public DatabaseID getDbId() {
        return dbId;
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
    public ServerResponse<?> saveGridData(ParmID id,
            TimeRange originalTimeRange, List<GFERecord> records,
            WsId requesterId) {
        throw new UnsupportedOperationException("Not implemented for class "
                + this.getClass().getName());
    }

    /**
     * Updates the publish times in the database of all provided
     * GridDataHistories. Does not alter the publish times in memory.
     * 
     * @param history
     *            the histories to alter in the database
     * @param publishTime
     *            the publish time to update to
     * @return ServerResponse containing status only
     */
    public ServerResponse<?> updatePublishTime(List<GridDataHistory> history,
            Date publishTime) {
        throw new UnsupportedOperationException("Not implemented for class "
                + this.getClass().getName());
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
     * @return ServerResponse containing updated histories
     */
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> updateSentTime(
            final ParmID parmId, TimeRange tr, Date sentTime) {
        throw new UnsupportedOperationException("Not implemented for class "
                + this.getClass().getName());
    }

    /**
     * Save grid slices
     * 
     * @param parmId
     * @param tr
     * @param sliceData
     * @param requestor
     * @param skipDelete
     * @return ServerResponse containing status only
     */
    public ServerResponse<?> saveGridSlices(ParmID parmId, TimeRange tr,
            List<IGridSlice> sliceData, WsId requestor,
            List<TimeRange> skipDelete) {
        throw new UnsupportedOperationException("Not implemented for class "
                + this.getClass().getName());

    }

    /**
     * Return the internally cached parmID for this database implementation.
     * 
     * @param parmID
     * @return cached ParmID
     * @throws GfeException
     *             If the parm does not exist for this database.
     */
    public ParmID getCachedParmID(ParmID parmID) throws GfeException {
        // base implementation, must be overridden by Databases that store
        // ParmID objects
        return parmID;
    }
}
