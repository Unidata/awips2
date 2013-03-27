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
package com.raytheon.uf.edex.plugin.grid.dao;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Cache the gridInfo objects from the database to avoid repeated lookups.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2012            bsteffen     Initial creation
 * Mar 27, 2013 1821       bsteffen    Speed up GridInfoCache.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridInfoCache {

    // 6 hours
    private static final int ROTATION_INTERVAL = 1 * 1 * 60 * 1000;

    private static GridInfoCache instance = new GridInfoCache();

    public static GridInfoCache getInstance() {
        return instance;
    }

    private static final CoreDao dao = new CoreDao(
            DaoConfig.forClass(GridInfoRecord.class));

    private Map<String, DatasetCache> cache = null;

    private long lastRotationTime;

    private GridInfoCache() {
        cache = Collections
                .synchronizedMap(new HashMap<String, DatasetCache>());
        lastRotationTime = System.currentTimeMillis();
    }

    public GridInfoRecord getGridInfo(GridInfoRecord record)
            throws DataAccessLayerException {
        DatasetCache dCache = cache.get(record.getDatasetId());
        if (dCache == null) {
            dCache = createDatasetCache(record.getDatasetId());
        }
        GridInfoRecord result = dCache.getGridInfo(record);
        if (System.currentTimeMillis() > lastRotationTime + ROTATION_INTERVAL) {
            rotateCache();
        }
        return result;
    }

    /**
     * Remove the info records with the specified ids from the cache.
     * 
     * @param infoKeys
     */
    public void purgeCache(Collection<Integer> infoKeys) {
        synchronized (cache) {
            Iterator<DatasetCache> it = cache.values().iterator();
            while (it.hasNext()) {
                DatasetCache next = it.next();
                next.purgeCache(infoKeys);
                if (next.isEmpty()) {
                    it.remove();
                }
            }
        }
    }

    private DatasetCache createDatasetCache(String datasetId)
            throws DataAccessLayerException {
        synchronized (cache) {
            DatasetCache dCache = cache.get(datasetId);
            if (dCache == null) {
                dCache = new DatasetCache(datasetId);
                cache.put(datasetId, dCache);
            }
            return dCache;
        }
    }

    private void rotateCache() {
        synchronized (cache) {
            if (System.currentTimeMillis() > lastRotationTime
                    + ROTATION_INTERVAL) {
                Iterator<DatasetCache> it = cache.values().iterator();
                while (it.hasNext()) {
                    DatasetCache next = it.next();
                    next.rotateCache();
                    if (next.isEmpty()) {
                        it.remove();
                    }
                }
            }
            lastRotationTime = System.currentTimeMillis();
        }
    }

    /**
     * 
     * A second chance cache for all GridInfoRecords for a single datasetid.
     * 
     */
    private static class DatasetCache {

        private Map<GridInfoRecord, GridInfoRecord> primaryCache;

        private Map<GridInfoRecord, GridInfoRecord> secondChanceCache;

        public DatasetCache(String datasetid) throws DataAccessLayerException {
            primaryCache = Collections
                    .synchronizedMap(new HashMap<GridInfoRecord, GridInfoRecord>());
            secondChanceCache = Collections
                    .synchronizedMap(new HashMap<GridInfoRecord, GridInfoRecord>());
            DatabaseQuery query = new DatabaseQuery(GridInfoRecord.class);
            query.addQueryParam(GridInfoConstants.DATASET_ID, datasetid);
            queryAndAdd(query);
        }

        public GridInfoRecord getGridInfo(GridInfoRecord record)
                throws DataAccessLayerException {
            GridInfoRecord result = checkLocalCache(record);
            if (result == null) {
                result = query(record);
                if (result == null) {
                    result = insert(record);
                }
            }
            return result;
        }

        public void purgeCache(Collection<Integer> infoKeys) {
            purgeCache(infoKeys, primaryCache);
            purgeCache(infoKeys, secondChanceCache);
        }

        public void rotateCache() {
            secondChanceCache = primaryCache;
            primaryCache = Collections
                    .synchronizedMap(new HashMap<GridInfoRecord, GridInfoRecord>());
        }

        public boolean isEmpty() {
            return primaryCache.isEmpty() && secondChanceCache.isEmpty();
        }

        private GridInfoRecord checkLocalCache(GridInfoRecord record) {
            GridInfoRecord result = primaryCache.get(record);
            if (result == null) {
                result = secondChanceCache.get(record);
                if (result != null) {
                    addToCache(result);
                }
            }
            return result;
        }

        /**
         * Query the database for a record, if a record is found then it will be
         * added to the cache and returned.
         * 
         * @param record
         * @return
         * @throws DataAccessLayerException
         */
        private GridInfoRecord query(GridInfoRecord record)
                throws DataAccessLayerException {
            DatabaseQuery query = new DatabaseQuery(GridInfoRecord.class);
            query.addQueryParam(GridInfoConstants.DATASET_ID,
                    record.getDatasetId());
            query.addQueryParam(GridInfoConstants.PARAMETER_ABBREVIATION,
                    record.getParameter().getAbbreviation());
            query.addQueryParam(GridInfoConstants.LEVEL_ID, record.getLevel()
                    .getId());
            query.addQueryParam(GridInfoConstants.LOCATION_ID, record
                    .getLocation().getId());
            queryAndAdd(query);
            return checkLocalCache(record);
        }

        private void queryAndAdd(DatabaseQuery query)
                throws DataAccessLayerException {
            List<?> dbList = dao.queryByCriteria(query);
            if (dbList != null && !dbList.isEmpty()) {
                for (Object pdo : dbList) {
                    addToCache((GridInfoRecord) pdo);
                }
            }
        }

        /**
         * Replace several fields with cached versions to save memory and then
         * add to the primaryCache.
         * 
         * @param record
         */
        private void addToCache(GridInfoRecord record) {
            record.setLocation(GridCoverageLookup.getInstance().getCoverage(
                    record.getLocation().getId()));
            record.setParameter(ParameterLookup.getInstance().getParameter(
                    record.getParameter().getAbbreviation()));
            try {
                record.setLevel(LevelFactory.getInstance().getLevel(
                        record.getLevel().getId()));
            } catch (CommunicationException e) {
                // This should never hit and if it does ignore it, the only side
                // affect is thatthe level in the record will not be the same as
                // the other records on the same level.
            }
            primaryCache.put(record, record);
        }

        /**
         * Insert the record into the database if there is no current record
         * that equals this one. This method uses a fairly broad cluster lock so
         * only one thread at a time across all clustered edices can insert at a
         * time. This method should not be used much on running systems since
         * gridded models maintain fairly consistent info records over time.
         * 
         * @param record
         * @return
         */
        private GridInfoRecord insert(GridInfoRecord record)
                throws DataAccessLayerException {
            ClusterTask ct = null;
            do {
                ct = ClusterLockUtils.lock("grid_info_create",
                        record.getDatasetId(), 30000, true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));
            try {
                GridInfoRecord existing = query(record);
                if (existing != null) {
                    return existing;
                }
                dao.saveOrUpdate(record);
            } finally {
                ClusterLockUtils.unlock(ct, false);
            }
            addToCache(record);
            return record;
        }

        private void purgeCache(Collection<Integer> infoKeys,
                Map<GridInfoRecord, GridInfoRecord> cache) {
            synchronized (cache) {
                Iterator<GridInfoRecord> it = cache.keySet().iterator();
                while (it.hasNext()) {
                    GridInfoRecord next = it.next();
                    if (infoKeys.contains(next.getId())) {
                        it.remove();
                    }
                }
            }
        }

    }

}
