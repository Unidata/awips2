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

import java.lang.ref.SoftReference;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
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

    private static GridInfoCache instance = new GridInfoCache();

    public static GridInfoCache getInstance() {
        return instance;
    }

    private final CoreDao dao;

    // A weak hashmap of soft references is used as a SoftSet.
    private Map<GridInfoRecord, SoftReference<GridInfoRecord>> cache = null;

    private GridInfoCache() {
        cache = Collections
                .synchronizedMap(new WeakHashMap<GridInfoRecord, SoftReference<GridInfoRecord>>());
        dao = new CoreDao(DaoConfig.forClass(GridInfoRecord.class));
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

    private GridInfoRecord checkLocalCache(GridInfoRecord record) {
        GridInfoRecord result = null;
        SoftReference<GridInfoRecord> ref = cache.get(record);
        if (ref != null) {
            result = ref.get();
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
        // It is possible that this query will return multiple
        // results, for example in the case of models with secondary ids. In
        // general these cases should be rare and small. So we handle it by
        // caching everything returned and then double checking the cache.
        DatabaseQuery query = new DatabaseQuery(GridInfoRecord.class);
        query.addQueryParam(GridInfoConstants.DATASET_ID, record.getDatasetId());
        query.addQueryParam(GridInfoConstants.PARAMETER_ABBREVIATION, record
                .getParameter().getAbbreviation());
        query.addQueryParam(GridInfoConstants.LEVEL_ID, record.getLevel()
                .getId());
        query.addQueryParam(GridInfoConstants.LOCATION_ID, record.getLocation()
                .getId());
        List<?> dbList = dao.queryByCriteria(query);

        if (dbList != null && !dbList.isEmpty()) {
            for (Object pdo : dbList) {
                GridInfoRecord gir = (GridInfoRecord) pdo;
                // if we don't remove then when an entry exists already the key
                // and value become references to different objects which is not
                // what we want.
                cache.remove(gir);
                cache.put(gir, new SoftReference<GridInfoRecord>(gir));
            }
        }
        return checkLocalCache(record);
    }

    /**
     * Insert the record into the database if there is no current record that
     * equals this one. This method uses a fairly broad cluster lock so only one
     * thread at a time across all clustered edices can insert at a time. This
     * method should not be used much on running systems since gridded models
     * maintain fairly consistent info records over time.
     * 
     * @param record
     * @return
     */
    private GridInfoRecord insert(GridInfoRecord record)
            throws DataAccessLayerException {
        ClusterTask ct = null;
        do {
            ct = ClusterLockUtils.lock("grid_info", "newEntry", 30000, true);
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
        cache.put(record, new SoftReference<GridInfoRecord>(record));
        return record;
    }

    /**
     * Remove the info records with the specified ids from the cache.
     * 
     * @param infoKeys
     */
    public void purgeCache(Collection<Integer> infoKeys) {
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
