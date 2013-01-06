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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

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

    public GridInfoRecord getGridInfo(GridInfoRecord record) {
        GridInfoRecord result = checkLocalCache(record);
        if (result == null) {
            synchronized (this) {
                // It is possible that this query will return multiple results,
                // for example if the record we are looking for has a null
                // secondaryId but some db entries have a secondaryId set then
                // this query will return all matching models ignoring
                // secondaryId. In general these cases should be rare and small.
                // So we handle it by caching everything returned and then
                // double checking the cache.
                List<PersistableDataObject<Integer>> dbList = dao
                        .queryByExample(record);
                if (dbList != null && !dbList.isEmpty()) {
                    for (PersistableDataObject<Integer> pdo : dbList) {
                        GridInfoRecord gir = (GridInfoRecord) pdo;
                        cache.put(gir, new SoftReference<GridInfoRecord>(gir));
                    }
                }
                result = checkLocalCache(record);
                if (result == null) {
                    dao.saveOrUpdate(record);
                    result = record;
                }
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

    public void purgeCache(List<Integer> modelKeys) {
        for (Integer key : modelKeys) {
            cache.remove(key);
        }
    }

}
