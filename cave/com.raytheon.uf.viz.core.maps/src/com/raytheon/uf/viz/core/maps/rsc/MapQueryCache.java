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
package com.raytheon.uf.viz.core.maps.rsc;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.localization.FileLocker;
import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.cache.ICacheObject;
import com.raytheon.uf.common.util.cache.LRUCache;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.MapQueryCache.QueryCacheable;

/**
 * Map query caching derived from Erik Magnuson's code changes to DirectDbQuery
 * to keep map results around on the client and not waste time/bandwidth
 * querying.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MapQueryCache extends LRUCache<String, QueryCacheable> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapQueryCache.class);

    private static MapQueryCache mapQueryCache = new MapQueryCache(
            64 * 1024 * 1024);

    private static boolean cache = false;

    @DynamicSerialize
    protected static class QueryCacheable implements ICacheObject {

        @DynamicSerializeElement
        QueryResult queryResult;

        public QueryCacheable() {

        }

        protected QueryCacheable(QueryResult result) {
            queryResult = result;
        }

        @Override
        public int getSize() {
            // TODO get better number or move this off LRU
            return queryResult.getColumnCount() * queryResult.getRows().length
                    * 4;
        }

        public QueryResult getQueryResult() {
            return queryResult;
        }

        public void setQueryResult(QueryResult queryResult) {
            this.queryResult = queryResult;
        }

    }

    @DynamicSerialize
    protected static class CachePair {

        @DynamicSerializeElement
        String queryKey;

        @DynamicSerializeElement
        QueryCacheable cacheable;

        public CachePair() {

        }

        protected CachePair(String key, QueryCacheable obj) {
            this.queryKey = key;
            this.cacheable = obj;
        }

        public String getQueryKey() {
            return queryKey;
        }

        public void setQueryKey(String queryKey) {
            this.queryKey = queryKey;
        }

        public QueryCacheable getCacheable() {
            return cacheable;
        }

        public void setCacheable(QueryCacheable cacheable) {
            this.cacheable = cacheable;
        }

    }

    /**
     * Private constructor
     * 
     * @param maxSize
     */
    private MapQueryCache(long maxSize) {
        super(maxSize);
    }

    /**
     * Executes a mapped query against DirectDbQuery API. If cache is enabled
     * will check the cache before sending a query and if sending a query, will
     * store the result in the cache.
     * 
     * @param query
     * @param database
     * @param language
     * @return
     * @throws VizException
     */
    public static QueryResult executeMappedQuery(String query, String database,
            QueryLanguage language) throws VizException {
        QueryResult queryResult = null;
        String key = query + database + language;

        if (cache) {
            QueryCacheable cacheResult = mapQueryCache.get(key);
            if (cacheResult != null) {
                queryResult = cacheResult.queryResult;
            }
        }

        if (queryResult == null) {
            // either cache was disabled, it wasn't in cache, or reading cache
            // failed, so send the query
            queryResult = DirectDbQuery.executeMappedQuery(query, database,
                    language);
            if (cache) {
                mapQueryCache.put(key, new QueryCacheable(queryResult));
            }
        }

        return queryResult;
    }

    /**
     * Executes a query against DirectDbQuery API. If cache is enabled will
     * check the cache before sending a query and if sending a query, will store
     * the result in the cache.
     * 
     * @param query
     * @param database
     * @param language
     * @return
     * @throws VizException
     */
    public static List<Object[]> executeQuery(String query, String database,
            QueryLanguage language) throws VizException {
        QueryResult result = executeMappedQuery(query, database, language);
        List<Object[]> unmappedResults = new ArrayList<Object[]>();

        for (QueryResultRow row : result.getRows()) {
            unmappedResults.add(row.getColumnValues());
        }
        return unmappedResults;
    }

    public static void setCaching(boolean caching) {
        cache = caching;
    }

    private Collection<CachePair> getCacheables() {
        List<CachePair> pairs = new ArrayList<CachePair>(lruMap.size());
        synchronized (this) {
            for (Entry<String, Item> entry : lruMap.entrySet()) {
                pairs.add(new CachePair(entry.getKey(), entry.getValue().value));
            }
        }
        return pairs;
    }

    private void setCacheables(Collection<CachePair> pairs) {
        for (CachePair pair : pairs) {
            put(pair.queryKey, pair.cacheable);
        }
    }

    /**
     * Restore the geometry cache from the file system
     */
    @SuppressWarnings("unchecked")
    public static synchronized void restoreCache(File cacheFile) {
        try {
            FileLocker.lock(MapQueryCache.class, cacheFile, Type.READ);
            if (cacheFile.exists() && cacheFile.length() > 0) {
                FileInputStream fin = new FileInputStream(cacheFile);
                Collection<CachePair> pairs = (Collection<CachePair>) DynamicSerializationManager
                        .getManager(SerializationType.Thrift).deserialize(fin);
                mapQueryCache.setCacheables(pairs);
            }
        } catch (Exception e) {
            statusHandler.error("Error restoring cache from file system", e);
            e.printStackTrace();
        } finally {
            FileLocker.unlock(MapQueryCache.class, cacheFile);
        }
    }

    /**
     * Store the geometry cache to the file system
     */
    public static synchronized void storeCache(File cacheFile) {
        try {
            FileLocker.lock(MapQueryCache.class, cacheFile, Type.WRITE);
            FileOutputStream out = new FileOutputStream(cacheFile);
            DynamicSerializationManager.getManager(SerializationType.Thrift)
                    .serialize(mapQueryCache.getCacheables(), out);
            out.close();
        } catch (Exception e) {
            statusHandler.error("Error storing cache to file system", e);
            e.printStackTrace();
        } finally {
            FileLocker.unlock(MapQueryCache.class, cacheFile);
        }
    }

}
