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
package com.raytheon.uf.viz.thinclient.cave.cache.map;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.FileLocker;
import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.maps.rsc.DbMapQueryFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CacheDbMapQueryFactory extends DbMapQueryFactory {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CacheDbMapQueryFactory.class);

    private static CacheDbMapQueryFactory instance;

    private Map<String, CacheDbMapQuery> cache = new HashMap<String, CacheDbMapQuery>();

    @Override
    protected CacheDbMapQuery getMapQueryInternal(String table, String geomField) {
        String key = table + ":::" + geomField;
        synchronized (cache) {
            CacheDbMapQuery query = cache.get(key);
            if (query == null) {
                query = new CacheDbMapQuery(table, geomField);
                cache.put(key, query);
            }
            return query;
        }
    }

    public static synchronized void setEnableCaching(boolean enabled) {
        if (enabled) {
            if (instance == null) {
                instance = new CacheDbMapQueryFactory();
            }
            setCustomInstance(instance);
        } else {
            setCustomInstance(null);
        }
    }

    public static synchronized void restoreCache(File cacheFile) {
        if (instance == null) {
            setCustomInstance(instance = new CacheDbMapQueryFactory());
        }
        synchronized (instance.cache) {
            try {
                FileLocker.lock(CacheDbMapQuery.class, cacheFile, Type.READ);
                if (cacheFile.exists() && cacheFile.length() > 0) {
                    FileInputStream fin = new FileInputStream(cacheFile);
                    List<CacheDbMapQuerySerializeable> list = (List<CacheDbMapQuerySerializeable>) DynamicSerializationManager
                            .getManager(SerializationType.Thrift).deserialize(
                                    fin);
                    for (CacheDbMapQuerySerializeable s : list) {
                        CacheDbMapQuery c = new CacheDbMapQuery(s);
                        String key = c.getTable() + ":::" + c.getGeomField();
                        instance.cache.put(key, c);
                    }
                }
            } catch (Exception e) {
                statusHandler
                        .error("Error restoring cache from file system", e);
                e.printStackTrace();
            } finally {
                FileLocker.unlock(CacheDbMapQuery.class, cacheFile);
            }
        }
    }

    /**
     * Store the geometry cache to the file system
     */
    public static synchronized void storeCache(File cacheFile) {
        if (instance == null) {
            return;
        }
        synchronized (instance.cache) {
            try {
                FileLocker.lock(CacheDbMapQuery.class, cacheFile, Type.WRITE);
                FileOutputStream out = new FileOutputStream(cacheFile);
                List<CacheDbMapQuerySerializeable> list = new ArrayList<CacheDbMapQuerySerializeable>(
                        instance.cache.size());
                for (CacheDbMapQuery value : instance.cache.values()) {
                    list.add(value.getSerializeable());
                }
                DynamicSerializationManager
                        .getManager(SerializationType.Thrift).serialize(list,
                                out);
                out.close();
            } catch (Exception e) {
                statusHandler.error("Error storing cache to file system", e);
                e.printStackTrace();
            } finally {
                FileLocker.unlock(CacheDbMapQuery.class, cacheFile);
            }
        }
    }

}
