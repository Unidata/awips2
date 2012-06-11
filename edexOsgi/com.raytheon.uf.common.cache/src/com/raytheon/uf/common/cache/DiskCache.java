package com.raytheon.uf.common.cache;

import java.io.File;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.SystemUtil;

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

/**
 * TODO Remove old cache/hung cache. Cache should be removed on workspace exit.
 * 
 * A object cache that writes all objects to disk. Each object is also kept in a
 * map of soft references. This will allow for the cache to grow as needed and
 * will objects in memory until a garbage collection is requested. Items can be
 * removed from the cache if they are no longer needed.
 * 
 * TODO Features to add:
 * 
 * 1) Configure cache to allow hard references based on configuration (last 20
 * objects for example)
 * 
 * 2) Specifcy a name/configuration for DiskCache's to allow for disk caches
 * with different configurations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 5, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DiskCache<K> implements ICache<K> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiskCache.class.getPackage().getName(), "CAVE",
                    "WORKSTATION");

    protected String name;

    protected String baseCacheDir;

    /**
     * Number of items allowed in the mem cache map. Defaults to 100 items.
     */
    private int sizeMemCacheMap = 100;

    // unique per jvm, configured DiskCache instance, not clusterable
    protected File cacheDir;

    protected class MetaData {
        private Object syncObj = null;

        private String cacheFilePath = null;

        private SoftReference<K> softRef = null;

        private K ref = null;
    }

    private class RefMap<X extends String, V extends MetaData> extends
            LinkedHashMap<X, V> {

        /**
         * 
         */
        public RefMap() {
            super();
            // TODO Auto-generated constructor stub
        }

        /**
         * @param initialCapacity
         * @param loadFactor
         * @param accessOrder
         */
        public RefMap(int initialCapacity, float loadFactor, boolean accessOrder) {
            super(initialCapacity, loadFactor, accessOrder);
            // TODO Auto-generated constructor stub
        }

        /**
         * @param initialCapacity
         * @param loadFactor
         */
        public RefMap(int initialCapacity, float loadFactor) {
            super(initialCapacity, loadFactor);
            // TODO Auto-generated constructor stub
        }

        /**
         * @param initialCapacity
         */
        public RefMap(int initialCapacity) {
            super(initialCapacity);
            // TODO Auto-generated constructor stub
        }

        /**
         * @param m
         */
        public RefMap(Map<? extends X, ? extends V> m) {
            super(m);
            // TODO Auto-generated constructor stub
        }

        @Override
        protected boolean removeEldestEntry(Entry<X, V> eldest) {
            boolean rval = size() > sizeMemCacheMap;

            if (rval) {
                MetaData md = eldest.getValue();
                cacheWriter.asyncWrite(md.cacheFilePath, md.ref, md.syncObj);
                md.softRef = new SoftReference<K>(md.ref);
                md.ref = null;
                softMetaDataMap.put(eldest.getKey(), eldest.getValue());
            }

            return rval;
        }
    }

    /**
     * Should this be static or one writer thread per cache? Only have so much
     * through put to disk.
     */
    protected static DiskCacheWriter cacheWriter;

    private ConcurrentMap<String, MetaData> softMetaDataMap = new ConcurrentHashMap<String, MetaData>(
            512);

    private LinkedHashMap<String, MetaData> metaDataMap = new RefMap<String, MetaData>(
            128, 0.75f, true);

    static {
        cacheWriter = new DiskCacheWriter();
        cacheWriter.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.cache.ICache#getFromCache(java.lang.String)
     */
    @SuppressWarnings("unchecked")
    @Override
    public K getFromCache(String id) {
        MetaData md = null;
        K obj = null;

        // check the hard ref map
        synchronized (metaDataMap) {
            md = metaDataMap.get(id);
        }

        if (md != null) {
            obj = md.ref;
        } else {
            // check the soft ref map
            md = softMetaDataMap.get(id);

            if (md == null) {
                // object not cached
                return null;
            }

            // cancel pending write for data if pending
            obj = (K) cacheWriter.cancelWrite(md.cacheFilePath);

            if (obj == null) {
                obj = md.softRef.get();
            }

            if (obj == null) {
                // object no longer in memory, read from disk
                byte[] data = null;

                try {
                    synchronized (md.syncObj) {
                        // verify data wasn't already retrieved
                        if (md.ref == null) {
                            if (data == null) {
                                // data wasn't pending, read from disk
                                File f = new File(md.cacheFilePath);
                                data = FileUtil.file2bytes(f);
                            }

                            obj = (K) SerializationUtil
                                    .transformFromThrift(data);
                            md.ref = obj;
                        }
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Error occurred retrieving cached data from disk",
                            e);
                }
            }

            // add object back to hard cache
            md.ref = obj;
            md.softRef = null;
            synchronized (metaDataMap) {
                metaDataMap.put(id, md);
            }
        }

        return obj;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.cache.ICache#removeFromCache(java.lang.String)
     */
    @Override
    public void removeFromCache(String id) {
        MetaData md = null;
        synchronized (metaDataMap) {
            md = metaDataMap.remove(id);
        }
        if (md == null) {
            md = softMetaDataMap.remove(id);
        } else {
            softMetaDataMap.remove(id);
        }

        if (md != null && md.cacheFilePath != null) {
            cacheWriter.cancelWrite(md.cacheFilePath);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.cache.ICache#addToCache(java.lang.String, K)
     */
    @Override
    public void addToCache(String id, K obj) throws IOException {
        MetaData md = null;

        // check map of hard refs
        synchronized (metaDataMap) {
            md = metaDataMap.get(id);
        }

        // No hard ref, check for soft ref
        if (md == null) {
            md = softMetaDataMap.get(id);
        }

        // no previous cache'd entry, make new one
        if (md == null) {
            md = new MetaData();
            md.syncObj = new Object();
            md.cacheFilePath = File.createTempFile("cache", ".bin", cacheDir)
                    .getAbsolutePath();
        }

        synchronized (metaDataMap) {
            metaDataMap.put(id, md);
        }

        md.softRef = null;
        md.ref = obj;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.cache.ICache#addToCache(K)
     */
    @Override
    public String addToCache(K obj) throws IOException {
        MetaData md = new MetaData();
        md.syncObj = new Object();
        md.cacheFilePath = File.createTempFile("cache", ".bin", cacheDir)
                .getAbsolutePath();

        synchronized (metaDataMap) {
            metaDataMap.put(md.cacheFilePath, md);
        }

        md.ref = obj;
        md.softRef = null;

        // unique id will be the unique temp file created
        return md.cacheFilePath;
    }

    public void closeCache() {
        cacheWriter.run = false;
    }
    
    public void clearCache() {
    	metaDataMap.clear();
    }

    public int getSizeMemCacheMap() {
        return sizeMemCacheMap;
    }

    public void setSizeMemCacheMap(int sizeMemCacheMap) {
        this.sizeMemCacheMap = sizeMemCacheMap;

        // need to push extra entries to disk?
        if (sizeMemCacheMap > metaDataMap.size()) {
            synchronized (metaDataMap) {
                RefMap<String, MetaData> tmp = new RefMap<String, MetaData>(
                        (int) (sizeMemCacheMap * 1.25) + 1, 0.75f, true);
                tmp.putAll(metaDataMap);
                metaDataMap = tmp;
            }
        }

        this.sizeMemCacheMap = sizeMemCacheMap;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getBaseCacheDir() {
        return baseCacheDir;
    }

    public void setBaseCacheDir(String baseCacheDir) {
        this.baseCacheDir = baseCacheDir;
    }

    public void activateCache() {
        int pid = SystemUtil.getPid();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext userContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.WORKSTATION);

        if (baseCacheDir == null) {
            baseCacheDir = "diskCache";
        }

        String path = baseCacheDir + File.separator + name + File.separator
                + File.separator + "pid_" + pid;
        this.cacheDir = PathManagerFactory.getPathManager().getFile(
                userContext, path);

        if (!cacheDir.exists()) {
            cacheDir.mkdirs();
        }

        CacheFactory factory = CacheFactory.getInstance();
        factory.addCache(name, this);

        // TODO: Throw exception if not properly configured
    }
    
    public void activateEdexCache() {
        int pid = SystemUtil.getPid();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.SITE);

        if (baseCacheDir == null) {
            baseCacheDir = "diskCache";
        }

        String path = baseCacheDir + File.separator + name + File.separator
                + File.separator + "pid_" + pid;
        try {
        	LocalizationFile dir = PathManagerFactory.getPathManager().getLocalizationFile(context, path);
        	this.cacheDir = dir.getFile();
        } catch (Exception e) {
        	// no localization file exists
        	this.cacheDir = new File(path);
        }

        if (!cacheDir.exists()) {
            cacheDir.mkdirs();
        }

        CacheFactory factory = CacheFactory.getInstance();
        factory.addCache(name, this);

        // TODO: Throw exception if not properly configured
    }
}
