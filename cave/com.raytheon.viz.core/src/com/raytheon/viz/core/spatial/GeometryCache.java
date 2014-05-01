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
package com.raytheon.viz.core.spatial;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.uf.common.localization.FileLocker;
import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.cache.ICacheObject;
import com.raytheon.uf.common.util.cache.LRUCache;
import com.raytheon.viz.core.spatial.GeometryCache.GeometryCachable;
import com.raytheon.viz.core.spatial.GeometryCache.GeometryCacheKey;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Fixed memory LRU caching of geometries. Capable of storing/restoring data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GeometryCache extends LRUCache<GeometryCacheKey, GeometryCachable> {

    @DynamicSerialize
    public static class GeometryCacheKey {

        @DynamicSerializeElement
        private String dataset;

        @DynamicSerializeElement
        private String gid;

        @DynamicSerializeElement
        private String geomField;

        public GeometryCacheKey() {

        }

        /**
         * @param dataset
         * @param gid
         * @param geomField
         */
        private GeometryCacheKey(String dataset, String gid, String geomField) {
            this.dataset = dataset;
            this.gid = gid;
            this.geomField = geomField;
        }

        /**
         * @return the dataset
         */
        public String getDataset() {
            return dataset;
        }

        /**
         * @param dataset
         *            the dataset to set
         */
        public void setDataset(String dataset) {
            this.dataset = dataset;
        }

        /**
         * @return the gid
         */
        public String getGid() {
            return gid;
        }

        /**
         * @param gid
         *            the gid to set
         */
        public void setGid(String gid) {
            this.gid = gid;
        }

        /**
         * @return the geomField
         */
        public String getGeomField() {
            return geomField;
        }

        /**
         * @param geomField
         *            the geomField to set
         */
        public void setGeomField(String geomField) {
            this.geomField = geomField;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((dataset == null) ? 0 : dataset.hashCode());
            result = prime * result
                    + ((geomField == null) ? 0 : geomField.hashCode());
            result = prime * result + ((gid == null) ? 0 : gid.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            GeometryCacheKey other = (GeometryCacheKey) obj;
            if (dataset == null) {
                if (other.dataset != null)
                    return false;
            } else if (!dataset.equals(other.dataset))
                return false;
            if (geomField == null) {
                if (other.geomField != null)
                    return false;
            } else if (!geomField.equals(other.geomField))
                return false;
            if (gid == null) {
                if (other.gid != null)
                    return false;
            } else if (!gid.equals(other.gid))
                return false;
            return true;
        }

    }

    @DynamicSerialize
    public static class GeometryCachable implements ICacheObject {

        @DynamicSerializeElement
        private Geometry geom;

        public GeometryCachable() {

        }

        private GeometryCachable(Geometry geom) {
            this.geom = geom;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.common.util.cache.ICacheObject#getSize()
         */
        @Override
        public int getSize() {
            return geom.getNumPoints() * 3 * 8;
        }

        /**
         * @return the geom
         */
        public Geometry getGeom() {
            return geom;
        }

        /**
         * @param geom
         *            the geom to set
         */
        public void setGeom(Geometry geom) {
            this.geom = geom;
        }

    }

    @DynamicSerialize
    public static class GeomCachePair {

        @DynamicSerializeElement
        private GeometryCacheKey key;

        @DynamicSerializeElement
        private GeometryCachable cache;

        public GeomCachePair() {

        }

        private GeomCachePair(GeometryCacheKey key, GeometryCachable cache) {
            this.key = key;
            this.cache = cache;
        }

        /**
         * @return the key
         */
        public GeometryCacheKey getKey() {
            return key;
        }

        /**
         * @param key
         *            the key to set
         */
        public void setKey(GeometryCacheKey key) {
            this.key = key;
        }

        /**
         * @return the cache
         */
        public GeometryCachable getCache() {
            return cache;
        }

        /**
         * @param cache
         *            the cache to set
         */
        public void setCache(GeometryCachable cache) {
            this.cache = cache;
        }

    }

    /** The cache instance */
    private static GeometryCache geometryCache = new GeometryCache();

    /**
     * Attempt to retrieve a geometry from the cache
     * 
     * @param dataset
     * @param gid
     * @param geomId
     * @return the geometry if found, null if not in cache
     */
    public static Geometry getGeometry(String dataset, String gid, String geomId) {
        GeometryCachable cached = geometryCache.get(new GeometryCacheKey(
                dataset, gid, geomId));
        return cached != null ? cached.geom : null;
    }

    /**
     * Add a geometry to the cache
     * 
     * @param dataset
     * @param gid
     * @param geomId
     * @param geom
     */
    public static void putGeometry(String dataset, String gid, String geomId,
            Geometry geom) {
        geometryCache.put(new GeometryCacheKey(dataset, gid, geomId),
                new GeometryCachable(geom));
    }

    /**
     * Restore the geometry cache from the file system
     */
    @SuppressWarnings("unchecked")
    public static synchronized void restoreCache(File cacheFile) {
        try {
            FileLocker.lock(GeometryCache.class, cacheFile, Type.READ);
            if (cacheFile.exists() && cacheFile.length() > 0) {
                FileInputStream fin = new FileInputStream(cacheFile);
                Collection<GeomCachePair> pairs = (Collection<GeomCachePair>) DynamicSerializationManager
                        .getManager(SerializationType.Thrift).deserialize(fin);
                geometryCache.setCacheables(pairs);
            }
        } catch (Exception e) {
            System.err.println("Error restoring cache from file system");
            e.printStackTrace();
        } finally {
            FileLocker.unlock(GeometryCache.class, cacheFile);
        }
    }

    /**
     * Store the geometry cache to the file system
     */
    public static synchronized void storeCache(File cacheFile) {
        try {
            FileLocker.lock(GeometryCache.class, cacheFile, Type.WRITE);
            FileOutputStream out = new FileOutputStream(cacheFile);
            DynamicSerializationManager.getManager(SerializationType.Thrift)
                    .serialize(geometryCache.getCacheables(), out);
            out.close();
        } catch (Exception e) {
            System.err.println("Error storing cache to file system");
            e.printStackTrace();
        } finally {
            FileLocker.unlock(GeometryCache.class, cacheFile);
        }
    }

    /**
     * A 32MB Map cache, could potentially write out to file system if desired
     */
    private GeometryCache() {
        super(32 * 1024 * 1024);
    }

    private Collection<GeomCachePair> getCacheables() {
        List<GeomCachePair> pairs = new ArrayList<GeomCachePair>(lruMap.size());
        synchronized (this) {
            for (Entry<GeometryCacheKey, Item> entry : lruMap.entrySet()) {
                pairs.add(new GeomCachePair(entry.getKey(),
                        entry.getValue().value));
            }
        }
        return pairs;
    }

    private void setCacheables(Collection<GeomCachePair> pairs) {
        for (GeomCachePair pair : pairs) {
            put(pair.key, pair.cache);
        }
    }
}
