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
package com.raytheon.uf.viz.thinclient.cave.cache;

import java.awt.Point;
import java.awt.Rectangle;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;
import java.security.MessageDigest;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.Request.Type;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.cache.LRUCacheFS;
import com.raytheon.uf.viz.core.data.BufferSlicer;

/**
 * Caching layer used by {@link CachingDataStore}. Caches two things:
 * <ul>
 * <li>
 * For a group all dataset names are cached.</li>
 * <li>
 * For a dataset a IDataRecord is cached for each request that has been
 * performed.</li>
 * </ul>
 * 
 * These two pieces of information together can fulfill any {@link IDataStore}
 * request if the data has been requested before. This class maintains a short
 * lived memory cache using {@link SoftReference} and also a persistent disk
 * cache.
 * 
 * In addition to simply caching java objects this class also compares new
 * {@link Request} objects to what is already in the cache and attempts to reuse
 * cached {@link IDataRecord} objects whenever possible.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 18, 2013  2309     bsteffen    Initial creation
 * Dec 04, 2013  2600     bsteffen    Fix typo in contains.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @see IDataStore
 */

public class DataStoreCache {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataStoreCache.class, "ThinClient");

    // quick byte string to hex conversion
    private static final String HEXES = "0123456789ABCDEF";

    private final File cacheDir;

    private final Map<String, Reference<String[]>> namesMemoryCache;

    private final Map<String, Reference<Map<Request, IDataRecord>>> dataMemoryCache;

    /**
     * Construct a DataStoreCache that will store cache files in the given
     * cacheDir.
     * 
     * @param cacheDir
     *            directory for storing files. If the directory does not exist
     *            it will be created. If the directory cannot be created or
     *            written too then disk caching will not work and lots of
     *            warnings will be sent to UFStatus.
     */
    public DataStoreCache(File cacheDir) {
        this.cacheDir = cacheDir;
        this.namesMemoryCache = Collections
                .synchronizedMap(new HashMap<String, Reference<String[]>>());
        this.dataMemoryCache = Collections
                .synchronizedMap(new HashMap<String, Reference<Map<Request, IDataRecord>>>());
    }

    /**
     * Gets the dataset names for a group from this cache.
     * 
     * @param group
     *            the name of a group, must not be null
     * @return an array of dataset names or null if they are not in this cache
     * @see IDataStore#getDatasets(String)
     * @see #cacheDatasetNames(String, String[])
     */
    public String[] getDatasetNames(String group) {
        String[] names = getNamesFromMemory(group);
        if (names == null) {
            names = getNamesFromFile(group);
            if (names != null) {
                cacheNamesInMemory(group, names);
            }
        }
        return names;
    }

    /**
     * Stores dataset names for a group in this cache. The names can be
     * retrieved from the cache using {@link #getDatasetNames(String)}
     * 
     * @param group
     *            the name of a group, must not be null
     * @param datasetNames
     *            the names of all datasets in the group, must not be null
     * @see #getDatasetNames(String)
     */
    public void cacheDatasetNames(String group, String[] datasetNames) {
        cacheNamesInMemory(group, datasetNames);
        cacheNamesInFile(group, datasetNames);
    }

    private String[] getNamesFromMemory(String group) {
        Reference<String[]> ref = namesMemoryCache.get(group);
        if (ref != null) {
            return ref.get();
        }
        return null;
    }

    private String[] getNamesFromFile(String group) {
        File cacheFile = getCacheFile(group);
        if (cacheFile != null && cacheFile.exists()) {
            return retrieveFromCache(cacheFile, String[].class);

        }
        return null;
    }

    private void cacheNamesInMemory(String group, String[] names) {
        namesMemoryCache.put(group, new SoftReference<String[]>(names));
    }

    private void cacheNamesInFile(String group, String[] names) {
        storeToCache(getCacheFile(group), names);
    }

    /**
     * Gets an {@link IDataRecord} for a specific dataset/group path and request
     * from this cache.
     * 
     * @param datasetGroupPath
     *            the group and dataset concatenated together with a
     *            {@link DataStoreFactory#DEF_SEPARATOR}, must not be null.
     * @param request
     *            the request defining the portion of the dataset that is
     *            needed, must not be null.
     * @return an IDataRecord or null if it is not in the cache.
     * @see IDataStore#retrieveDatasets(String[], Request)
     * @see #cacheDataset(String, IDataRecord, Request)
     */
    public IDataRecord getDataset(String datasetGroupPath, Request request) {
        Map<Request, IDataRecord> data = getData(datasetGroupPath);
        if (data == null) {
            return null;
        } else {
            synchronized (data) {
                if (data.containsKey(request)) {
                    return data.get(request);
                } else {
                    for (Entry<Request, IDataRecord> entry : data.entrySet()) {
                        Request cachedRequest = entry.getKey();
                        if (contains(cachedRequest, request)) {
                            IDataRecord newRecord = slice(request,
                                    cachedRequest, entry.getValue());
                            if (newRecord != null) {
                                return newRecord;
                            }
                        }
                    }
                }
                return null;
            }
        }
    }

    /**
     * Stores a portion of a dataset corresponding to request in this cache. The
     * record can be retrieved from the cache using
     * {@link #getDataset(String, Request)}
     * 
     * @param datasetGroupPath
     *            the group and dataset concatenated together with a
     *            {@link DataStoreFactory#DEF_SEPARATOR}, must not be null.
     * @param record
     *            the data record containing data corresponding to the request,
     *            must not be null.
     * @param request
     *            the request defining the portion of the dataset that is
     *            needed, must not be null.
     * @see #getDataset(String, Request)
     */
    public void cacheDataset(String datasetGroupPath, IDataRecord record,
            Request request) {
        Map<Request, IDataRecord> data = getData(datasetGroupPath);
        if (data != null) {
            synchronized (data) {
                /*
                 * Minimize the size of the cache by ensuring any requests that
                 * are a subset of another request are not cached.
                 */
                Iterator<Request> it = data.keySet().iterator();
                while (it.hasNext()) {
                    Request cachedRequest = it.next();
                    if (contains(cachedRequest, request)) {
                        return;
                    } else if (contains(request, cachedRequest)) {
                        it.remove();
                    }
                }
                data.put(request, record);
                cacheDataInFile(datasetGroupPath, data);
            }
        } else {
            data = new HashMap<Request, IDataRecord>();
            data.put(request, record);
            cacheData(datasetGroupPath, data);
        }
    }

    private Map<Request, IDataRecord> getData(String datasetGroupPath) {
        Map<Request, IDataRecord> data = getDataFromMemory(datasetGroupPath);
        if (data == null) {
            data = getDataFromFile(datasetGroupPath);
            if (data != null) {
                cacheDataInMemory(datasetGroupPath, data);
            }
        }
        return data;
    }

    private Map<Request, IDataRecord> getDataFromMemory(String datasetGroupPath) {
        Reference<Map<Request, IDataRecord>> ref = dataMemoryCache
                .get(datasetGroupPath);
        if (ref != null) {
            return ref.get();
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private Map<Request, IDataRecord> getDataFromFile(String datasetGroupPath) {
        File cacheFile = getCacheFile(datasetGroupPath);
        if (cacheFile != null && cacheFile.exists()) {
            return retrieveFromCache(cacheFile, Map.class);
        }
        return null;
    }

    private void cacheData(String datasetGroupPath,
            Map<Request, IDataRecord> data) {
        cacheDataInMemory(datasetGroupPath, data);
        cacheDataInFile(datasetGroupPath, data);
    }

    private void cacheDataInMemory(String datasetGroupPath,
            Map<Request, IDataRecord> data) {
        dataMemoryCache.put(datasetGroupPath,
                new SoftReference<Map<Request, IDataRecord>>(data));
    }

    private void cacheDataInFile(String datasetGroupPath,
            Map<Request, IDataRecord> data) {
        storeToCache(getCacheFile(datasetGroupPath), data);
    }

    /**
     * Determine if the data returned by outer contains enough of the data to
     * fulfill inner.
     * 
     * @return true if outer has enough data for inner, false if not.
     */
    private static boolean contains(Request outer, Request inner) {
        Type outerType = outer.getType();
        Type innerType = inner.getType();
        if (outerType == Type.ALL) {
            return true;
        } else if (outerType == Type.SLAB) {
            if (innerType == Type.SLAB) {
                return getRectangle(outer).contains(getRectangle(inner));
            }
            if (innerType == Type.POINT) {
                Rectangle rect = getRectangle(outer);
                for (Point p : inner.getPoints()) {
                    if (!rect.contains(p)) {
                        return false;
                    }
                }
                return true;
            }
        } else if (outerType == Type.POINT) {
            if (innerType == Type.POINT) {
                Set<Point> outerSet = new HashSet<Point>(Arrays.asList(outer
                        .getPoints()));
                for (Point p : inner.getPoints()) {
                    if (!outerSet.contains(p)) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    /**
     * Create a new {@link IDataRecord} with a subset of the data in
     * cachedRecord. The new record will be the data needed to fulfill
     * newRequest. This method chould only be used if cacheRequest contains
     * newRequest.
     */
    private static IDataRecord slice(Request newRequest, Request cacheRequest,
            IDataRecord cachedRecord) {
        Type cacheType = cacheRequest.getType();
        Type newType = newRequest.getType();
        if (cacheType == Type.ALL) {
            if (newType == Type.SLAB) {
                return sliceAllToSlab(newRequest, cacheRequest, cachedRecord);
            }
        } else if (cacheType == Type.SLAB) {
            if (newType == Type.SLAB) {
                return sliceSlabToSlab(newRequest, cacheRequest, cachedRecord);
            }

        }
        return null;

    }

    /**
     * Specialized slice handling for getting a slab record from an ALL request.
     */
    private static IDataRecord sliceAllToSlab(Request newRequest,
            Request cacheRequest, IDataRecord cachedRecord) {
        Buffer buffer = toBuffer(cachedRecord);
        if (buffer == null) {
            return null;
        }
        Rectangle newRect = getRectangle(newRequest);
        long[] sizes = cachedRecord.getSizes();
        Rectangle cacheRect = new Rectangle((int) sizes[0], (int) sizes[1]);
        buffer = BufferSlicer.slice(buffer, newRect, cacheRect);
        IDataRecord newRecord = toDataRecord(buffer, newRect, cachedRecord);
        return newRecord;
    }

    /**
     * Specialized slice handling for getting a slab record from a larger slab
     * record.
     */
    private static IDataRecord sliceSlabToSlab(Request newRequest,
            Request cacheRequest, IDataRecord cachedRecord) {
        Buffer buffer = toBuffer(cachedRecord);
        if (buffer == null) {
            return null;
        }
        Rectangle newRect = getRectangle(newRequest);
        Rectangle cacheRect = getRectangle(cacheRequest);
        buffer = BufferSlicer.slice(buffer, newRect, cacheRect);
        IDataRecord newRecord = toDataRecord(buffer, newRect, cachedRecord);
        return newRecord;
    }

    /**
     * Extract the raw numeric data from a {@link IDataRecord} in {@link Buffer}
     * form.
     */
    private static Buffer toBuffer(IDataRecord record) {
        if (record instanceof FloatDataRecord) {
            return FloatBuffer.wrap(((FloatDataRecord) record).getFloatData());
        } else if (record instanceof IntegerDataRecord) {
            return IntBuffer.wrap(((IntegerDataRecord) record).getIntData());
        } else if (record instanceof ShortDataRecord) {
            return ShortBuffer.wrap(((ShortDataRecord) record).getShortData());
        } else if (record instanceof ByteDataRecord) {
            return ByteBuffer.wrap(((ByteDataRecord) record).getByteData());
        }
        return null;
    }

    /**
     * Convert a {@link Buffer} into an {@link IDataRecord}, the record will
     * have dataset attributes bfrom copy but with new sizes.
     */
    private static IDataRecord toDataRecord(Buffer buffer, Rectangle sizes,
            IDataRecord copy) {
        String name = copy.getName();
        String group = copy.getGroup();
        IDataRecord record = null;
        if (buffer instanceof FloatBuffer) {
            record = new FloatDataRecord(name, group,
                    ((FloatBuffer) buffer).array());
        } else if (buffer instanceof IntBuffer) {
            record = new IntegerDataRecord(name, group,
                    ((IntBuffer) buffer).array());
        } else if (buffer instanceof ShortBuffer) {
            record = new ShortDataRecord(name, group,
                    ((ShortBuffer) buffer).array());
        } else if (buffer instanceof ByteBuffer) {
            record = new ByteDataRecord(name, group,
                    ((ByteBuffer) buffer).array());
        } else {
            return null;
        }
        record.setSizes(new long[] { sizes.width, sizes.height });
        record.setDataAttributes(copy.getDataAttributes());
        record.setDimension(copy.getDimension());
        record.setFillValue(copy.getFillValue());
        record.setProperties(copy.getProperties());
        return record;
    }

    /**
     * Convert a slab {@link Request} into a {@link Rectangle}.
     */
    private static Rectangle getRectangle(Request request) {
        int[] max = request.getMaxIndexForSlab();
        int[] min = request.getMinIndexForSlab();
        return new Rectangle(min[0], min[1], max[0] - min[0], max[1] - min[1]);
    }

    /**
     * Get a unique File to store a cached object
     */
    private File getCacheFile(String obj) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(obj.getBytes());
            String md5sum = toHex(md.digest());
            return new File(cacheDir, md5sum);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "Error getting cache file", e);
        }
        return null;
    }

    /**
     * Load a java object from a cache file.
     * 
     * @param file
     *            the file containing the object
     * @param clazz
     *            the type of the object we are expecting
     * @return an object of type clazz loaded from file.
     */
    private static <T> T retrieveFromCache(File file, Class<T> clazz) {
        T rval = null;
        try {
            FileInputStream fin = new FileInputStream(file);
            Object fromFile = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).deserialize(fin);
            fin.close();
            if (clazz.isInstance(fromFile)) {
                rval = clazz.cast(fromFile);
            }
        } catch (Throwable e) {
            statusHandler.handle(Priority.WARN,
                    "Error retreiving object from cache file", e);
        }

        if (rval != null) {
            LRUCacheFS.poll(file);
        }
        return rval;
    }

    /**
     * Store a java object into a file
     */
    private static void storeToCache(File file, Object result) {
        if (result != null) {
            try {
                if (!file.getParentFile().exists()) {
                    file.getParentFile().mkdirs();
                }
                FileOutputStream fout = new FileOutputStream(file);
                DynamicSerializationManager
                        .getManager(SerializationType.Thrift).serialize(result,
                                fout);
                fout.close();
                LRUCacheFS.poll(file);
                file.setReadable(true, false);
                file.setWritable(true, false);
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "Error storing object to file", e);
            }
        }
    }

    /**
     * Convert some bytes to hex, used to generate unique file names.
     */
    private static String toHex(byte[] raw) {
        if (raw == null) {
            return null;
        }
        final StringBuilder hex = new StringBuilder(2 * raw.length);
        for (final byte b : raw) {
            hex.append(HEXES.charAt((b & 0xF0) >> 4)).append(
                    HEXES.charAt((b & 0x0F)));
        }
        return hex.toString();
    }

}
