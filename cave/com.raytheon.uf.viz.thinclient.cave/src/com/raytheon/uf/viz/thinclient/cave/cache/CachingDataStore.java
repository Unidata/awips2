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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.security.MessageDigest;
import java.util.Map;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.cache.LRUCacheFS;

/**
 * Data store used to cache requests to the filesystem to save bandwidth
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2011             mschenke     Initial creation
 * Feb 12, 2013     #1608  randerso     Added explicit deletes for groups and datasets
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2013            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class CachingDataStore implements IDataStore {

    // quick byte string to hex conversion
    private static final String HEXES = "0123456789ABCDEF";

    private IDataStore delegate;

    private File cacheDir;

    CachingDataStore(IDataStore delegate, File cacheDir) {
        this.delegate = delegate;
        this.cacheDir = cacheDir;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieve(java.lang.String)
     */
    @Override
    public IDataRecord[] retrieve(String group) throws StorageException,
            FileNotFoundException {
        IDataRecord[] records = null;
        File cacheFile = getCacheFile(group);
        if (cacheFile != null && cacheFile.exists()) {
            records = retrieveFromCache(cacheFile, IDataRecord[].class);
        }
        if (records == null) {
            records = delegate.retrieve(group);
            storeToCache(cacheFile, records);
        }
        return records;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieve(java.lang.String,
     * boolean)
     */
    @Override
    public IDataRecord[] retrieve(String group, boolean includeInterpolated)
            throws StorageException, FileNotFoundException {
        IDataRecord[] records = null;
        File cacheFile = getCacheFile(new Object[] { group, includeInterpolated });
        if (cacheFile != null && cacheFile.exists()) {
            records = retrieveFromCache(cacheFile, IDataRecord[].class);
        }
        if (records == null) {
            records = delegate.retrieve(group, includeInterpolated);
            storeToCache(cacheFile, records);
        }
        return records;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieve(java.lang.String,
     * java.lang.String, com.raytheon.uf.common.datastorage.Request)
     */
    @Override
    public IDataRecord retrieve(String group, String dataset, Request request)
            throws StorageException, FileNotFoundException {
        IDataRecord record = null;
        File cacheFile = getCacheFile(new Object[] { group, dataset, request });
        if (cacheFile != null && cacheFile.exists()) {
            record = retrieveFromCache(cacheFile, IDataRecord.class);
        }
        if (record == null) {
            record = delegate.retrieve(group, dataset, request);
            storeToCache(cacheFile, record);
        }
        return record;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieveDatasets(java.lang
     * .String[], com.raytheon.uf.common.datastorage.Request)
     */
    @Override
    public IDataRecord[] retrieveDatasets(String[] datasetGroupPath,
            Request request) throws StorageException, FileNotFoundException {
        IDataRecord[] records = null;
        File cacheFile = getCacheFile(new Object[] { datasetGroupPath, request });
        if (cacheFile != null && cacheFile.exists()) {
            records = retrieveFromCache(cacheFile, IDataRecord[].class);
        }
        if (records == null) {
            records = delegate.retrieveDatasets(datasetGroupPath, request);
            storeToCache(cacheFile, records);
        }
        return records;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieveGroups(java.lang
     * .String[], com.raytheon.uf.common.datastorage.Request)
     */
    @Override
    public IDataRecord[] retrieveGroups(String[] groups, Request request)
            throws StorageException, FileNotFoundException {
        IDataRecord[] records = null;
        File cacheFile = getCacheFile(new Object[] { groups, request });
        if (cacheFile != null && cacheFile.exists()) {
            records = retrieveFromCache(cacheFile, IDataRecord[].class);
        }
        if (records == null) {
            records = delegate.retrieveGroups(groups, request);
            storeToCache(cacheFile, records);
        }
        return records;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#getDatasets(java.lang.String
     * )
     */
    @Override
    public String[] getDatasets(String group) throws StorageException,
            FileNotFoundException {
        String[] datasets = null;
        File cacheFile = getCacheFile(group);
        if (cacheFile != null && cacheFile.exists()) {
            datasets = retrieveFromCache(cacheFile, String[].class);
        }

        if (datasets == null) {
            datasets = delegate.getDatasets(group);
            storeToCache(cacheFile, datasets);
        }
        return datasets;
    }

    @SuppressWarnings("unchecked")
    private <T> T retrieveFromCache(File file, Class<T> clazz) {
        long t0 = System.currentTimeMillis();
        T rval = null;
        try {
            FileInputStream fin = new FileInputStream(file);
            Object fromFile = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).deserialize(fin);
            fin.close();
            if (clazz.isInstance(fromFile)) {
                rval = (T) fromFile;
            }
        } catch (Throwable e) {
            System.err.println("Error retreiving object from cache file: "
                    + e.getLocalizedMessage());
        }

        if (rval != null) {
            LRUCacheFS.poll(file);
        }
        System.out.println("Time to retreive from cache = "
                + (System.currentTimeMillis() - t0) + "ms");
        return rval;
    }

    private void storeToCache(File file, Object result) {
        long t0 = System.currentTimeMillis();
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
                System.err.println("Error storing object to file: "
                        + e.getLocalizedMessage());
            }
        }
        System.out.println("Time to store to cache = "
                + (System.currentTimeMillis() - t0) + "ms");
    }

    private File getCacheFile(Object obj) {
        long t0 = System.currentTimeMillis();
        try {
            byte[] thriftDigest = SerializationUtil.transformToThrift(obj);
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(thriftDigest);
            String md5sum = toHex(md.digest());
            return new File(cacheDir, md5sum);
        } catch (Exception e) {
            System.err.println("Error getting cache file: "
                    + e.getLocalizedMessage());
        } finally {
            System.out.println("Time to getCacheFile = "
                    + (System.currentTimeMillis() - t0) + "ms");
        }
        return null;
    }

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

    // Non-caching methods

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#deleteFiles(java.lang.String
     * [])
     */
    @Override
    public void deleteFiles(String[] datesToDelete) throws StorageException,
            FileNotFoundException {
        delegate.deleteFiles(datesToDelete);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#createDataset(com.raytheon
     * .uf.common.datastorage.records.IDataRecord)
     */
    @Override
    public void createDataset(IDataRecord rec) throws StorageException,
            FileNotFoundException {
        delegate.createDataset(rec);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#addDataRecord(com.raytheon
     * .uf.common.datastorage.records.IDataRecord,
     * com.raytheon.uf.common.datastorage.StorageProperties)
     */
    @Override
    public void addDataRecord(IDataRecord dataset, StorageProperties properties)
            throws StorageException {
        delegate.addDataRecord(dataset, properties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#addDataRecord(com.raytheon
     * .uf.common.datastorage.records.IDataRecord)
     */
    @Override
    public void addDataRecord(IDataRecord dataset) throws StorageException {
        delegate.addDataRecord(dataset);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datastorage.IDataStore#store()
     */
    @Override
    public StorageStatus store() throws StorageException {
        return delegate.store();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#deleteDatasets(java.lang
     * .String[])
     */
    @Override
    public void deleteDatasets(String... datasets) throws StorageException,
            FileNotFoundException {
        delegate.deleteDatasets(datasets);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#deleteGroups(java.lang.
     * String[])
     */
    @Override
    public void deleteGroups(String... groups) throws StorageException,
            FileNotFoundException {
        delegate.deleteGroups(groups);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#store(com.raytheon.uf.common
     * .datastorage.IDataStore.StoreOp)
     */
    @Override
    public StorageStatus store(StoreOp storeOp) throws StorageException {
        return delegate.store(storeOp);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#createLinks(java.util.Map)
     */
    @Override
    public void createLinks(Map<String, LinkLocation> links)
            throws StorageException, FileNotFoundException {
        delegate.createLinks(links);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#repack(com.raytheon.uf.
     * common.datastorage.StorageProperties.Compression)
     */
    @Override
    public void repack(Compression compression) throws StorageException {
        delegate.repack(compression);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datastorage.IDataStore#copy(java.lang.String,
     * com.raytheon.uf.common.datastorage.StorageProperties.Compression,
     * java.lang.String, int, int)
     */
    @Override
    public void copy(String outputDir, Compression compression,
            String timestampCheck, int minMillisSinceLastChange,
            int maxMillisSinceLastChange) throws StorageException {
        delegate.copy(outputDir, compression, timestampCheck,
                minMillisSinceLastChange, maxMillisSinceLastChange);
    }
}
