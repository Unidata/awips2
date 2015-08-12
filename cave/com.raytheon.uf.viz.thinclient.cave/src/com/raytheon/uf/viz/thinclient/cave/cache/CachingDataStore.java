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

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * Data store which wraps with another {@link IDataStore}. This data store will
 * always check with its cache before using the delegate and adds all results to
 * the cache.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 08, 2011           mschenke    Initial creation
 * Feb 12, 2013  1608     randerso    Added explicit deletes for groups and
 *                                    datasets
 * Sep 18, 2013  2309     bsteffen    Move disk acces to DataStoreCache
 * Nov 14, 2013  2393     bclement    removed datastore interpolation
 * Jul 30, 2015  1574     nabowle     Add #deleteOrphanData(Date)
 *
 * </pre>
 *
 * @author mschenke
 * @version 1.0
 */
public class CachingDataStore implements IDataStore {

    private final IDataStore delegate;

    private DataStoreCache cache;

    CachingDataStore(IDataStore delegate, DataStoreCache cache) {
        this.delegate = delegate;
        this.cache = cache;
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
        String[] datasets = cache.getDatasetNames(group);
        if (datasets != null) {
            List<String> datasetGroupPaths = new ArrayList<String>(
                    datasets.length);
            List<IDataRecord> records = new ArrayList<IDataRecord>();
            for (String dataset : datasets) {
                String datasetGroupPath = toDatasetGroupPath(group, dataset);
                IDataRecord record = cache.getDataset(datasetGroupPath,
                        Request.ALL);
                if (record == null) {
                    datasetGroupPaths.add(datasetGroupPath);
                } else {
                    records.add(record);
                }
            }
            if (!datasetGroupPaths.isEmpty()) {
                IDataRecord[] newRecords = retrieveDatasets(
                        datasetGroupPaths.toArray(new String[0]), Request.ALL);
                for (int i = 0; i < newRecords.length; i += 1) {
                    cache.cacheDataset(datasetGroupPaths.get(i), newRecords[i],
                            Request.ALL);
                }
                records.addAll(Arrays.asList(newRecords));
            }
            return records.toArray(new IDataRecord[0]);
        } else {
            IDataRecord[] records = delegate.retrieve(group);
            cacheDatasets(group, Arrays.asList(records), Request.ALL);
            return records;
        }
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
        String datasetGroupPath = toDatasetGroupPath(group, dataset);
        IDataRecord record = cache.getDataset(datasetGroupPath, request);
        if (record == null) {
            record = delegate.retrieve(group, dataset, request);
            cache.cacheDataset(datasetGroupPath, record, request);
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
    public IDataRecord[] retrieveDatasets(String[] datasetGroupPaths,
            Request request) throws StorageException, FileNotFoundException {
        Map<String, IDataRecord> records = new HashMap<String, IDataRecord>();
        List<String> toRequest = new ArrayList<String>(datasetGroupPaths.length);
        for (String datasetGroupPath : datasetGroupPaths) {
            IDataRecord record = cache.getDataset(datasetGroupPath, request);
            if (record == null) {
                toRequest.add(datasetGroupPath);
            } else {
                records.put(datasetGroupPath, record);
            }
        }
        if (!toRequest.isEmpty()) {
            IDataRecord[] newRecords = delegate.retrieveDatasets(
                    toRequest.toArray(new String[0]), request);
            for (int i = 0; i < newRecords.length; i += 1) {
                String datasetGroupPath = toRequest.get(i);
                IDataRecord record = newRecords[i];
                cache.cacheDataset(datasetGroupPath, record, request);
                records.put(datasetGroupPath, record);
            }
        }
        IDataRecord[] result = new IDataRecord[datasetGroupPaths.length];
        for (int i = 0; i < datasetGroupPaths.length; i += 1) {
            result[i] = records.get(datasetGroupPaths[i]);
        }
        return result;
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
        List<String> toRequest = new ArrayList<String>();
        Map<String, List<IDataRecord>> records = new HashMap<String, List<IDataRecord>>();
        for (String group : groups) {
            String[] datasets = cache.getDatasetNames(group);
            if (datasets != null) {
                IDataRecord[] cachedRecords = new IDataRecord[datasets.length];
                for (int i = 0; i < datasets.length; i += 1) {
                    cachedRecords[i] = cache.getDataset(
                            toDatasetGroupPath(group, datasets[i]),
                            request);
                    if (cachedRecords[i] == null) {
                        toRequest.add(group);
                        cachedRecords = null;
                        break;
                    }
                }
                if (cachedRecords != null) {
                    records.put(group, Arrays.asList(cachedRecords));
                }
            } else {
                toRequest.add(group);
            }
        }
        if (!toRequest.isEmpty()) {
            IDataRecord[] newRecords = delegate.retrieveGroups(
                    toRequest.toArray(new String[0]), request);
            for (IDataRecord record : newRecords) {
                String group = getGroup(record);
                List<IDataRecord> groupRecs = records.get(group);
                if (groupRecs == null) {
                    groupRecs = new ArrayList<IDataRecord>();
                    records.put(group, groupRecs);
                }
                groupRecs.add(record);
            }
            for (String group : toRequest) {
                cacheDatasets(group, records.get(group), request);
            }
        }
        List<IDataRecord> result = new ArrayList<IDataRecord>();
        for (String group : groups) {
            result.addAll(records.get(group));
        }
        return result.toArray(new IDataRecord[0]);
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
        String[] datasets = cache.getDatasetNames(group);
        if (datasets == null) {
            datasets = delegate.getDatasets(group);
            cache.cacheDatasetNames(group, datasets);
        }
        return datasets;
    }

    // Caching utility methods

    /**
     * Cache all datasets for a group. Both the indivdual datasets and the names
     * of the datasets are cached.
     */
    private void cacheDatasets(String group, List<IDataRecord> records,
            Request request) {
        String[] datasets = new String[records.size()];
        for (int i = 0; i < datasets.length; i += 1) {
            datasets[i] = records.get(i).getName();
            cache.cacheDataset(toDatasetGroupPath(group, datasets[i]),
                    records.get(i), request);
        }
        cache.cacheDatasetNames(group, datasets);
    }

    private static String getGroup(IDataRecord record) {
        String group = record.getGroup();
        /*
         * TODO this works around a bug in older pypies. Delete it after 14.2.1
         * is fielded everywhere.
         */
        group = group.replaceAll("::", DataStoreFactory.DEF_SEPARATOR);
        return group;
    }

    private static String toDatasetGroupPath(String group, String dataset) {
        return group + DataStoreFactory.DEF_SEPARATOR + dataset;
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

    @Override
    public void deleteOrphanData(Date oldestDate) throws StorageException {
        this.delegate.deleteOrphanData(oldestDate);
    }
}
