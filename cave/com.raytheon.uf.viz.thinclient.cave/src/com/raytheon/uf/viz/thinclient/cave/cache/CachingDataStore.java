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
import java.util.Collection;
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
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier;

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
 * Feb 24, 2016  5389     nabowle     Refactor to #deleteOrphanData(Map<String,Date>)
 * Feb 29, 2016  5420     tgurney     Remove timestampCheck arg from copy()
 * Sep 23, 2021  8608     mapeters    Add metadata id handling
 *
 * </pre>
 *
 * @author mschenke
 */
public class CachingDataStore implements IDataStore {

    private final IDataStore delegate;

    private DataStoreCache cache;

    CachingDataStore(IDataStore delegate, DataStoreCache cache) {
        this.delegate = delegate;
        this.cache = cache;
    }

    @Override
    public IDataRecord[] retrieve(String group)
            throws StorageException, FileNotFoundException {
        String[] datasets = cache.getDatasetNames(group);
        if (datasets != null) {
            List<String> datasetGroupPaths = new ArrayList<>(datasets.length);
            List<IDataRecord> records = new ArrayList<>();
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

    @Override
    public IDataRecord[] retrieveDatasets(String[] datasetGroupPaths,
            Request request) throws StorageException, FileNotFoundException {
        Map<String, IDataRecord> records = new HashMap<>();
        List<String> toRequest = new ArrayList<>(datasetGroupPaths.length);
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

    @Override
    public IDataRecord[] retrieveGroups(String[] groups, Request request)
            throws StorageException, FileNotFoundException {
        List<String> toRequest = new ArrayList<>();
        Map<String, List<IDataRecord>> records = new HashMap<>();
        for (String group : groups) {
            String[] datasets = cache.getDatasetNames(group);
            if (datasets != null) {
                IDataRecord[] cachedRecords = new IDataRecord[datasets.length];
                for (int i = 0; i < datasets.length; i += 1) {
                    cachedRecords[i] = cache.getDataset(
                            toDatasetGroupPath(group, datasets[i]), request);
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
            IDataRecord[] newRecords = delegate
                    .retrieveGroups(toRequest.toArray(new String[0]), request);
            for (IDataRecord record : newRecords) {
                String group = getGroup(record);
                List<IDataRecord> groupRecs = records.get(group);
                if (groupRecs == null) {
                    groupRecs = new ArrayList<>();
                    records.put(group, groupRecs);
                }
                groupRecs.add(record);
            }
            for (String group : toRequest) {
                cacheDatasets(group, records.get(group), request);
            }
        }
        List<IDataRecord> result = new ArrayList<>();
        for (String group : groups) {
            result.addAll(records.get(group));
        }
        return result.toArray(new IDataRecord[0]);
    }

    @Override
    public String[] getDatasets(String group)
            throws StorageException, FileNotFoundException {
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

    @Override
    public void deleteFiles(String[] datesToDelete)
            throws StorageException, FileNotFoundException {
        delegate.deleteFiles(datesToDelete);
    }

    @Override
    public void createDataset(IDataRecord rec)
            throws StorageException, FileNotFoundException {
        delegate.createDataset(rec);
    }

    @Override
    public void addDataRecord(IDataRecord dataset,
            IMetadataIdentifier metadataIdentifier,
            StorageProperties properties) throws StorageException {
        delegate.addDataRecord(dataset, metadataIdentifier, properties);
    }

    @Override
    public void addDataRecord(IDataRecord dataset,
            IMetadataIdentifier metadataIdentifier) throws StorageException {
        delegate.addDataRecord(dataset, metadataIdentifier);
    }

    @Override
    public void addDataRecord(IDataRecord dataset,
            Collection<IMetadataIdentifier> metadataIdentifiers)
            throws StorageException {
        this.delegate.addDataRecord(dataset, metadataIdentifiers);
    }

    @Override
    public void addDataRecord(IDataRecord dataset,
            Collection<IMetadataIdentifier> metadataIdentifiers,
            StorageProperties properties) throws StorageException {
        this.delegate.addDataRecord(dataset, metadataIdentifiers, properties);
    }

    @Override
    public StorageStatus store() throws StorageException {
        return delegate.store();
    }

    @Override
    public void deleteDatasets(String... datasets)
            throws StorageException, FileNotFoundException {
        delegate.deleteDatasets(datasets);
    }

    @Override
    public void deleteGroups(String... groups)
            throws StorageException, FileNotFoundException {
        delegate.deleteGroups(groups);
    }

    @Override
    public StorageStatus store(StoreOp storeOp) throws StorageException {
        return delegate.store(storeOp);
    }

    @Override
    public void createLinks(Map<String, LinkLocation> links)
            throws StorageException, FileNotFoundException {
        delegate.createLinks(links);
    }

    @Override
    public void repack(Compression compression) throws StorageException {
        delegate.repack(compression);
    }

    @Override
    public void copy(String outputDir, Compression compression,
            int minMillisSinceLastChange, int maxMillisSinceLastChange)
            throws StorageException {
        delegate.copy(outputDir, compression, minMillisSinceLastChange,
                maxMillisSinceLastChange);
    }

    @Override
    public void deleteOrphanData(Map<String, Date> oldestDateMap)
            throws StorageException {
        this.delegate.deleteOrphanData(oldestDateMap);
    }
}
