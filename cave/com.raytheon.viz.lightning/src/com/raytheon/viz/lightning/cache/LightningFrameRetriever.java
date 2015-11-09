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
package com.raytheon.viz.lightning.cache;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetrieverAndDisposer;

/**
 * Cache object retriever for lighting frame data. Singleton to ensure that
 * cached data is shared between resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 09, 2014  3333     bclement  moved from LightningResource
 * Jul 22, 2014  3214     bclement  fixed typos in populatePulseData() and
 *                                  updateAndGet()
 * Sep 11, 2014  3608     bclement  index records by group and dataset name for
 *                                  better error handling
 * Sep 25, 2015  4605     bsteffen  repeat binning
 * Nov 05, 2015  5090     bsteffen  Use constants for datatime start/end
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class LightningFrameRetriever implements
        IObjectRetrieverAndDisposer<LightningFrameMetadata, LightningFrame> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LightningFrameRetriever.class);

    private static final LightningFrameRetriever instance = new LightningFrameRetriever();

    /**
     * @return singleton instance
     */
    public static LightningFrameRetriever getInstance() {
        return instance;
    }

    /**
     * singleton constructor
     */
    private LightningFrameRetriever() {
    }

    /**
     * Add any new records to cache and return the updated frame. This method is
     * synchronous and should only be called as part of a job.
     * 
     * @param records
     * @param co
     * @return
     */
    public LightningFrame updateAndGet(List<BinLightningRecord> records,
            CacheObject<LightningFrameMetadata, LightningFrame> co) {
        LightningFrameMetadata metadata = co.getMetadata();
        LightningFrame rval;
        synchronized (metadata) {
            if (metadata.hasRecords()) {
                /* Add as new records */
                List<BinLightningRecord> newRecords = metadata.getNewRecords();
                List<BinLightningRecord> processed = metadata.getProcessed();
                for (BinLightningRecord record : records) {
                    if (newRecords.contains(record) == false
                            && processed.contains(record) == false) {
                        newRecords.add(record);
                    }
                }
                rval = co.getObjectSync();
                if (processed.size() > 0 && newRecords.size() > 0) {
                    /*
                     * if we've already processed some records, request the new
                     * ones now and merge
                     */
                    LightningFrame newBundle = retrieveObject(metadata);
                    rval.merge(newBundle);
                }
            } else {
                /*
                 * Always request data for new frames to ensure nothing is
                 * missing.
                 */
                RequestConstraint sourceRC = new RequestConstraint(
                        metadata.getSource());
                TimeRange range = metadata.getOffset().getTimeRange(
                        metadata.getFrameTime());
                RequestConstraint startRC = new RequestConstraint(
                        TimeUtil.formatToSqlTimestamp(range.getEnd()),
                        ConstraintType.LESS_THAN);
                RequestConstraint endRC = new RequestConstraint(
                        TimeUtil.formatToSqlTimestamp(range.getStart()),
                        ConstraintType.GREATER_THAN);

                DbQueryRequest request = new DbQueryRequest();
                request.setEntityClass(BinLightningRecord.class);
                request.addConstraint(LightningConstants.SOURCE, sourceRC);
                request.addConstraint(PluginDataObject.STARTTIME_ID,
                        startRC);
                request.addConstraint(PluginDataObject.ENDTIME_ID, endRC);
                try {
                    DbQueryResponse response = (DbQueryResponse) RequestRouter
                            .route(request);
                    BinLightningRecord[] newRecords = response.getEntityObjects(BinLightningRecord.class);
                    metadata.getNewRecords().addAll(Arrays.asList(newRecords));
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retriving metadata for time "
                                    + metadata.getFrameTime()
                                            .getDisplayString(), e);
                }
                rval = co.getObjectSync();
            }
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetriever#retrieveObject
     * (java.lang.Object)
     */
    @Override
    public LightningFrame retrieveObject(LightningFrameMetadata metadata) {
        synchronized (metadata) {
            LightningFrame bundle = new LightningFrame(metadata.getFrameTime(),
                    metadata);
            populateData(metadata, bundle);
            return bundle;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetriever#getSize(java
     * .lang.Object)
     */
    @Override
    public int getSize(LightningFrame object) {
        int rval = 0;
        if (object != null) {
            synchronized (object) {
                rval = object.getDataSize();
            }
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetrieverAndDisposer
     * #disposeObject(java.lang.Object)
     */
    @Override
    public void disposeObject(LightningFrame object) {
        LightningFrameMetadata metadata = object.getMetadata();
        synchronized (metadata) {
            List<BinLightningRecord> newRecords = metadata.getNewRecords();
            List<BinLightningRecord> processed = metadata.getProcessed();
            newRecords.addAll(processed);
            processed.clear();
        }
    }

    /**
     * Populate frame with data from HDF5
     * 
     * @param frame
     * @param bundle
     */
    private static void populateData(final LightningFrameMetadata frame,
            LightningFrame bundle) {
        long t0 = System.currentTimeMillis();
        long strikeCount = 0;
        long dsTime = 0;

        // Bin up requests to the same hdf5
        Map<File, List<BinLightningRecord>> fileMap = new HashMap<File, List<BinLightningRecord>>();
        List<BinLightningRecord> newRecords = frame.getNewRecords();
        List<BinLightningRecord> processed = frame.getProcessed();
        for (BinLightningRecord record : newRecords) {
            File f = HDF5Util.findHDF5Location(record);
            List<BinLightningRecord> recList = fileMap.get(f);
            if (recList == null) {
                recList = new ArrayList<BinLightningRecord>();
                fileMap.put(f, recList);
            }
            recList.add(record);
            processed.add(record);
        }
        newRecords.clear();

        for (File f : fileMap.keySet()) {
            List<BinLightningRecord> recList = fileMap.get(f);
            String[] groups = new String[recList.size()];
            for (int i = 0; i < recList.size(); i++) {
                groups[i] = recList.get(i).getDataURI();
            }

            // Go fetch data
            try {
                long tDS0 = System.currentTimeMillis();
                IDataStore ds = DataStoreFactory.getDataStore(f);
                IDataRecord[] records = ds.retrieveGroups(groups, Request.ALL);

                long tDS1 = System.currentTimeMillis();
                dsTime += (tDS1 - tDS0);
                // Throw in a map for easy accessibility
                Map<String, Map<String, IDataRecord>> groupedRecords = createRecordMap(records);

                for (Entry<String, Map<String, IDataRecord>> entry : groupedRecords
                        .entrySet()) {
                    Map<String, IDataRecord> recordMap = entry.getValue();

                    IDataRecord timeRec = recordMap
                            .get(LightningConstants.TIME_DATASET);
                    IDataRecord intensities = recordMap
                            .get(LightningConstants.INTENSITY_DATASET);
                    IDataRecord lats = recordMap
                            .get(LightningConstants.LAT_DATASET);
                    IDataRecord lons = recordMap
                            .get(LightningConstants.LON_DATASET);
                    IDataRecord types = recordMap
                            .get(LightningConstants.STRIKE_TYPE_DATASET);
                    IDataRecord pulseIndexes = recordMap
                            .get(LightningConstants.PULSE_INDEX_DATASET);

                    if (timeRec == null || intensities == null || lats == null
                            || lons == null || types == null
                            || pulseIndexes == null) {
                        List<String> missing = getMissingDatasets(recordMap);
                        statusHandler.error("Group '" + entry.getKey()
                                + "' missing dataset(s): " + missing);
                        continue;
                    }
                    if (!allSameLength(timeRec, intensities, lats, lons, types,
                            pulseIndexes)) {
                        statusHandler.error("Group '" + entry.getKey()
                                + "' has mismatched dataset lengths");
                        continue;
                    }
                    if (hasPulseData(pulseIndexes)) {
                        populatePulseData(frame, bundle, timeRec.getGroup(), ds);
                    }
                    LongDataRecord time = (LongDataRecord) timeRec;
                    // Now loop through the obs times and intensities and
                    // start categorizing strikes
                    int numRecords = (int) time.getSizes()[0];

                    long[] timeData = time.getLongData();

                    int[] intensityData = ((IntegerDataRecord) intensities)
                            .getIntData();
                    float[] latitudeData = ((FloatDataRecord) lats)
                            .getFloatData();
                    float[] longitudeData = ((FloatDataRecord) lons)
                            .getFloatData();
                    byte[] typeData = ((ByteDataRecord) types).getByteData();

                    for (int i = 0; i < numRecords; i++) {

                        DataTime dt = new DataTime(new Date(timeData[i]));
                        if (!frame.contains(dt)) {
                            /*
                             * only add the strike to the list if the data time
                             * of the strike matches the data time of the frame
                             */
                            continue;
                        }
                        List<double[]> list;
                        LtgStrikeType type = LtgStrikeType.getById(typeData[i]);
                        switch (type) {
                        case CLOUD_TO_CLOUD:
                            list = bundle.getCloudLatLonList();
                            break;
                        default:
                            if (intensityData[i] > 0) {
                                list = bundle.getPosLatLonList();
                            } else {
                                list = bundle.getNegLatLonList();
                            }
                            break;
                        }

                        double[] latLon = new double[] { longitudeData[i],
                                latitudeData[i] };

                        list.add(latLon);
                        strikeCount++;

                    }
                }
            } catch (StorageException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Storage error retrieving lightning data", e);
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to open lightning file", e);
            }

        }

        long t1 = System.currentTimeMillis();

        System.out.println("Decoded: " + strikeCount + " strikes in "
                + (t1 - t0) + "ms (hdf5 time = " + dsTime + "ms)");
    }

    /**
     * @param records
     * @return true if all the records are the same length
     */
    private static boolean allSameLength(IDataRecord... records) {
        if (records.length == 0) {
            return true;
        }
        boolean rval = true;
        long size = records[0].getSizes()[0];
        for (IDataRecord rec : records) {
            if (rec.getSizes()[0] != size) {
                rval = false;
                break;
            }
        }
        return rval;
    }

    /**
     * @param recordMap
     * @return list of lightning datasets that are not in map
     */
    private static List<String> getMissingDatasets(
            Map<String, IDataRecord> recordMap) {
        String[] datasets = { LightningConstants.TIME_DATASET,
                LightningConstants.INTENSITY_DATASET,
                LightningConstants.LAT_DATASET, LightningConstants.LON_DATASET,
                LightningConstants.STRIKE_TYPE_DATASET,
                LightningConstants.PULSE_INDEX_DATASET };
        List<String> rval = new ArrayList<>(datasets.length);
        for (String ds : datasets) {
            if (!recordMap.containsKey(ds)) {
                rval.add(ds);
            }
        }
        return rval;
    }

    /**
     * Index records first by datastore group then by dataset name
     * 
     * @param records
     * @return
     */
    public static Map<String, Map<String, IDataRecord>> createRecordMap(
            IDataRecord[] records) {
        Map<String, Map<String, IDataRecord>> rval = new HashMap<String, Map<String, IDataRecord>>();
        for (IDataRecord rec : records) {
            String group = rec.getGroup();
            String name = rec.getName();
            Map<String, IDataRecord> datasets = rval.get(group);
            if (datasets == null) {
                datasets = new HashMap<>();
                rval.put(group, datasets);
            }
            IDataRecord prevEntry = datasets.put(name, rec);
            if (prevEntry != null) {
                /* this should never happen */
                statusHandler.warn("Group '" + group
                        + "' hash multiple datasets with name " + name);
            }
        }
        return rval;
    }

    /**
     * Search records and return first found with name
     * 
     * @param records
     * @param name
     * @return null if none found
     */
    private static IDataRecord findDataRecord(IDataRecord[] records, String name) {
        IDataRecord rval = null;
        for (IDataRecord record : records) {
            if (record.getName().equals(name)) {
                rval = record;
                break;
            }
        }
        return rval;
    }

    /**
     * @param record
     * @return true if data record has a valid pulse index
     */
    private static boolean hasPulseData(IDataRecord record) {
        int[] indexData = ((IntegerDataRecord) record).getIntData();
        for (int i = 0; i < indexData.length; ++i) {
            if (indexData[i] >= 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Read pulse data from datastore and populate in frame
     * 
     * @param frame
     * @param bundle
     * @param group
     * @param ds
     */
    private static void populatePulseData(LightningFrameMetadata frame,
            LightningFrame bundle, String group, IDataStore ds) {
        try {
            IDataRecord[] records = ds.retrieve(group + DataURI.SEPARATOR
                    + LightningConstants.PULSE_HDF5_GROUP_SUFFIX);
            FloatDataRecord latRecord = (FloatDataRecord) findDataRecord(
                    records, LightningConstants.LAT_DATASET);
            FloatDataRecord lonRecord = (FloatDataRecord) findDataRecord(
                    records, LightningConstants.LON_DATASET);
            if (latRecord == null || lonRecord == null) {
                throw new StorageException(
                        "Missing pulse latitude and/or longitude data", null);
            }
            float[] lats = latRecord.getFloatData();
            float[] lons = lonRecord.getFloatData();
            if (lats.length != lons.length) {
                throw new StorageException(
                        "Mismatched pulse latitude/longitude data", latRecord);
            }
            for (int i = 0; i < lats.length; ++i) {
                bundle.getPulseLatLonList()
                        .add(new double[] { lons[i], lats[i] });
            }
        } catch (FileNotFoundException e) {
            statusHandler.error("Unable to open lightning file", e);
        } catch (StorageException e) {
            statusHandler.error("Unable to read pulse datasets for group "
                    + group, e);
        }
    }

}
