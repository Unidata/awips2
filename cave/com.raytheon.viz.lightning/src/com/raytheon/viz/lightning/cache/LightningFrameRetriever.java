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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 09, 2014 3333       bclement     moved from LightningResource
 * Jul 22, 2014 3214       bclement     fixed typos in populatePulseData() and updateAndGet()
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
     * Add any new records to cache and return the updated frame
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
            // Add as new records
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
                // if we've already processed some records, request the
                // new ones now and merge
                LightningFrame newBundle = retrieveObject(metadata);
                rval.merge(newBundle);
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
                Map<String, List<IDataRecord>> recordMap = createRecordMap(records);

                List<IDataRecord> times = recordMap
                        .get(LightningConstants.TIME_DATASET);
                List<IDataRecord> intensities = recordMap
                        .get(LightningConstants.INTENSITY_DATASET);
                List<IDataRecord> lats = recordMap
                        .get(LightningConstants.LAT_DATASET);
                List<IDataRecord> lons = recordMap
                        .get(LightningConstants.LON_DATASET);
                List<IDataRecord> types = recordMap
                        .get(LightningConstants.STRIKE_TYPE_DATASET);
                List<IDataRecord> pulseIndexes = recordMap
                        .get(LightningConstants.PULSE_INDEX_DATASET);

                int k = 0;
                for (IDataRecord timeRec : times) {
                    if (hasPulseData(pulseIndexes, k)) {
                        populatePulseData(frame, bundle, timeRec.getGroup(), ds);
                    }
                    LongDataRecord time = (LongDataRecord) timeRec;
                    // Now loop through the obs times and intensities and
                    // start categorizing strikes
                    int numRecords = (int) time.getSizes()[0];

                    long[] timeData = time.getLongData();

                    int[] intensityData = ((IntegerDataRecord) intensities
                            .get(k)).getIntData();
                    float[] latitudeData = ((FloatDataRecord) lats.get(k))
                            .getFloatData();
                    float[] longitudeData = ((FloatDataRecord) lons.get(k))
                            .getFloatData();
                    byte[] typeData = ((ByteDataRecord) types.get(k))
                            .getByteData();

                    for (int i = 0; i < numRecords; i++) {

                        DataTime dt = new DataTime(new Date(timeData[i]));
                        dt = frame.getOffset().getNormalizedTime(dt);
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

                        // only add the strike to the list if the data time
                        // of the strike matches the data time of the frame
                        if (dt.equals(bundle.getFrameTime())) {
                            list.add(latLon);
                            strikeCount++;
                        }

                    }
                    k++;
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
     * Unpack records into map keyed by record name
     * 
     * @param records
     * @return
     */
    public static Map<String, List<IDataRecord>> createRecordMap(
            IDataRecord[] records) {
        Map<String, List<IDataRecord>> recordMap = new HashMap<String, List<IDataRecord>>();
        for (IDataRecord rec : records) {
            List<IDataRecord> recordList = recordMap.get(rec.getName());
            if (recordList == null) {
                recordList = new ArrayList<IDataRecord>();
                recordMap.put(rec.getName(), recordList);
            }
            recordList.add(rec);
        }
        return recordMap;
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
     * @param pulseIndexes
     * @param recordIndex
     * @return true if any data record in list has a valid pulse index
     */
    private static boolean hasPulseData(List<IDataRecord> pulseIndexes,
            int recordIndex) {
        if (pulseIndexes != null) {
            IDataRecord record = pulseIndexes.get(recordIndex);
            int[] indexData = ((IntegerDataRecord) record).getIntData();
            for (int i = 0; i < indexData.length; ++i) {
                if (indexData[i] >= 0) {
                    return true;
                }
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
