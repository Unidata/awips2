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
package com.raytheon.uf.common.dataplugin.binlightning.dataaccess;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData.Type;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataaccess.util.PDOUtil;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Data access framework factory for bin lightning
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2014 2667       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BinLightingAccessFactory extends AbstractDataPluginFactory {
    
    private static final String sourceKey = "source";

    private static final IUFStatusHandler log = UFStatus
            .getHandler(BinLightningRecord.class);

    private static final GeometryFactory geomFactory = new GeometryFactory();

    private static final String timeKey = "obsTime";

    private static final String latKey = "latitude";

    private static final String lonKey = "longitude";

    private static final String[] requiredKeys = { timeKey, latKey, lonKey };

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableLocationNames
     * (com.raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        throw new IncompatibleRequestException(this.getClass()
                + " does not support location names");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory#
     * getRequiredIdentifiers()
     */
    @Override
    public String[] getRequiredIdentifiers() {
        return new String[] { sourceKey };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory#
     * getValidIdentifiers()
     */
    @Override
    public String[] getValidIdentifiers() {
        return new String[] { sourceKey };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataPluginFactory#
     * buildConstraintsFromRequest
     * (com.raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();

        Map<String, Object> identifiers = request.getIdentifiers();
        if (identifiers != null) {
            for (Entry<String, Object> entry : identifiers.entrySet()) {
                rcMap.put(entry.getKey(), new RequestConstraint(entry
                        .getValue().toString()));
            }
        }
        return rcMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataPluginFactory#
     * getGeometryData(com.raytheon.uf.common.dataaccess.IDataRequest,
     * com.raytheon.uf.common.dataquery.responses.DbQueryResponse)
     */
    @Override
    protected IGeometryData[] getGeometryData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {
        Map<File, List<BinLightningRecord>> results = unpackResults(dbQueryResponse);

        List<IGeometryData> rval = new ArrayList<IGeometryData>();
        for (Entry<File, List<BinLightningRecord>> resultEntry : results
                .entrySet()) {
            Map<String, List<String>> srcDatasets = getSourceDatasets(request,
                    resultEntry.getValue());
            IDataStore ds = DataStoreFactory.getDataStore(resultEntry.getKey());
            for (Entry<String, List<String>> groupEntry : srcDatasets
                    .entrySet()) {
                addGeometryData(rval, ds, groupEntry.getKey(),
                        groupEntry.getValue());
            }
        }
        return rval.toArray(new IGeometryData[rval.size()]);
    }


    /**
     * Add geometry data elements to dataList from data store
     * 
     * @param dataList
     *            target result list
     * @param ds
     *            datastore
     * @param source
     *            lightning source value from metadata
     * @param datasets
     *            requested datasets from datastore
     */
    private void addGeometryData(List<IGeometryData> dataList, IDataStore ds,
            String source, List<String> datasets) {
        // Go fetch data
        try {
            IDataRecord[] records = ds.retrieveDatasets(
                    datasets.toArray(new String[datasets.size()]), Request.ALL);

            Map<String, List<IDataRecord>> recordMap = new HashMap<String, List<IDataRecord>>();
            // Throw in a map for easy accessibility
            for (IDataRecord rec : records) {
                List<IDataRecord> recordList = recordMap.get(rec.getName());
                if (recordList == null) {
                    recordList = new ArrayList<IDataRecord>();
                    recordMap.put(rec.getName(), recordList);
                }
                recordList.add(rec);
            }

            // remove required records from map so they won't be used again when
            // we look for optional records
            List<IDataRecord> times = recordMap.remove(timeKey);
            List<IDataRecord> lats = recordMap.remove(latKey);
            List<IDataRecord> lons = recordMap.remove(lonKey);

            int k = 0;
            for (IDataRecord timeRec : times) {
                LongDataRecord time = (LongDataRecord) timeRec;

                long[] timeData = time.getLongData();
                float[] latitudeData = ((FloatDataRecord) lats.get(k))
                        .getFloatData();
                float[] longitudeData = ((FloatDataRecord) lons.get(k))
                        .getFloatData();

                for (int i = 0; i < timeData.length; i++) {
                    DataTime dt = new DataTime(new Date(timeData[i]));
                    DefaultGeometryData data = new DefaultGeometryData();
                    data.setDataTime(dt);
                    data.addAttribute(sourceKey, source);
                    data.setGeometry(geomFactory.createPoint(new Coordinate(
                            longitudeData[i], latitudeData[i])));
                    // add the optional parameter records
                    addParameterData(data, recordMap, k, i);
                    dataList.add(data);
                }
                k++;
            }
        } catch (StorageException e) {
            log.error("Storage error retrieving lightning data", e);
        } catch (FileNotFoundException e) {
            log.error("Unable to open lightning file", e);
        }
    }

    /**
     * Add parameters from record map to data
     * 
     * @param data
     *            target geometry data
     * @param recordMap
     *            map of parameter names to list of data records
     * @param recordIndex
     *            index into list of data records
     * @param valueIndex
     *            index into the target data record's value array
     */
    private void addParameterData(DefaultGeometryData data,
            Map<String, List<IDataRecord>> recordMap, int recordIndex,
            int valueIndex) {
        for (Entry<String, List<IDataRecord>> entry : recordMap.entrySet()) {
            String parameterName = entry.getKey();
            IDataRecord record = entry.getValue().get(recordIndex);
            if (record instanceof IntegerDataRecord) {
                int value = ((IntegerDataRecord) record).getIntData()[valueIndex];
                data.addData(parameterName, value, Type.INT);
            } else if (record instanceof ByteDataRecord) {
                int value = ((ByteDataRecord) record).getByteData()[valueIndex];
                data.addData(parameterName, value, Type.INT);
            } else {
                // lightning only uses ints and bytes, we can add support for
                // more types if needed
                log.warn("Unsupported parameter record type for lightning: "
                        + record.getClass());
            }
        }
    }

    /**
     * Return mapping of lightning data source to list of datasets
     * 
     * @param recList
     * @return
     */
    private Map<String, List<String>> getSourceDatasets(IDataRequest request,
            List<BinLightningRecord> recList) {
        List<String> includedDatasets = getIncludedDatasets(request);
        
        Map<String, List<String>> rval = new HashMap<String, List<String>>();
        for (BinLightningRecord record : recList) {
            String src = record.getSource();
            List<String> groups = rval.get(src);
            if (groups == null) {
                groups = new ArrayList<String>();
                rval.put(src, groups);
            }
            for (String dataset : includedDatasets) {
                groups.add(record.getDataURI() + DataStoreFactory.DEF_SEPARATOR
                        + dataset);
            }
        }
        return rval;
    }

    /**
     * Get a list of HDF5 datasets to request
     * 
     * @param request
     * @return
     */
    private List<String> getIncludedDatasets(IDataRequest request){
        String[] parameters = request.getParameters();
        List<String> rval = new ArrayList<String>(parameters.length
                + requiredKeys.length);
        rval.addAll(Arrays.asList(requiredKeys));
        rval.addAll(Arrays.asList(parameters));
        return rval;
    }

    /**
     * Unpack records from response and group by HDF5 file
     * 
     * @param dbQueryResponse
     * @return
     */
    private Map<File, List<BinLightningRecord>> unpackResults(
            DbQueryResponse dbQueryResponse) {
        // Bin up requests to the same hdf5
        Map<File, List<BinLightningRecord>> fileMap = new HashMap<File, List<BinLightningRecord>>();

        for (Map<String, Object> result : dbQueryResponse.getResults()) {
            Object object = result.get(null);
            if (object == null || !(object instanceof BinLightningRecord)) {
                log.warn("Unexpected result for bin lightning: " + object);
                continue;
            }
            BinLightningRecord record = (BinLightningRecord) object;
            File hdf5File = PDOUtil.getHDF5File(record);
            List<BinLightningRecord> recList = fileMap.get(hdf5File);
            if (recList == null) {
                recList = new ArrayList<BinLightningRecord>();
                fileMap.put(hdf5File, recList);
            }
            recList.add(record);
        }
        return fileMap;
    }

}
