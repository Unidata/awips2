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
package com.raytheon.viz.radar.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.pointdata.util.AbstractPointDataInventory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class VwpInventory extends AbstractPointDataInventory {

    protected static final String ProductCode = "48";

    protected static final String Mnemonic = "VWP";

    protected static final String REFTIME = "refTime";

    protected static final String FORECASTHR = "forecastHr";

    protected static final String STAELEV = "staElev";

    protected static final String LEVELS = "levels";

    protected static final String NUMLEVELS = "numLevels";

    protected static final String STATIONID = "stationId";

    protected static final String DATAURI = "dataURI";

    protected static final String LATITUDE = "latitude";

    protected static final String LONGITUDE = "longitude";

    /**
     * @param plugins
     */
    public VwpInventory() {
        super(Arrays.asList("radar"));
    }

    @Override
    public String getTypeKey(String pluginName) {
        return "productCode";
    }

    public String[] getAvailableTypes(String pluginName) throws VizException {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(RadarRecord.class.getName());
        String typeKey = getTypeKey(null);
        request.addRequestField(typeKey);
        request.addConstraint(typeKey, new RequestConstraint(ProductCode));
        request.setLimit(1);
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(request);
        if (response != null && response.getResults().size() > 0) {
            return new String[] { Mnemonic };
        }
        return new String[0];
    }

    private String convertValue(MapValues value) {
        String param = value.toString().replace("VAD_", "");
        param = param.substring(0, 1) + param.substring(1).toLowerCase();
        return "vwp" + param;
    }

    @Override
    protected List<String> getBaseParams(String pluginName, String type) {
        if (type.equals(Mnemonic)) {

            List<String> params = new ArrayList<String>();
            for (MapValues value : MapValues.values()) {
                if (value.toString().startsWith("VAD_")) {
                    params.add(convertValue(value));
                }
            }
            params.add(LEVELS);
            params.add(STAELEV);
            params.add(REFTIME);
            params.add(FORECASTHR);
            params.add(NUMLEVELS);
            params.add(DATAURI);
            params.add(STATIONID);
            params.add(LATITUDE);
            params.add(LONGITUDE);
            return params;
        }
        return Collections.emptyList();
    }

    protected PointDataContainer getBaseRecords(
            Collection<String> baseParameters,
            Map<String, RequestConstraint> queryParams)
            throws DataCubeException {
        List<String> baseParams = new ArrayList<String>(baseParameters);
        baseParams.remove(LATITUDE);
        baseParams.remove(LONGITUDE);
        queryParams.put(PluginDataObject.PLUGIN_NAME_ID, new RequestConstraint(
                RadarRecord.PLUGIN_NAME));
        queryParams.put("productCode", new RequestConstraint(ProductCode));

        queryParams.remove("type");
        DbQueryResponse response;
        try {
            response = (DbQueryResponse) RequestRouter
                    .route(new DbQueryRequest(queryParams));
        } catch (Exception e1) {
            throw new DataCubeException(e1);
        }
        RadarRecord[] records = response.getEntityObjects(RadarRecord.class);
        for (RadarRecord record : records) {
            File loc = HDF5Util.findHDF5Location(record);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            try {
                RadarDataRetriever.populateRadarRecord(dataStore, record);
            } catch (FileNotFoundException e) {
                throw new DataCubeException(
                        "Error Retrieving VWP Data from Radar Record", e);
            } catch (StorageException e) {
                throw new DataCubeException(
                        "Error Retrieving VWP Data from Radar Record", e);
            }
        }
        Object[] vals = new Object[baseParams.size()];
        for (int i = 0; i < baseParams.size(); i++) {
            String parameter = baseParams.get(i);
            if (REFTIME.equals(parameter)) {
                vals[i] = new long[records.length];
            } else if (FORECASTHR.equals(parameter)) {
                vals[i] = new int[records.length];
            } else if (STAELEV.equals(parameter)) {
                vals[i] = new float[records.length];
            } else if (NUMLEVELS.equals(parameter)) {
                vals[i] = new int[records.length];
            } else if (STATIONID.equals(parameter)) {
                vals[i] = new String[records.length];
            } else if (DATAURI.equals(parameter)) {
                vals[i] = new String[records.length];
            } else {
                vals[i] = new float[records.length][];
            }
        }
        int[] ids = new int[records.length];
        float[] latitudes = new float[records.length];
        float[] longitudes = new float[records.length];
        for (int i = 0; i < records.length; i++) {
            RadarRecord record = records[i];
            latitudes[i] = record.getLocation().getLat();
            longitudes[i] = record.getLocation().getLon();
            ids[i] = record.getId();
            if (baseParams.contains(REFTIME)) {
                int j = baseParams.indexOf(REFTIME);
                long[] data = (long[]) vals[j];
                data[i] = record.getDataTime().getRefTime().getTime();
            }
            if (baseParams.contains(FORECASTHR)) {
                int j = baseParams.indexOf(FORECASTHR);
                int[] data = (int[]) vals[j];
                data[i] = record.getDataTime().getFcstTime();
            }
            if (baseParams.contains(STAELEV)) {
                int j = baseParams.indexOf(STAELEV);
                float[] data = (float[]) vals[j];
                data[i] = records[i].getSpatialObject().getElevMeter();
            }
            if (baseParams.contains(STATIONID)) {
                int j = baseParams.indexOf(STATIONID);
                String[] data = (String[]) vals[j];
                data[i] = records[i].getIcao();
            }
            if (baseParams.contains(DATAURI)) {
                int j = baseParams.indexOf(DATAURI);
                String[] data = (String[]) vals[j];
                data[i] = records[i].getDataURI();
            }
            List<String> altitudes = record.getIds(MapValues.VAD_TYPE);
            if (baseParams.contains(NUMLEVELS)) {
                int j = baseParams.indexOf(NUMLEVELS);
                int[] data = (int[]) vals[j];
                data[i] = altitudes.size();
            }
            if (altitudes.size() == 0) {
                for (MapValues value : MapValues.values()) {
                    if (baseParams.contains(convertValue(value))) {
                        int k = baseParams.indexOf(convertValue(value));
                        float[][] data = (float[][]) vals[k];
                        data[i] = new float[altitudes.size()];
                    }
                }
            }
            for (int j = 0; j < altitudes.size(); j++) {
                String altitudeStr = altitudes.get(j);
                for (MapValues value : MapValues.values()) {
                    if (baseParams.contains(convertValue(value))) {
                        int k = baseParams.indexOf(convertValue(value));
                        float[][] data = (float[][]) vals[k];
                        if (data[i] == null) {
                            data[i] = new float[altitudes.size()];
                        }
                        String strValue = record.getProductVals(
                                RadarConstants.MapValues.VAD_TYPE, altitudeStr,
                                value);
                        if (strValue.equals("NA")) {
                            data[i][j] = -9999;
                        } else {
                            data[i][j] = Float.parseFloat(strValue);
                        }
                    }
                }
                if (baseParams.contains(LEVELS)) {
                    int k = baseParams.indexOf(LEVELS);
                    float[][] data = (float[][]) vals[k];
                    if (data[i] == null) {
                        data[i] = new float[altitudes.size()];
                    }
                    data[i][j] = Float.parseFloat(altitudeStr);
                }
            }

        }
        IDataRecord[] dataRecords = new IDataRecord[baseParams.size() + 3];
        for (int i = 0; i < baseParams.size(); i++) {
            if (vals[i] instanceof float[]) {
                dataRecords[i] = new FloatDataRecord(baseParams.get(i), "",
                        (float[]) vals[i]);
            } else if (vals[i] instanceof int[]) {
                dataRecords[i] = new IntegerDataRecord(baseParams.get(i), "",
                        (int[]) vals[i]);
            } else if (vals[i] instanceof long[]) {
                dataRecords[i] = new LongDataRecord(baseParams.get(i), "",
                        (long[]) vals[i]);
            } else if (vals[i] instanceof String[]) {
                dataRecords[i] = new StringDataRecord(baseParams.get(i), "",
                        (String[]) vals[i]);
            } else if (vals[i] instanceof float[][]) {
                float[][] val = (float[][]) vals[i];
                int maxLength = 1;
                for (int j = 0; j < val.length; j++) {
                    if (val[j] != null && val[j].length > maxLength) {
                        maxLength = val[j].length;
                    }
                }
                float[] newValues = new float[maxLength * records.length];
                for (int j = 0; j < records.length; j++) {
                    for (int k = 0; k < maxLength; k++) {
                        if (val[j] != null && val[j].length > k) {
                            newValues[j * maxLength + k] = val[j][k];
                        } else {
                            newValues[j * maxLength + k] = -9999;
                        }
                    }
                }
                dataRecords[i] = new FloatDataRecord(baseParams.get(i), "",
                        newValues, 2, new long[] { maxLength, records.length });
            }
        }

        dataRecords[baseParams.size()] = new IntegerDataRecord("id", "", ids);
        dataRecords[baseParams.size() + 1] = new FloatDataRecord(LATITUDE, "",
                latitudes);
        dataRecords[baseParams.size() + 2] = new FloatDataRecord(LONGITUDE, "",
                longitudes);

        PointDataContainer pdc = PointDataContainer.build(dataRecords);
        if (baseParams.contains(REFTIME)) {
            pdc.getDescription(REFTIME).setUnit("ms");
        }
        if (baseParams.contains(STAELEV)) {
            pdc.getDescription(STAELEV).setUnit("m");
        }
        if (baseParams.contains(LEVELS)) {
            pdc.getDescription(LEVELS).setUnit("ft*100");
        }
        pdc.setCurrentSz(pdc.getAllocatedSz());
        return pdc;
    }

}
