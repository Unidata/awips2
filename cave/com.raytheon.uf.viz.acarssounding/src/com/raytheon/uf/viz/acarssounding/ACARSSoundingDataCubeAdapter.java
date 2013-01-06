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
package com.raytheon.uf.viz.acarssounding;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingLayer;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.viz.pointdata.util.AbstractPointDataInventory;
import com.raytheon.viz.pointdata.util.PointDataCubeAdapter;

/**
 * 
 * Acarssounding data does not use the point data api, to get derived parameters
 * to work with acarssounding data this class will intercept point data
 * requests, and request all sounding layers from the db and reconstruct the
 * data to fit in a PointDataContainer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 26, 2012            bsteffen     Initial javadoc
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class ACARSSoundingDataCubeAdapter extends PointDataCubeAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ACARSSoundingDataCubeAdapter.class);

    protected static final String REFTIME = AcarsSoundingInventory.REFTIME;

    protected static final String FORECASTHR = AcarsSoundingInventory.FORECASTHR;

    protected static final String NUMLEVELS = AcarsSoundingInventory.NUMLEVELS;

    protected static final String STATIONID = AcarsSoundingInventory.STATIONID;

    protected static final String DATAURI = AcarsSoundingInventory.DATAURI;

    protected static final String TAILNUMBER = AcarsSoundingInventory.TAILNUMBER;

    protected static final String ALTITUDE = AcarsSoundingInventory.ALTITUDE;

    protected static final String TEMPERATURE = AcarsSoundingInventory.TEMPERATURE;

    protected static final String RELHUM = AcarsSoundingInventory.RELHUM;

    protected static final String MIXRATIO = AcarsSoundingInventory.MIXRATIO;

    protected static final String WINDDIR = AcarsSoundingInventory.WINDDIR;

    protected static final String WINDSPD = AcarsSoundingInventory.WINDSPD;

    protected static final String DEWPOINT = AcarsSoundingInventory.DEWPOINT;

    protected static final String PRESSURE = AcarsSoundingInventory.PRESSURE;

    private static final String[] LEVEL_PARAMS = { ALTITUDE, TEMPERATURE,
            RELHUM, MIXRATIO, WINDDIR, WINDSPD, DEWPOINT, PRESSURE };

    private static final Map<String, String> UNITS = new HashMap<String, String>();
    static {
        UNITS.put(REFTIME, "ms");
        UNITS.put(TEMPERATURE, "K");
    }

    private static Comparator<ACARSSoundingLayer> layerComparator = new Comparator<ACARSSoundingLayer>() {

        @Override
        public int compare(ACARSSoundingLayer arg0, ACARSSoundingLayer arg1) {
            return arg0.getFlightLevel().compareTo(arg1.getFlightLevel());
        }

    };

    @Override
    public String[] getSupportedPlugins() {
        return new String[] { "acarssounding" };
    }

    @Override
    public void initInventory() {
        if (inventory == null) {
            AbstractPointDataInventory pointInventory = new AcarsSoundingInventory();
            try {
                pointInventory.initTree(DerivedParameterGenerator
                        .getDerParLibrary());
                this.inventory = pointInventory;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    @Override
    public PointDataContainer getBaseRecords(Collection<String> baseParameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        List<String> baseParams = new ArrayList<String>(baseParameters);
        String script = ScriptCreator.createScript(queryParams, 1000, "select");
        List<Object> respList = Loader.loadData(script, 60000);
        ACARSSoundingRecord[] records = new ACARSSoundingRecord[respList.size()];
        for (int i = 0; i < records.length; i++) {
            records[i] = (ACARSSoundingRecord) respList.get(i);
        }
        Object[] vals = new Object[baseParams.size()];
        for (int i = 0; i < baseParams.size(); i++) {
            String parameter = baseParams.get(i);
            if (REFTIME.equals(parameter)) {
                vals[i] = new long[records.length];
            } else if (FORECASTHR.equals(parameter)) {
                vals[i] = new int[records.length];
            } else if (NUMLEVELS.equals(parameter)) {
                vals[i] = new int[records.length];
            } else if (STATIONID.equals(parameter)) {
                vals[i] = new String[records.length];
            } else if (DATAURI.equals(parameter)) {
                vals[i] = new String[records.length];
            } else if (TAILNUMBER.equals(parameter)) {
                vals[i] = new String[records.length];
            } else {
                vals[i] = new float[records.length][];
            }
        }
        int[] ids = new int[records.length];
        float[] latitudes = new float[records.length];
        float[] longitudes = new float[records.length];
        for (int i = 0; i < records.length; i++) {
            ACARSSoundingRecord record = records[i];
            latitudes[i] = record.getLocation().getLatitude().floatValue();
            longitudes[i] = record.getLocation().getLongitude().floatValue();
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
            if (baseParams.contains(STATIONID)) {
                int j = baseParams.indexOf(STATIONID);
                String[] data = (String[]) vals[j];
                data[i] = records[i].getLocation().getStationId();
            }
            if (baseParams.contains(DATAURI)) {
                int j = baseParams.indexOf(DATAURI);
                String[] data = (String[]) vals[j];
                data[i] = records[i].getDataURI();
            }
            // TODO currently it is necessary to remove duplicate flight levels
            // and pressure values to avoid errors in interpolation when
            // calculating values at rounded numbers(500mb) however it is not
            // good to be discarding random data
            Set<ACARSSoundingLayer> uniqueLayers = new HashSet<ACARSSoundingLayer>();
            Set<ACARSSoundingLayer> allLayers = records[i].getLevels();
            List<Integer> levels = new ArrayList<Integer>();
            List<Integer> pressures = new ArrayList<Integer>();
            for (ACARSSoundingLayer layer : allLayers) {

                if (!levels.contains(layer.getFlightLevel())
                        && (layer.getPressure() == null || !pressures
                                .contains(layer.getPressure().intValue()))) {
                    uniqueLayers.add(layer);
                    levels.add(layer.getFlightLevel());
                    if (layer.getPressure() != null) {
                        pressures.add(layer.getPressure().intValue());
                    }
                }
            }
            ACARSSoundingLayer[] layers = uniqueLayers
                    .toArray(new ACARSSoundingLayer[uniqueLayers.size()]);
            Arrays.sort(layers, layerComparator);
            if (baseParams.contains(NUMLEVELS)) {
                int j = baseParams.indexOf(NUMLEVELS);
                int[] data = (int[]) vals[j];
                data[i] = layers.length;
            }
            if (baseParams.contains(TAILNUMBER)) {
                int j = baseParams.indexOf(TAILNUMBER);
                String[] data = (String[]) vals[j];
                data[i] = layers[0].getTailNumber();
            }
            for (String levelParam : LEVEL_PARAMS) {
                if (baseParams.contains(levelParam)) {
                    int j = baseParams.indexOf(levelParam);
                    float[][] data = (float[][]) vals[j];
                    data[i] = new float[layers.length];
                }
            }
            for (int j = 0; j < layers.length; j++) {
                for (String levelParam : LEVEL_PARAMS) {
                    if (baseParams.contains(levelParam)) {
                        int k = baseParams.indexOf(levelParam);
                        float[][] data = (float[][]) vals[k];
                        Number value = null;
                        if (levelParam.equals(ALTITUDE)) {
                            value = layers[j].getFlightLevel();
                        } else if (levelParam.equals(TEMPERATURE)) {
                            value = layers[j].getTemp();
                        } else if (levelParam.equals(RELHUM)) {
                            value = layers[j].getHumidity();
                        } else if (levelParam.equals(MIXRATIO)) {
                            value = layers[j].getMixingRatio();
                        } else if (levelParam.equals(WINDDIR)) {
                            value = layers[j].getWindDirection();
                        } else if (levelParam.equals(WINDSPD)) {
                            value = layers[j].getWindSpeed();
                        } else if (levelParam.equals(DEWPOINT)) {
                            value = layers[j].getDwpt();
                        } else if (levelParam.equals(PRESSURE)) {
                            value = layers[j].getPressure();
                            if (value == null) {
                                value = Controller.ztopsa(layers[j]
                                        .getFlightLevel());
                            } else {
                                value = value.floatValue() / 100;
                            }
                        } else {
                            value = -9999;
                        }
                        if (value == null) {
                            value = -9999;
                        }
                        data[i][j] = value.floatValue();
                    }
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
        dataRecords[baseParams.size() + 1] = new FloatDataRecord("latitude",
                "", latitudes);
        dataRecords[baseParams.size() + 2] = new FloatDataRecord("longitude",
                "", longitudes);

        PointDataContainer pdc = PointDataContainer.build(dataRecords);
        for (String parameter : UNITS.keySet()) {
            if (baseParams.contains(parameter)) {
                pdc.getDescription(parameter).setUnit(UNITS.get(parameter));
            }
        }
        pdc.setCurrentSz(pdc.getAllocatedSz());
        return pdc;
    }

}
