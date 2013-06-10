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
package com.raytheon.uf.edex.pointdata.accumulate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.accumulate.AccumDataRequestMessage;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.pointdata.DbParameterDescription;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2010            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class AccumDataHandler implements
        IRequestHandler<AccumDataRequestMessage> {

    private static final String DATATIME = "dataTime.validPeriod.start";

    private static final long MINUTE = 60000;

    private static final long BUFFER = 15 * MINUTE;

    private static final long DELTA = 30 * MINUTE;

    private static final int MISSING = -9999;

    private static final String NO_DATA = "No Data Available";

    private static final Unit<?> MS = SI.MILLI(SI.SECOND);

    @Override
    public IDataRecord handleRequest(AccumDataRequestMessage request)
            throws Exception {
        if (request == null) {
            throw new IllegalArgumentException("Null request");
        }

        Map<String, List<PointDataView>> stationMap = buildStationMap(request);

        return getData(request, stationMap);
    }

    private PointDataPluginDao<?> getDao(AccumDataRequestMessage request)
            throws PluginException {
        String pluginName = request.getPluginName();
        if (pluginName == null) {
            throw new IllegalArgumentException("Plugin name null");
        }
        PluginDao pd = PluginFactory.getInstance().getPluginDao(pluginName);
        if (!(pd instanceof PointDataPluginDao<?>)) {
            throw new PluginException(pluginName
                    + " DAO is not a point data DAO");
        }
        return (PointDataPluginDao<?>) pd;
    }

    private Map<String, List<PointDataView>> buildStationMap(
            AccumDataRequestMessage request) throws Exception {

        long minValidTime = Long.MAX_VALUE;
        long maxValidTime = Long.MIN_VALUE;
        for (long time : request.getTimes()) {
            if (time < minValidTime) {
                minValidTime = time;
            }
            if (time > maxValidTime) {
                maxValidTime = time;
            }
        }

        int incMinutes = request.getIncMinutes();
        int totalMinutes = request.getTotalMinutes();

        Map<String, List<PointDataView>> stationMap = new HashMap<String, List<PointDataView>>();

        if (incMinutes == totalMinutes) {
            // Special case, grab data only from the past
            query(stationMap, request, minValidTime - incMinutes * MINUTE
                    - BUFFER, maxValidTime - incMinutes * MINUTE + BUFFER);
        } else if (incMinutes > 120) {
            long curMaxTime = maxValidTime;
            long curMinTime = minValidTime;
            for (int i = 0; i < totalMinutes / incMinutes; i++) {
                query(stationMap, request, minValidTime - BUFFER, maxValidTime
                        + BUFFER);
                curMaxTime -= incMinutes * MINUTE;
                curMinTime -= incMinutes * MINUTE;
            }
        } else {
            query(stationMap, request, minValidTime - totalMinutes * MINUTE
                    - BUFFER, maxValidTime + BUFFER);
        }
        return stationMap;
    }

    private void query(Map<String, List<PointDataView>> stationMap,
            AccumDataRequestMessage request, long startTime, long endTime)
            throws Exception {

        String pluginName = request.getPluginName();

        PointDataQuery query = new PointDataQuery(pluginName);
        query.requestAllLevels();
        StringBuilder requestParams = new StringBuilder();
        requestParams.append(request.getParameter());
        requestParams.append(",");
        requestParams.append(request.getTimeParameter());
        requestParams.append(",");
        requestParams.append(request.getStationParameter());
        
        query.setParameters(requestParams.toString());

        query.addParameter(DATATIME,
                new DataTime(new Date(startTime)).toString() + "--"
                        + new DataTime(new Date(endTime)).toString(), "between");

        // Attempt to filter off of stationName if stationName is in the
        // database
        PointDataDbDescription dbDesc = getDao(request)
                .getPointDataDbDescription();
        if (dbDesc != null) {
            for (DbParameterDescription desc : dbDesc.parameters) {
                String[] idParameters = request.getStationParameter()
                        .split(",");
                for (int i = 0; i < idParameters.length; i++) {
                    String idParameter = idParameters[i];
                    if (desc.getParameterName().equals(idParameter)) {
                        StringBuilder stationList = null;
                        for (String station : request.getStations()) {
                            station = station.split(",")[i];
                            if (stationList == null) {
                                stationList = new StringBuilder();
                            } else {
                                stationList.append(",");
                            }
                            stationList.append(station);
                        }
                        query.addParameter(desc.getQueryName(),
                                stationList.toString(), "in");
                    }
                }
            }
        }

        PointDataContainer pdc = query.execute();
        if (pdc == null) {
            return;
        }
        pdc.setCurrentSz(pdc.getAllocatedSz());
        for (int pCounter = 0; pCounter < pdc.getAllocatedSz(); pCounter++) {
            PointDataView pdv = pdc.readRandom(pCounter);
            StringBuilder station = new StringBuilder();
            for (String idParameter : request.getStationParameter().split(",")) {
                if (station.length() > 0) {
                    station.append(",");
                }
                Type type = pdv.getType(idParameter);
                if (type == Type.STRING) {
                    station.append(pdv.getString(idParameter));
                } else {
                    station.append(pdv.getNumber(idParameter));
                }
            }
            List<PointDataView> pdvList = stationMap.get(station.toString());
            if (pdvList == null) {
                pdvList = new ArrayList<PointDataView>();
                stationMap.put(station.toString(), pdvList);
            }
            pdvList.add(pdv);
        }
    }

    private IDataRecord getData(AccumDataRequestMessage request,
            Map<String, List<PointDataView>> stationMap) {
        // Get data out of the request
        String[] stations = request.getStations();
        long[] times = request.getTimes();
        int totalMinutes = request.getTotalMinutes();
        int incMinutes = request.getIncMinutes();
        String parameter = request.getParameter();
        // Set things up
        Calendar cal = Calendar.getInstance();
        int size = totalMinutes / incMinutes;
        int rows = stations.length;

        // a couple of strange flags, there really should be a standardized way
        // of defining these, this algorithm is just guessing
        boolean shiftToHour = false;
        boolean topOfHour = false;
        if (incMinutes % 60 == 0 && totalMinutes % 60 == 0) {
            shiftToHour = true;
            topOfHour = incMinutes != totalMinutes;
        }

        Type type = Type.FLOAT;
        int dimension = 1;
        for (List<PointDataView> pdvList : stationMap.values()) {
            if (pdvList != null && !pdvList.isEmpty()) {
                type = pdvList.get(0).getType(parameter);
                ParameterDescription description = pdvList.get(0)
                        .getContainer().getDescription(parameter);
                dimension = description.getDimensionAsInt();
                if (dimension <= 0) {
                    dimension = 1;
                }
                break;
            }
        }

        Object[] data = new Object[size * rows * dimension];

        int index = 0;
        for (int i = 0; i < stations.length; i++) {
            String station = stations[i];
            List<PointDataView> pdvList = stationMap.get(station);
            if (pdvList != null && pdvList.size() > 0) {
                long targetTime = times[i];
                if (incMinutes == totalMinutes) {
                    targetTime -= incMinutes * MINUTE;
                }
                cal.setTimeInMillis(targetTime);
                int hour = cal.get(Calendar.HOUR_OF_DAY);
                int min = cal.get(Calendar.MINUTE);
                if (shiftToHour) {
                    if (min > 30 && min < 60) {
                        cal.set(Calendar.HOUR_OF_DAY, hour + 1);
                    }
                    cal.set(Calendar.MINUTE, 0);
                }
                long baseTime = cal.getTimeInMillis();

                for (int counter = 0; counter < size; counter++) {
                    Object value = null;
                    if (dimension == 1) {
                        value = getNoData(type);
                    } else {
                        value = new Object[dimension];
                        Arrays.fill((Object[]) value, getNoData(type));
                    }
                    for (int j = 0; j < pdvList.size(); j++) {
                        PointDataView pdv = pdvList.get(j);
                        long time = pdv.getLong(request.getTimeParameter());
                        Unit<?> timeUnit = pdv.getUnit(request
                                .getTimeParameter());
                        if (MS.isCompatible(timeUnit)) {
                            time = (long) timeUnit.getConverterTo(MS).convert(
                                    time);

                        }
                        if (time == targetTime
                                || (topOfHour && baseTime - time >= 0 && time > (baseTime
                                        - DELTA + 1))) {
                            value = pullData(parameter, pdv);
                        }

                    }
                    if (dimension == 1) {
                        data[index++] = value;
                    } else {
                        for (int j = 0; j < dimension; j++) {
                            data[index++] = ((Object[]) value)[j];
                        }
                    }
                    baseTime = baseTime - (incMinutes * MINUTE);
                    targetTime = targetTime - (incMinutes * MINUTE);
                }

            } else {
                // just fill with missing data values
                for (int j = 0; j < size; j++) {
                    for (int k = 0; k < dimension; k++) {
                        data[index] = getNoData(type);
                        index++;
                    }
                }
            }
        }

        return createDataRecord(parameter, type, data, rows, size, dimension);

    }

    private IDataRecord createDataRecord(String parameter, Type type,
            Object[] data, int rows, int size, int dimension) {
        long[] sizes = null;
        if (dimension == 1 && size == 1) {
            sizes = new long[] { rows };
        } else if (size == 1) {
            sizes = new long[] { dimension, rows };
        } else if (dimension == 1) {
            sizes = new long[] { size, rows };
        } else {
            // TODO test this
            sizes = new long[] { size, dimension, rows };
        }
        switch (type) {
        case STRING:
            String[] stringData = new String[data.length];
            for (int i = 0; i < data.length; i++) {
                stringData[i] = data[i].toString();
            }
            return new StringDataRecord(parameter, "", stringData,
                    sizes.length, sizes);
        case INT:
            int[] intData = new int[data.length];
            for (int i = 0; i < data.length; i++) {
                intData[i] = ((Number) data[i]).intValue();
            }
            return new IntegerDataRecord(parameter, "", intData, sizes.length,
                    sizes);
        case FLOAT:
            float[] floatData = new float[data.length];
            for (int i = 0; i < data.length; i++) {
                floatData[i] = ((Number) data[i]).floatValue();
            }
            return new FloatDataRecord(parameter, "", floatData, sizes.length,
                    sizes);
        default:
            return null;
        }
    }

    private Object getNoData(Type type) {
        switch (type) {
        case STRING:
            return NO_DATA;
        case INT:
        case FLOAT:
            return MISSING;
        default:
            return null;
        }
    }

    private Object pullData(String parameter, PointDataView pdv) {
        Type type = pdv.getType(parameter);
        if (pdv.getDimensions(parameter) == 2) {
            switch (type) {
            case STRING:
                return pdv.getStringAllLevels(parameter);
            case INT:
            case FLOAT:
                return pdv.getNumberAllLevels(parameter);
            default:
                return null;
            }
        } else {
            switch (type) {
            case STRING:
                return pdv.getString(parameter);
            case INT:
            case FLOAT:
                return pdv.getNumber(parameter);
            default:
                return null;
            }
        }
    }
}
