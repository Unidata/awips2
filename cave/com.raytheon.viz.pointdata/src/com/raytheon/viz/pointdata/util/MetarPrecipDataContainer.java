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
package com.raytheon.viz.pointdata.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2011            bsteffen     Initial creation
 * Jan 10, 2013            snaples      updated getBasePrecipData to use correct data for 1 hour precip.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@SuppressWarnings("unchecked")
public class MetarPrecipDataContainer {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetarPrecipDataContainer.class);

    public static class PrecipData {

        private final long timeObs;

        private final String stationName;

        private final Double precipAmt;

        private final Coordinate latLon;

        public PrecipData(long timeObs, String stationName, Double precipAmt,
                Double longitude, Double latitude) {
            super();
            this.timeObs = timeObs;
            this.stationName = stationName;
            this.precipAmt = precipAmt;
            this.latLon = new Coordinate(longitude, latitude);
        }

        public Double getPrecipAmt() {
            return precipAmt;
        }

        public Coordinate getLatLon() {
            return latLon;
        }

        public String getStationName() {
            return stationName;
        }

        public long getTimeObs() {
            return timeObs;
        }

    }

    private static final String LAT_KEY = "latitude";

    private static final String LON_KEY = "longitude";

    private static final String NAME_KEY = "stationName";

    private static final String TIME_KEY = "timeObs";

    private static final String P1_KEY = "precip1Hour";

    private static final String P3_KEY = "precip3Hour";

    private static final String P6_KEY = "precip6Hour";

    private static final String P24_KEY = "precip24Hour";

    private static final long FIFTEEN_MIN = 15 * 60 * 1000l;

    private static final long ONE_HOUR = 60 * 60 * 1000l;

    private static final long THREE_HOUR = 3 * ONE_HOUR;

    private static final long SIX_HOUR = 6 * ONE_HOUR;

    private final int duration;

    private Map<String, RequestConstraint> rcMap = null;

    private final Map<Long, Map<String, PrecipData>> cache3 = new HashMap<Long, Map<String, PrecipData>>();

    private final Map<Long, Map<String, PrecipData>> cache6 = new HashMap<Long, Map<String, PrecipData>>();

    private final Map<DataTime, Set<String>> baseStations = new HashMap<DataTime, Set<String>>();

    public MetarPrecipDataContainer(int duration,
            Map<String, RequestConstraint> rcMap) {
        this.duration = duration;
        this.rcMap = rcMap;
    }

    public List<PrecipData> getBasePrecipData(DataTime time) {
        Map<String, PrecipData> precipMap = new HashMap<String, PrecipData>();
        long validTime = time.getMatchValid();
        if (duration == 1) {
            PointDataContainer pdc = requestPointData(rcMap, validTime, 1,
                    P1_KEY);
            Map<String, PrecipData> precipMap1 = null;
            if (pdc != null) {
                precipMap1 = createPrecipData(pdc,
                        validTime - ONE_HOUR, validTime, P1_KEY);
                if (precipMap1 == null) {
                precipMap1 = createPrecipData(pdc,
                        validTime - ONE_HOUR + FIFTEEN_MIN, validTime
                                - FIFTEEN_MIN, P1_KEY);
                }
                // Data frame 15 minutes ago is better then data now for some
                // reason
                precipMap = precipMap1;

            }
        } else if (duration == 3) {
            precipMap = getRawPrecipData3(validTime);
        } else if (duration == 6) {
            precipMap = getRawPrecipData6(validTime);
        } else if (duration == 24) {
            PointDataContainer pdc = requestPointData(rcMap, validTime, 1,
                    P24_KEY);
            if (pdc != null) {
                precipMap = createPrecipData(pdc, validTime - ONE_HOUR,
                        validTime, P24_KEY);
            }
        }
        if (precipMap == null) {
            precipMap = new HashMap<String, PrecipData>();
        }
        baseStations.put(time, precipMap.keySet());
        ArrayList<PrecipData> result = new ArrayList<PrecipData>(
                precipMap.values());
        return result;
    }

    public List<PrecipData> getDerivedPrecipData(DataTime time) {
        Map<String, PrecipData> precipMap = new HashMap<String, PrecipData>();
        long validTime = time.getMatchValid();
        if (duration == 3) {
            Map<String, PrecipData> precipMap6minus3 = null;
            Map<String, PrecipData> precipMap1time3 = null;
            Map<String, PrecipData> precipMap6 = getRawPrecipData6(validTime);
            if (precipMap6 != null) {
                Map<String, PrecipData> precipMap3Old = getRawPrecipData3(validTime
                        - THREE_HOUR);
                precipMap6minus3 = subtract(precipMap6, precipMap3Old);

            }
            precipMap1time3 = getRawPrecipData1sum(validTime, 3);
            precipMap = combine(precipMap6minus3, precipMap1time3);
        } else if (duration == 6) {
            precipMap = getRawPrecipData1sum(validTime, 6);
        } else if (duration == 24) {
            List<Map<String, PrecipData>> maps = new ArrayList<Map<String, PrecipData>>();
            for (int i = 0; i < 4; i++) {
                long endTime = validTime - SIX_HOUR * i;
                maps.add(getRawPrecipData6(endTime));
            }
            precipMap = add(maps.toArray(new Map[0]));
        }
        if (precipMap == null) {
            return Collections.emptyList();
        }
        precipMap.keySet().removeAll(baseStations.get(time));
        ArrayList<PrecipData> result = new ArrayList<PrecipData>(
                precipMap.values());
        return result;
    }

    private Map<String, PrecipData> getRawPrecipData1sum(long validTime,
            int sumTime) {
        List<Map<String, PrecipData>> maps = new ArrayList<Map<String, PrecipData>>();
        PointDataContainer pdc = requestPointData(rcMap, validTime, sumTime,
                P1_KEY);
        if (pdc == null) {
            return null;
        }
        for (int i = 0; i < sumTime; i++) {
            long endTime = validTime - ONE_HOUR * i;
            long startTime = endTime - ONE_HOUR;
            Map<String, PrecipData> precipMap1 = createPrecipData(pdc,
                    startTime, endTime, P1_KEY);
            Map<String, PrecipData> precipMap1old = createPrecipData(pdc,
                    startTime + FIFTEEN_MIN, endTime - FIFTEEN_MIN, P1_KEY);
            maps.add(combine(precipMap1old, precipMap1));
        }
        return add(maps.toArray(new Map[0]));
    }

    private Map<String, PrecipData> getRawPrecipData3(long validTime) {
        if (cache3.containsKey(validTime)) {
            return cache3.get(validTime);
        }
        PointDataContainer pdc = requestPointData(rcMap, validTime, 1, P3_KEY,
                P6_KEY);
        if (pdc == null) {
            cache3.put(validTime, null);
            cache6.put(validTime, null);
            return null;
        }
        Map<String, PrecipData> precipMap3 = createPrecipData(pdc, validTime
                - ONE_HOUR, validTime, P3_KEY);
        Map<String, PrecipData> precipMap6 = createPrecipData(pdc, validTime
                - ONE_HOUR, validTime, P6_KEY);
        cache3.put(validTime, precipMap3);
        cache6.put(validTime, precipMap6);
        return precipMap3;
    }

    private Map<String, PrecipData> getRawPrecipData6(long validTime) {
        if (cache6.containsKey(validTime)) {
            return cache6.get(validTime);
        }
        PointDataContainer pdc = requestPointData(rcMap, validTime, 1, P6_KEY);
        if (pdc == null) {
            cache6.put(validTime, null);
            return null;
        }
        Map<String, PrecipData> precipMap6 = createPrecipData(pdc, validTime
                - ONE_HOUR, validTime, P6_KEY);
        cache6.put(validTime, precipMap6);
        return precipMap6;
    }

    private Map<String, PrecipData> createPrecipData(PointDataContainer pdc,
            long startTime, long latestTime, String precipKey) {
        Map<String, PrecipData> precipMap = new HashMap<String, PrecipData>();
        for (int i = 0; i < pdc.getCurrentSz(); i++) {
            PointDataView pdv = pdc.readRandom(i);
            Number latitude = pdv.getNumber(LAT_KEY);
            Number longitude = pdv.getNumber(LON_KEY);
            String stationName = pdv.getString(NAME_KEY);
            long timeObs = pdv.getLong(TIME_KEY);
            Number precipAmt = pdv.getNumber(precipKey);
            if (precipAmt.doubleValue() <= -0.05) {
                continue;
            }
            if (timeObs > latestTime || timeObs <= startTime) {
                continue;
            }

            PrecipData data = precipMap.get(stationName);
            if (data == null) {
                data = new PrecipData(timeObs, stationName,
                        precipAmt.doubleValue(), longitude.doubleValue(),
                        latitude.doubleValue());
                precipMap.put(stationName, data);
            } else {
                if (timeObs > data.getTimeObs()) {
                    data = new PrecipData(timeObs, stationName,
                            precipAmt.doubleValue(), longitude.doubleValue(),
                            latitude.doubleValue());
                    precipMap.put(stationName, data);
                }
            }
        }
        return precipMap;
    }

    private PointDataContainer requestPointData(
            Map<String, RequestConstraint> rcMap, long time, int duration,
            String... precipKeys) {
        List<String> parameters = new ArrayList<String>(
                Arrays.asList(precipKeys));
        parameters.add(LAT_KEY);
        parameters.add(LON_KEY);
        parameters.add(NAME_KEY);
        parameters.add(TIME_KEY);
        rcMap = new HashMap<String, RequestConstraint>(rcMap);
        Calendar validTime = Calendar.getInstance();
        validTime.setTimeInMillis(time);
        validTime.add(Calendar.MINUTE, 0);
        DataTime end = new DataTime(validTime);
        validTime = (Calendar) end.getValidTime().clone();
        validTime.add(Calendar.HOUR, -1 * duration);
        validTime.add(Calendar.MINUTE, 1);
        DataTime start = new DataTime(validTime);
        RequestConstraint timeRC = new RequestConstraint(null,
                ConstraintType.BETWEEN);
        timeRC.setBetweenValueList(new String[] { start.toString(),
                end.toString() });
        rcMap.put("dataTime", timeRC);
        PointDataContainer pdc = null;
        try {
            pdc = DataCubeContainer.getPointData("obs",
                    parameters.toArray(new String[0]), rcMap);
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error getting precip data, some precip will not display.",
                    e);
        }
        if (pdc != null) {
            pdc.setCurrentSz(pdc.getAllocatedSz());
            return pdc;
        }
        return null;
    }

    private Map<String, PrecipData> subtract(Map<String, PrecipData> map1,
            Map<String, PrecipData> map2) {
        Map<String, PrecipData> result = new HashMap<String, PrecipData>();
        for (Entry<String, PrecipData> entry : map1.entrySet()) {
            PrecipData data1 = entry.getValue();
            PrecipData data2 = map2.get(entry.getKey());
            if (data2 == null) {
                continue;
            }
            Double precipAmt = data1.getPrecipAmt() - data2.getPrecipAmt();
            PrecipData newData = new PrecipData(data1.getTimeObs(),
                    data1.getStationName(), precipAmt, data1.getLatLon().x,
                    data1.getLatLon().y);
            result.put(entry.getKey(), newData);
        }
        return result;
    }

    private Map<String, PrecipData> add(Map<String, PrecipData>... maps) {
        Map<String, PrecipData> result = new HashMap<String, PrecipData>();
        for (Map<String, PrecipData> map : maps) {
            if (map == null) {
                continue;
            }
            for (Entry<String, PrecipData> entry : map.entrySet()) {
                PrecipData data1 = entry.getValue();
                PrecipData data2 = result.get(entry.getKey());
                if (data2 == null) {
                    result.put(entry.getKey(), data1);
                    continue;
                }
                Double precipAmt = data1.getPrecipAmt() + data2.getPrecipAmt();
                PrecipData newData = new PrecipData(data1.getTimeObs(),
                        data1.getStationName(), precipAmt, data1.getLatLon().x,
                        data1.getLatLon().y);
                result.put(entry.getKey(), newData);
            }
        }
        return result;
    }

    private Map<String, PrecipData> combine(Map<String, PrecipData>... maps) {
        Map<String, PrecipData> result = new HashMap<String, PrecipData>();
        List<Map<String, PrecipData>> mapsList = new ArrayList<Map<String, PrecipData>>(
                Arrays.asList(maps));
        // Putting them all in a new map in reverse order ensures any items in
        // the first map override items in the last map.
        Collections.reverse(mapsList);
        for (Map<String, PrecipData> map : mapsList) {
            if (map == null) {
                continue;
            }
            result.putAll(map);
        }
        return result;
    }
}
