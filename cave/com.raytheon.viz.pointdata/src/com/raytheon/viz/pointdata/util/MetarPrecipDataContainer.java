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

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;

/**
 *
 * Container for requesting and caching metar precip data. This container can be
 * reused to request data for multiple times. Typically it is used in 2 stages,
 * first getBasePrecipData is used to quickly retrieve data for all metar
 * stations which have the data directly available, second getDerivedPrecipData
 * is used to get all the data taht is not directly available but has to be
 * derived by accumulating other reports over time.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2011            bsteffen    Initial creation
 * Jan 10, 2013            snaples     updated getBasePrecipData to use correct
 *                                     data for 1 hour precip.
 * Jun 07, 2013 2070       bsteffen    Add geospatial constraints to metar
 *                                     precip requests.
 * Dec 12, 2018 6890       dgilling    Fix potential NullPointerExceptions in
 *                                     getDerivedPrecipData.
 * Jan 10, 2019 DCS 20579  MPorricelli Modified to handle ice accum
 *
 * Aug 08  2019 DR 21515   MPorricelli Retrofit to handle older user procedures
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

    /*
     * Envelope which contains the whole world in LatLon projection. Used for
     * intersections/conversion since this is the valid area for metar records.
     */
    private static final ReferencedEnvelope WORLD_LAT_LON_ENVELOPE = new ReferencedEnvelope(
            -180, 180, -90, 90, MapUtil.LATLON_PROJECTION);

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

    private static final String P1_KEY = "1Hour";

    private static final String P3_KEY = "3Hour";

    private static final String P6_KEY = "6Hour";

    private static final String P24_KEY = "24Hour";

    private static final long FIFTEEN_MIN = 15 * 60 * 1000l;

    private static final long ONE_HOUR = 60 * 60 * 1000l;

    private static final long THREE_HOUR = 3 * ONE_HOUR;

    private static final long SIX_HOUR = 6 * ONE_HOUR;

    private final int duration;

    private final String ptype;

    private final Map<String, RequestConstraint> rcMap;

    private final org.opengis.geometry.Envelope descriptorEnvelope;

    private List<Envelope> latLonEnvelopes;

    private final Map<Long, Map<String, PrecipData>> cache3 = new HashMap<>();

    private final Map<Long, Map<String, PrecipData>> cache6 = new HashMap<>();

    private final Map<DataTime, Set<String>> baseStations = new HashMap<>();

    /**
     * Consturct a container with geospatially filtering to only request data in
     * the area of descriptorEnvelope
     *
     * @param duration
     * @param ptype
     * @param rcMap
     * @param descriptorEnvelope
     */
    public MetarPrecipDataContainer(int duration, String ptype,
            Map<String, RequestConstraint> rcMap,
            org.opengis.geometry.Envelope descriptorEnvelope) {
        this.duration = duration;
        if (ptype != null) {
            this.ptype = ptype;
        } else {
            this.ptype = "precip";
        }
        this.rcMap = rcMap;
        this.descriptorEnvelope = descriptorEnvelope;
    }

    /**
     * This will construct a container with no geospatial constraints, use it
     * only for updates where the rcMap already has stationIds that are
     * geospatially filtered.
     *
     * @param duration
     * @param ptype
     * @param rcMap
     */
    public MetarPrecipDataContainer(int duration,
            String ptype, Map<String, RequestConstraint> rcMap) {
        this.duration = duration;
        if (ptype != null) {
            this.ptype = ptype;
        } else {
            this.ptype = "precip";
        }
        this.rcMap = rcMap;
        this.descriptorEnvelope = null;
        this.latLonEnvelopes = Arrays.<Envelope> asList(WORLD_LAT_LON_ENVELOPE);
    }

    /**
     * Get the base precip data from all metar stations that have precip for the
     * specified duration directly encoded.
     *
     * @param time
     * @return
     */
    public List<PrecipData> getBasePrecipData(DataTime time) {
        Map<String, PrecipData> precipMap = new HashMap<>();
        long validTime = time.getMatchValid();
        if (duration == 1) {
            PointDataContainer pdc = requestPointData(rcMap, validTime, 1,
                    ptype+P1_KEY);
            Map<String, PrecipData> precipMap1 = null;
            if (pdc != null) {
                precipMap1 = createPrecipData(pdc, validTime - ONE_HOUR,
                        validTime, ptype+P1_KEY);
                if (precipMap1 == null) {
                    precipMap1 = createPrecipData(pdc, validTime - ONE_HOUR
                            + FIFTEEN_MIN, validTime - FIFTEEN_MIN, ptype+P1_KEY);
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
                    ptype+P24_KEY);
            if (pdc != null) {
                precipMap = createPrecipData(pdc, validTime - ONE_HOUR,
                        validTime, ptype+P24_KEY);
            }
        }
        if (precipMap == null) {
            precipMap = new HashMap<>();
        }
        baseStations.put(time, precipMap.keySet());
        ArrayList<PrecipData> result = new ArrayList<>(
                precipMap.values());
        return result;
    }

    /**
     * Get the derived precip data for all stations which don't have the base
     * data. This will attempt to accumulate precipitation from old reports.
     *
     * @param time
     * @return
     */
    public List<PrecipData> getDerivedPrecipData(DataTime time) {
        Map<String, PrecipData> precipMap = new HashMap<>();
        long validTime = time.getMatchValid();
        if (duration == 3) {
            Map<String, PrecipData> precipMap6minus3 = Collections.emptyMap();
            Map<String, PrecipData> precipMap6 = getRawPrecipData6(validTime);
            if (precipMap6 != null) {
                Map<String, PrecipData> precipMap3Old = getRawPrecipData3(validTime
                        - THREE_HOUR);
                if (precipMap3Old != null) {
                    precipMap6minus3 = subtract(precipMap6, precipMap3Old);
                }
            }
            Map<String, PrecipData> precipMap1time3 = getRawPrecipData1sum(
                    validTime, 3);
            if (precipMap1time3 == null) {
                precipMap1time3 = Collections.emptyMap();
            }
            precipMap = combine(precipMap6minus3, precipMap1time3);
        } else if (duration == 6) {
            precipMap = getRawPrecipData1sum(validTime, 6);
        } else if (duration == 24) {
            List<Map<String, PrecipData>> maps = new ArrayList<>();
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
        ArrayList<PrecipData> result = new ArrayList<>(
                precipMap.values());
        return result;
    }

    /**
     * Get raw metar data by summiong up multiple 1 hour observations
     *
     * @param validTime
     *            the valid time to get data for
     * @param sumTime
     *            how many 1hour precip obs to sum up.
     * @return
     */
    private Map<String, PrecipData> getRawPrecipData1sum(long validTime,
            int sumTime) {
        List<Map<String, PrecipData>> maps = new ArrayList<>();
        PointDataContainer pdc = requestPointData(rcMap, validTime, sumTime,
                ptype+P1_KEY);
        if (pdc == null) {
            return null;
        }
        for (int i = 0; i < sumTime; i++) {
            long endTime = validTime - ONE_HOUR * i;
            long startTime = endTime - ONE_HOUR;
            Map<String, PrecipData> precipMap1 = createPrecipData(pdc,
                    startTime, endTime, ptype+P1_KEY);
            Map<String, PrecipData> precipMap1old = createPrecipData(pdc,
                    startTime + FIFTEEN_MIN, endTime - FIFTEEN_MIN, ptype+P1_KEY);
            maps.add(combine(precipMap1old, precipMap1));
        }
        return add(maps.toArray(new Map[0]));
    }

    /**
     * Get all base 3 hour precip records for the provided times
     *
     * @param validTime
     * @return
     */
    private Map<String, PrecipData> getRawPrecipData3(long validTime) {
        if (cache3.containsKey(validTime)) {
            return cache3.get(validTime);
        }
        PointDataContainer pdc = requestPointData(rcMap, validTime, 1, ptype+P3_KEY,
                ptype+P6_KEY);
        if (pdc == null) {
            cache3.put(validTime, null);
            cache6.put(validTime, null);
            return null;
        }
        Map<String, PrecipData> precipMap3 = createPrecipData(pdc, validTime
                - ONE_HOUR, validTime, ptype+P3_KEY);
        Map<String, PrecipData> precipMap6 = createPrecipData(pdc, validTime
                - ONE_HOUR, validTime, ptype+P6_KEY);
        cache3.put(validTime, precipMap3);
        cache6.put(validTime, precipMap6);
        return precipMap3;
    }

    /**
     * Get all base 6 hour precip records for the provided times
     *
     * @param validTime
     * @return
     */
    private Map<String, PrecipData> getRawPrecipData6(long validTime) {
        if (cache6.containsKey(validTime)) {
            return cache6.get(validTime);
        }
        PointDataContainer pdc = requestPointData(rcMap, validTime, 1, ptype+P6_KEY);
        if (pdc == null) {
            cache6.put(validTime, null);
            return null;
        }
        Map<String, PrecipData> precipMap6 = createPrecipData(pdc, validTime
                - ONE_HOUR, validTime, ptype+P6_KEY);
        cache6.put(validTime, precipMap6);
        return precipMap6;
    }

    /**
     * build PricipData objects for every station in a PointDataContainer
     *
     * @param pdc
     * @param startTime
     * @param latestTime
     * @param precipKey
     *            the name of the parameter with precip.
     * @return
     */
    private Map<String, PrecipData> createPrecipData(PointDataContainer pdc,
            long startTime, long latestTime, String precipKey) {
        Map<String, PrecipData> precipMap = new HashMap<>();
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

    /**
     * This function perfroms the request to edex for point data.
     *
     * @param rcMap
     * @param time
     * @param duration
     * @param precipKeys
     * @return
     */
    private PointDataContainer requestPointData(
            Map<String, RequestConstraint> rcMap, long time, int duration,
            String... precipKeys) {
        List<String> parameters = new ArrayList<>(
                Arrays.asList(precipKeys));
        parameters.add(LAT_KEY);
        parameters.add(LON_KEY);
        parameters.add(NAME_KEY);
        parameters.add(TIME_KEY);
        rcMap = new HashMap<>(rcMap);
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
        // Over the dateline there might be an envelope on either side.
        for (Envelope latLonEnvelope : getLatLonEnvelopes()) {
            PointDataContainer tmppdc = null;
            RequestConstraint lonRC = new RequestConstraint(null,
                    ConstraintType.BETWEEN);
            Double minLon = latLonEnvelope.getMinX();
            Double maxLon = latLonEnvelope.getMaxX();
            lonRC.setBetweenValueList(new String[] { minLon.toString(),
                    maxLon.toString() });
            rcMap.put("location.longitude", lonRC);
            RequestConstraint latRC = new RequestConstraint(null,
                    ConstraintType.BETWEEN);
            Double minLat = latLonEnvelope.getMinY();
            Double maxLat = latLonEnvelope.getMaxY();
            latRC.setBetweenValueList(new String[] { minLat.toString(),
                    maxLat.toString() });
            rcMap.put("location.latitude", latRC);
            try {
                tmppdc = DataCubeContainer.getPointData("obs",
                        parameters.toArray(new String[0]), rcMap);
            } catch (DataCubeException e) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Error getting precip data, some precip will not display.",
                                e);
            }
            if (tmppdc != null) {
                tmppdc.setCurrentSz(tmppdc.getAllocatedSz());
                if (pdc != null) {
                    pdc.combine(tmppdc);
                    pdc.setCurrentSz(pdc.getAllocatedSz());
                } else {
                    pdc = tmppdc;
                }
            }
        }
        return pdc;
    }

    /**
     * Get envelopes describing the latlon area that should be used to constrain
     * all queries
     *
     * @return
     */
    private List<Envelope> getLatLonEnvelopes() {
        if (latLonEnvelopes == null) {
            this.latLonEnvelopes = new ArrayList<>(2);
            try {
                Geometry intersection = EnvelopeIntersection
                        .createEnvelopeIntersection(descriptorEnvelope,
                                WORLD_LAT_LON_ENVELOPE, 0.1, 180, 180);
                if (intersection instanceof GeometryCollection) {
                    GeometryCollection gc = (GeometryCollection) intersection;
                    for (int n = 0; n < gc.getNumGeometries(); n += 1) {
                        latLonEnvelopes.add(gc.getGeometryN(n)
                                .getEnvelopeInternal());
                    }
                } else {
                    latLonEnvelopes.add(intersection.getEnvelopeInternal());
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.VERBOSE, e.getLocalizedMessage(),
                        e);
                this.latLonEnvelopes.add(WORLD_LAT_LON_ENVELOPE);
            }
        }
        return latLonEnvelopes;
    }

    /**
     * Subtract the precip amounts in map2 from those in map1 for all stations
     * that are in both maps.
     *
     * @param map1
     * @param map2
     * @return
     */
    private Map<String, PrecipData> subtract(Map<String, PrecipData> map1,
            Map<String, PrecipData> map2) {
        Map<String, PrecipData> result = new HashMap<>();
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

    /**
     * Subtract the precip amounts in several maps for all stations that are in
     * all maps.
     *
     * @param map1
     * @param map2
     * @return
     */
    private Map<String, PrecipData> add(Map<String, PrecipData>... maps) {
        Map<String, PrecipData> result = new HashMap<>();
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

    /**
     * combine all maps so there is only one entry for each station. For
     * stations in multiple maps the entry from the first map is used.
     *
     * @param maps
     * @return
     */
    private Map<String, PrecipData> combine(Map<String, PrecipData>... maps) {
        Map<String, PrecipData> result = new HashMap<>();
        List<Map<String, PrecipData>> mapsList = new ArrayList<>(
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
