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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;

/**
 * 
 * Container for requesting and caching metar temp data. This container can be
 * reused to request data for multiple times. getBaseTempData is used to quickly
 * retrieve data for all metar stations which have the data directly available.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 05, 2016            mjames      Copied from MetarPrecipDataContainer
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@SuppressWarnings("unchecked")
public class MetarTempDataContainer {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetarTempDataContainer.class);

    /*
     * Envelope which contains the whole world in LatLon projection. Used for
     * intersections/conversion since this is the valid area for metar records.
     */
    private static final ReferencedEnvelope WORLD_LAT_LON_ENVELOPE = new ReferencedEnvelope(
            -180, 180, -90, 90, MapUtil.LATLON_PROJECTION);

    public static class TempData {

        private final long timeObs;

        private final String stationName;

        private final Double tempValue;

        private final Coordinate latLon;

        public TempData(long timeObs, String stationName, Double tempValue,
                Double longitude, Double latitude) {
            super();
            this.timeObs = timeObs;
            this.stationName = stationName;
            this.tempValue = getFahrenheitTemp(tempValue);
            this.latLon = new Coordinate(longitude, latitude);
        }

        public Double getTempValue() {
            return tempValue;
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

    private static final String TEMP_KEY = "temperature";
            
    private static final long TEN_MIN = 10 * 60 * 1000l;

    private final Map<String, RequestConstraint> rcMap;

    private final org.opengis.geometry.Envelope descriptorEnvelope;

    private List<Envelope> latLonEnvelopes;

    private final Map<DataTime, Set<String>> baseStations = new HashMap<DataTime, Set<String>>();

    /**
     * Consturct a container with geospatially filtering to only request data in
     * the area of descriptorEnvelope
     * 
     * @param rcMap
     * @param descriptorEnvelope
     */
    public MetarTempDataContainer(HashMap<String, RequestConstraint> rcMap,
            org.opengis.geometry.Envelope descriptorEnvelope) {
        this.rcMap = rcMap;
        this.descriptorEnvelope = descriptorEnvelope;
    }

    /**
     * This will construct a container with no geospatial constraints, use it
     * only for updates where the rcMap already has stationIds that are
     * geospatially filtered.
     * 
     * @param rcMap
     */
    public MetarTempDataContainer(Map<String, RequestConstraint> rcMap) {
        this.rcMap = rcMap;
        this.descriptorEnvelope = null;
        this.latLonEnvelopes = Arrays.<Envelope> asList(WORLD_LAT_LON_ENVELOPE);
    }

    /**
     * Get the base temp data from all metar stations
     * 
     * @param time
     * @return
     */
    public List<TempData> getBaseTempData(DataTime time) {
        Map<String, TempData> tempMap = new HashMap<String, TempData>();
        long validTime = time.getMatchValid();
        
        PointDataContainer pdc = requestPointData(rcMap, validTime, TEMP_KEY);
        if (pdc != null) {
            tempMap = createTempData(pdc, validTime - TEN_MIN,
                    validTime + TEN_MIN, TEMP_KEY);
        }
        
        if (tempMap == null) {
            tempMap = new HashMap<String, TempData>();
        }
        baseStations.put(time, tempMap.keySet());
        ArrayList<TempData> result = new ArrayList<TempData>(
                tempMap.values());
        return result;
    }

    /**
     * build TempData objects for every station in a PointDataContainer
     * 
     * @param pdc
     * @param startTime
     * @param latestTime
     * @param tempKey
     * @return
     */
    private Map<String, TempData> createTempData(PointDataContainer pdc,
            long startTime, long latestTime, String tempKey) {
        Map<String, TempData> tempMap = new HashMap<String, TempData>();
        for (int i = 0; i < pdc.getCurrentSz(); i++) {
            PointDataView pdv = pdc.readRandom(i);
            Number latitude = pdv.getNumber(LAT_KEY);
            Number longitude = pdv.getNumber(LON_KEY);
            String stationName = pdv.getString(NAME_KEY);
            long timeObs = pdv.getLong(TIME_KEY);
            Number tempValue = pdv.getNumber(tempKey);
            if (timeObs > latestTime || timeObs <= startTime) {
                continue;
            }

            TempData data = tempMap.get(stationName);
            if (data == null) {
                data = new TempData(timeObs, stationName,
                        tempValue.doubleValue(), longitude.doubleValue(),
                        latitude.doubleValue());
                if (tempValue.doubleValue() > -100.){
                    tempMap.put(stationName, data);
                }
            } else {
                if (timeObs > data.getTimeObs()) {
                    data = new TempData(timeObs, stationName,
                            tempValue.doubleValue(), longitude.doubleValue(),
                            latitude.doubleValue());
                    tempMap.put(stationName, data);
                }
            }
        }
        return tempMap;
    }

    /**
     * This function perfroms the request to edex for point data.
     * 
     * @param rcMap
     * @param time
     * @param tempKeys
     * @return
     */
    private PointDataContainer requestPointData(
            Map<String, RequestConstraint> rcMap, long time,
            String... tempKeys) {
        List<String> parameters = new ArrayList<String>(
                Arrays.asList(tempKeys));
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
        validTime.add(Calendar.HOUR, -1);
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
                                "Error getting temp data, some obs will not display.",
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
            this.latLonEnvelopes = new ArrayList<Envelope>(2);
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

    public static Double getFahrenheitTemp(Double val){
        if (val > -100.) {
        	return (val * 1.8) + 32.;
        } else {
        	return val;
        }
    	
    }
    
    
}
