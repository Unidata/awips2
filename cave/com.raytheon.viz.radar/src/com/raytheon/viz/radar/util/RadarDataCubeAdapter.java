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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord.ScanType;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.util.AbstractPointDataInventory;
import com.raytheon.viz.pointdata.util.PointDataCubeAdapter;
import com.raytheon.viz.radar.frame.RadarDataTime;

/**
 *
 * DataCubeAdapter for Radar Data. Passes the work for point data to radar point
 * data adapters
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Oct 08, 2009           bsteffen  Initial creation
 * Nov 21, 2009  3576     rjpeter   Refactored use of DerivParamDesc.
 * May 13, 2015  4461     bsteffen  Generate radar times from time queries.
 * Nov 02, 2015  5071     bsteffen  Fix NPE when time query of Unit Status
 * Oct 29, 2022  8959     mapeters  Update how data time levels are set
 * Mar 06, 2025  2038488  mapeters  Add linkSailsMrleTimesToNormalScanTimes
 * Jun 04, 2025  2038858  mapeters  Fix SAILS/MRLE time linking for multi-icao
 *                                  (mosaic) requests
 *
 * </pre>
 *
 * @author bsteffen
 */
public class RadarDataCubeAdapter extends PointDataCubeAdapter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarDataCubeAdapter.class);

    private static final String DATA_TIME_FIELD = "dataTime";

    private static final String LATEST_DATA_TIME_FIELD = "dataTime.refTime";

    private static final String LEVEL_FIELD = "primaryElevationAngle";

    private static final String ELEVATION_FIELD = "elevationNumber";

    private static final String VOLUME_FIELD = "volumeScanNumber";

    private static final String SCAN_TYPE_FIELD = "scanType";

    private static final String ICAO_FIELD = "icao";

    @Override
    public String[] getSupportedPlugins() {
        return new String[] { "radar" };
    }

    @Override
    public void initInventory() {
        if (inventory == null) {
            AbstractPointDataInventory pointInventory = new VwpInventory();
            try {
                pointInventory
                        .initTree(DerivedParameterGenerator.getDerParLibrary());
                this.inventory = pointInventory;
            } catch (DataCubeException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * @param queryParams
     * @return
     * @throws VizException
     */
    @Override
    public String getType(Map<String, RequestConstraint> queryParams)
            throws VizException {
        String type = super.getType(queryParams);
        if (VwpInventory.ProductCode.toString().equals(type)) {
            return VwpInventory.Mnemonic;
        }
        return type;
    }

    @Override
    public PointDataContainer getBaseRecords(Collection<String> baseParameters,
            Map<String, RequestConstraint> queryParams)
            throws DataCubeException {
        return ((VwpInventory) inventory).getBaseRecords(baseParameters,
                queryParams);
    }

    private Collection<DataTime> processTimeQueryResponse(
            DbQueryResponse response, boolean latestOnly, BinOffset binOffset) {
        String dataTimefield = DATA_TIME_FIELD;
        if (latestOnly) {
            dataTimefield = LATEST_DATA_TIME_FIELD;
        }
        List<DataTime> results = new ArrayList<>();
        for (Map<String, Object> map : response.getResults()) {
            DataTime time = null;
            if (latestOnly) {
                time = new DataTime((Date) map.get(dataTimefield), 0);
            } else {
                time = (DataTime) map.get(dataTimefield);
                Number elevation = (Number) map.get(ELEVATION_FIELD);
                /*
                 * Certain products such as Unit Status do not apply to a
                 * particular elevation.
                 */
                if (elevation != null) {
                    RadarDataTime radarTime = new RadarDataTime(time);
                    radarTime.setElevationNumber(elevation.intValue());
                    Number volume = (Number) map.get(VOLUME_FIELD);
                    radarTime.setVolumeScanNumber(volume.intValue());
                    ScanType scanType = (ScanType) map.get(SCAN_TYPE_FIELD);
                    radarTime.setScanType(scanType);
                    String icao = (String) map.get(ICAO_FIELD);
                    radarTime.setIcao(icao);
                    time = radarTime;
                }
                Number level = (Number) map.get(LEVEL_FIELD);
                time.setLevel(level.doubleValue(), RadarUtil.TILT);
            }
            // Best res requests need this because they span a time period
            if (time.getRefTime()
                    .before(SimulatedTime.getSystemTime().getTime())) {
                results.add(time);
            }
        }

        linkSailsMrleTimesToNormalScanTimes(results);

        Set<DataTime> resultsSet;
        if (binOffset != null) {
            Set<DataTime> scaledDates = new TreeSet<>();
            for (DataTime dt : results) {
                scaledDates.add(binOffset.getNormalizedTime(dt));
            }
            resultsSet = scaledDates;
        } else {
            /*
             * This set conversion may be unnecessary, but exists to match the
             * behavior before the SAILS/MRLE time linking logic was added.
             */
            resultsSet = new HashSet<>(results);
        }

        return resultsSet;
    }

    private DbQueryRequest getTimeQueryRequest(
            Map<String, RequestConstraint> queryParams, boolean latestOnly) {
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(queryParams);

        String dataTimefield = DATA_TIME_FIELD;
        if (latestOnly) {
            dataTimefield = LATEST_DATA_TIME_FIELD;
        }
        request.addRequestField(dataTimefield, latestOnly);
        if (!latestOnly) {
            request.addRequestField(LEVEL_FIELD);
            request.addRequestField(ELEVATION_FIELD);
            request.addRequestField(VOLUME_FIELD);
            request.addRequestField(SCAN_TYPE_FIELD);
            request.addRequestField(ICAO_FIELD);
        }
        request.setDistinct(true);
        return request;
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws DataCubeException {
        List<DbQueryRequest> dbRequests = new ArrayList<>(requests.size());
        for (TimeQueryRequest request : requests) {
            dbRequests.add(getTimeQueryRequest(request.getQueryTerms(),
                    request.isMaxQuery()));
        }
        DbQueryRequestSet requestSet = new DbQueryRequestSet();
        requestSet.setQueries(dbRequests.toArray(new DbQueryRequest[0]));
        DbQueryResponseSet responseSet;
        try {
            responseSet = (DbQueryResponseSet) RequestRouter.route(requestSet);
        } catch (Exception e) {
            throw new DataCubeException(e);
        }
        List<List<DataTime>> result = new ArrayList<>(requests.size());
        for (int i = 0; i < requests.size(); i++) {
            DbQueryResponse response = responseSet.getResults()[i];
            TimeQueryRequest request = requests.get(i);
            Collection<DataTime> times = processTimeQueryResponse(response,
                    request.isMaxQuery(), request.getBinOffset());

            result.add(new ArrayList<>(times));
        }
        return result;
    }

    /**
     * For each SAILS (or MRLE) scan time, determine the time of the normal scan
     * that the SAILS is a part of. That normal scan time is then stored in the
     * SAILS radar time. This is done to support time matching.
     *
     * @param times
     *            list of times in which to update any SAILS/MRLE times. This is
     *            specifically a list because the same time for different ICAOs
     *            are considered duplicates, and we don't want to throw any of
     *            them out.
     */
    protected static void linkSailsMrleTimesToNormalScanTimes(
            List<DataTime> times) {
        List<RadarDataTime> sailsMrleRdts = times.stream()
                .filter(dt -> dt instanceof RadarDataTime rdt
                        && rdt.getScanType() != ScanType.NORMAL)
                .map(RadarDataTime.class::cast).collect(Collectors.toList());
        Map<String, Map<ScanType, Long>> minOffsets = new HashMap<>();
        for (RadarDataTime sailsMrleRdt : sailsMrleRdts) {
            for (DataTime dt : times) {
                if (dt instanceof RadarDataTime normalRdt
                        && normalRdt.getScanType() == ScanType.NORMAL
                        && normalRdt.isSameScan(sailsMrleRdt)) {
                    long offset = sailsMrleRdt.getMatchRef()
                            - normalRdt.getMatchRef();
                    Map<ScanType, Long> icaoMinOffsets = minOffsets
                            .computeIfAbsent(sailsMrleRdt.getIcao(),
                                    icao -> new HashMap<>());
                    icaoMinOffsets.compute(sailsMrleRdt.getScanType(),
                            (scanType, minOffset) -> minOffset == null ? offset
                                    : Math.min(offset, minOffset));
                    sailsMrleRdt.setNormalScanMatchRef(normalRdt.getMatchRef());
                    break;
                }
            }
        }
        /*
         * For any SAILS/MRLE that we didn't find a corresponding normal scan
         * time for, make a best guess at the normal scan time. This should only
         * occur for SAILS/MRLE times right after the purge cutoff, whose normal
         * scan time has been purged.
         */
        for (RadarDataTime sailsRdt : sailsMrleRdts) {
            if (sailsRdt.getNormalScanMatchRef() == null) {
                String icao = sailsRdt.getIcao();
                ScanType scanType = sailsRdt.getScanType();
                Long minDiff = minOffsets.getOrDefault(icao, Map.of())
                        .get(scanType);
                statusHandler.info("Unable to determine normal scan time for "
                        + icao + " " + scanType + " scan: " + sailsRdt
                        + " elevation " + sailsRdt.getElevationNumber()
                        + ". Using minimum time difference: " + minDiff);
                if (minDiff != null) {
                    sailsRdt.setNormalScanMatchRef(
                            sailsRdt.getMatchRef() - minDiff);
                }
            }
        }
    }
}
