/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.grid.radar;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord.ScanType;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Tracks the available radar volume scan times, and the highest tilt available
 * for the current volume scan. This only tracks {@link ScanType#NORMAL} scan
 * times, ignoring SAILS/MRLE times.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarVolumeScanTracker implements IAlertObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarVolumeScanTracker.class);

    /**
     * Map of (icao, radar parameter) -> instance.
     */
    private static final Map<Pair<String, String>, RadarVolumeScanTracker> instanceMap = new HashMap<>();

    /**
     * Max time difference between the current volume scan and the previous
     * volume scan for a virtual volume to be created from the two scans.
     *
     * Defaults to 13 minutes per NWS feedback. (The clear air VCPs are around
     * 10min and there is a 2min system configuration check that can happen
     * every 8hrs, so a 13min cutoff is used to be safe.)
     */
    private static final long VIRTUAL_VOLUME_MAX_SCAN_DIFF_MS = Long.getLong(
            "radar.virtual.volume.max.scan.diff.ms",
            13 * TimeUtil.MILLIS_PER_MINUTE);

    /**
     * Max time to keep cached data before re-querying the DB to make sure we
     * are up to date.
     */
    private static final long CACHE_TIME_MS = TimeUtil.MILLIS_PER_MINUTE;

    /**
     * Timestamp of when we last queried times/tilts from the DB.
     */
    protected long lastQueryTime = Long.MIN_VALUE;

    /**
     * Sorted set of all normal volume scan times for our icao and product
     * codes.
     */
    protected final NavigableSet<DataTime> scanTimes = new TreeSet<>();

    /**
     * Latest/highest tilt in the current volume scan.
     */
    protected Double latestTilt;

    /**
     * Set of product codes for a single radar parameter.
     */
    private final Set<Integer> productCodes;

    private final String icao;

    protected RadarVolumeScanTracker(String icao,
            Collection<Integer> productCodes) {
        this.icao = icao;
        this.productCodes = Collections
                .unmodifiableSet(new HashSet<>(productCodes));

        ProductAlertObserver.addObserver(RadarAdapter.RADAR_SOURCE, this);
        loadVolumeScans();
    }

    protected DbQueryResponse queryTimesAndTilts() {
        DbQueryRequest dbQueryRequest = new DbQueryRequest();
        dbQueryRequest.setEntityClass(RadarRecord.class);
        dbQueryRequest.addRequestField(PluginDataObject.DATATIME_ID);
        dbQueryRequest.addRequestField(RadarAdapter.TILT_QUERY);
        dbQueryRequest.setDistinct(true);
        List<String> productCodeStrs = productCodes.stream()
                .map(i -> Integer.toString(i)).collect(Collectors.toList());

        dbQueryRequest.setConstraints(Map.of(RadarAdapter.ICAO_QUERY,
                new RequestConstraint(icao), RadarAdapter.PRODUCT_CODE_QUERY,
                new RequestConstraint(productCodeStrs),
                RadarAdapter.SCAN_TYPE_QUERY,
                new RequestConstraint(ScanType.NORMAL.name())));

        DbQueryResponse response;
        try {
            response = (DbQueryResponse) RequestRouter.route(dbQueryRequest);
        } catch (Exception e) {
            statusHandler.error("Error querying volume scan info", e);
            response = new DbQueryResponse();
        }

        return response;
    }

    /**
     * Query all volume scan times/tilts from the DB for our icao and product
     * codes, and populate scanTimes/latestTilt from them.
     */
    protected void loadVolumeScans() {
        synchronized (scanTimes) {
            scanTimes.clear();
            latestTilt = null;

            DbQueryResponse response = queryTimesAndTilts();

            DataTime[] times = response.getFieldObjects(
                    PluginDataObject.DATATIME_ID, DataTime.class);
            if (!ArrayUtils.isEmpty(times)) {
                Collections.addAll(scanTimes, times);
                DataTime latestScanTime = scanTimes.last();

                for (Map<String, Object> result : response.getResults()) {
                    DataTime time = (DataTime) result
                            .get(PluginDataObject.DATATIME_ID);
                    if (latestScanTime.equals(time)) {
                        Double tilt = (Double) result
                                .get(RadarAdapter.TILT_QUERY);
                        if (latestTilt == null || tilt > latestTilt) {
                            latestTilt = tilt;
                        }
                    }
                }
            }
            lastQueryTime = System.currentTimeMillis();
        }
    }

    /**
     * Get the latest two volume scan times and the latest tilt. This verifies
     * that the times are close enough together to create a virtual volume.
     *
     * @return virtual volume info
     */
    public VirtualVolumeInfo getVirtualVolumeInfo() {
        synchronized (scanTimes) {
            if (lastQueryTime + CACHE_TIME_MS < System.currentTimeMillis()) {
                loadVolumeScans();
            }
            if (latestTilt != null) {
                Iterator<DataTime> iter = scanTimes.descendingIterator();
                if (iter.hasNext()) {
                    DataTime last = iter.next();
                    if (iter.hasNext()) {
                        DataTime secondToLast = iter.next();

                        if (last.getValidTime().getTimeInMillis() - secondToLast
                                .getValidTime()
                                .getTimeInMillis() <= VIRTUAL_VOLUME_MAX_SCAN_DIFF_MS) {
                            return new VirtualVolumeInfo(last, secondToLast,
                                    latestTilt);
                        }
                    }
                }
            }
        }
        return null;
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        for (AlertMessage alertMessage : alertMessages) {
            processAlert(alertMessage);
        }
    }

    protected void processAlert(AlertMessage alertMessage) {
        Object obj = alertMessage.decodedAlert.get(RadarAdapter.ICAO_QUERY);
        if (obj == null || !icao.equals(obj.toString())) {
            return;
        }
        obj = alertMessage.decodedAlert.get(RadarAdapter.PRODUCT_CODE_QUERY);
        if (!(obj instanceof Integer) || !productCodes.contains(obj)) {
            return;
        }

        obj = alertMessage.decodedAlert.get(PluginDataObject.DATATIME_ID);
        if (!(obj instanceof DataTime)) {
            return;
        }
        DataTime time = (DataTime) obj;

        obj = alertMessage.decodedAlert.get(RadarAdapter.TILT_QUERY);
        if (!(obj instanceof Double)) {
            return;
        }
        double tilt = (double) obj;

        processValidAlert(time, tilt);
    }

    /**
     * Process a radar alert that has already been verified to be for our icao
     * and product codes.
     *
     * @param time
     *            alert time
     * @param tilt
     *            alert tilt
     */
    protected void processValidAlert(DataTime time, double tilt) {
        synchronized (scanTimes) {
            // Alert can have spatial info, ignore it
            if (!scanTimes.isEmpty() && scanTimes.last().equals(time, true)) {
                // Update for new tilt within latest normal volume scan
                if (latestTilt == null || tilt > latestTilt) {
                    latestTilt = tilt;
                }
            } else {
                /*
                 * The alert doesn't tell us the scan type (SAILS/MRLE vs
                 * normal), so we can't know if we should update for it or not.
                 * Just reset the query time so that we re-query the DB.
                 */
                lastQueryTime = Long.MIN_VALUE;
            }
        }
    }

    /**
     * Get the volume scan tracker for the given icao and parameter.
     *
     * @param icao
     *            the radar station icao
     * @param paramAbbrev
     *            the radar grid parameter abbreviation
     * @return the volume scan tracker
     */
    public static RadarVolumeScanTracker getInstance(String icao,
            String paramAbbrev) {
        synchronized (instanceMap) {
            RadarVolumeScanTracker instance = instanceMap.computeIfAbsent(
                    ImmutablePair.of(icao, paramAbbrev),
                    x -> new RadarVolumeScanTracker(icao,
                            RadarProductCodeMapping.getInstance()
                                    .getProductCodesForAbbrev(paramAbbrev)));
            return instance;
        }
    }
}
