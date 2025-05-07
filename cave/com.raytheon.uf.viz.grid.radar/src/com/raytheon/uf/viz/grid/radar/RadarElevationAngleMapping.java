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
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * This tracks which Volume Coverage Pattern (VCP) a radar station is using over
 * time, along with the primary elevation angle -> true elevation angle mapping
 * for each VCP. This is used to map primary elevation angles to true elevation
 * angles for a radar station at any given time.
 *
 * The VCP defines which elevation angles are scanned by the radar and the radar
 * rotation speed. For example, VCP 35 is a clear air VCP that scans 9 elevation
 * angles ranging from 0.5 to 6.4 degrees in around 7 minutes. VCP 12 is a
 * precipitation VCP that scans 14 elevation angles ranging from 0.5 to 19.5
 * degrees in 4.5 minutes. Common VCPs are listed in elevationLists.txt.
 *
 * The true elevation angle is the actual radar angle that data is scanned at.
 * The true elevation angles are grouped into bins that each map to a primary
 * elevation angle. These bins are listed in tiltAngleGroups.txt. For example,
 * VCP 31 includes a 3.5 degree scan, while VCP 12 includes 3.1 degrees. Both of
 * these fall into the 2.7-3.6 bin that maps to a primary elevation angle of 3.4
 * degrees. The true elevation angle should be used for actual data
 * calculations, while the primary elevation angle is used where the specific
 * angle isn't as important, such as in menu text.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2024 2037939    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarElevationAngleMapping implements IAlertObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarElevationAngleMapping.class);

    /** Map of icao -> instance */
    private static final Map<String, RadarElevationAngleMapping> instanceMap = new HashMap<>();

    private final String icao;

    /**
     * Map of VCP ID -> primary elevation angle -> true elevation angle
     *
     * Access must be synchronized on {@link #lock}.
     */
    protected final Map<Integer, Map<Double, Float>> vcpPrimaryToTrueMap = new HashMap<>();

    /**
     * [VCP ID, End Time] list indicating which VCPs the radar has been using
     * over time. The radar used each VCP in the list up to (and including) the
     * specified end time, at which point it switched to the next VCP in the
     * list.
     *
     * Access must be synchronized on {@link #lock}.
     */
    protected final Deque<VcpTime> vcpTimes = new LinkedList<>();

    private final Object lock = new Object();

    /**
     * Max time to keep cached data before re-querying the DB to make sure we
     * are up to date.
     */
    private static final long CACHE_TIME_MS = TimeUtil.MILLIS_PER_MINUTE;

    /** Timestamp of when we last queried the DB. */
    protected long lastQueryTime = Long.MIN_VALUE;

    /**
     * Internal constructor. {@link #getInstance} should be used externally.
     *
     * @param icao
     *            radar station icao
     */
    protected RadarElevationAngleMapping(String icao) {
        this.icao = icao;

        ProductAlertObserver.addObserver(RadarAdapter.RADAR_SOURCE, this);
        reloadMapping();
    }

    /**
     * @param icao
     *            radar station icao
     * @return primary->true elevation angle mapping for the given radar station
     */
    public static RadarElevationAngleMapping getInstance(String icao) {
        synchronized (instanceMap) {
            return instanceMap.computeIfAbsent(icao,
                    x -> new RadarElevationAngleMapping(icao));
        }
    }

    /**
     * Get the true elevation angle that this radar actually scanned at the
     * given time for the given primary elevation angle.
     *
     * @param primaryElevationAngle
     * @param time
     * @return true elevation angle if a mapping exists for this
     *         radar/time/primary angle combo, otherwise the given primary
     *         elevation angle
     */
    public double getTrueElevationAngle(double primaryElevationAngle,
            DataTime time) {
        synchronized (lock) {
            if (lastQueryTime + CACHE_TIME_MS < System.currentTimeMillis()) {
                reloadMapping();
            }

            Float trueElevationAngle = getTrueAngle(getVcp(time),
                    primaryElevationAngle);
            if (trueElevationAngle != null) {
                return trueElevationAngle;
            }
        }
        return primaryElevationAngle;
    }

    /**
     * @return all elevation-based radar records for this instance's icao from
     *         the DB, in ascending scan time order.
     */
    protected DbQueryResponse queryIcaoRecords() {
        DbQueryRequest dbQueryRequest = new DbQueryRequest();
        dbQueryRequest.setEntityClass(RadarRecord.class);
        // Must be in ascending time for populating the VCP times list
        dbQueryRequest.setOrderByField(PluginDataObject.DATATIME_ID);
        /*
         * Ignore products that do not apply to a particular elevation, which
         * have 0/null elevation number and may store data in the primary
         * elevation angle field that isn't actually an angle and would result
         * in invalid primary->true angle mappings.
         */
        dbQueryRequest.setConstraints(Map.of(RadarAdapter.ICAO_QUERY,
                new RequestConstraint(icao), RadarRecordUtil.ELEVATION_NUM,
                new RequestConstraint("0", ConstraintType.NOT_EQUALS)));

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
     * Clear and reload the internal tracking of which VCPs are used over time,
     * along with the primary -> true elevation angle mappings for each VCP.
     */
    protected void reloadMapping() {
        synchronized (lock) {
            vcpPrimaryToTrueMap.clear();
            vcpTimes.clear();

            DbQueryResponse response = queryIcaoRecords();

            for (RadarRecord record : response
                    .getEntityObjects(RadarRecord.class)) {
                if (record.getElevationNumber() == null) {
                    /*
                     * != 0 constraint in DB request should filter out null as
                     * well for postgres, but make sure
                     */
                    continue;
                }

                Integer vcp = record.getVolumeCoveragePattern();
                Double primaryAngle = record.getPrimaryElevationAngle();
                Float trueAngle = record.getTrueElevationAngle();
                DataTime time = record.getDataTime();
                if (vcp != null && primaryAngle != null && trueAngle != null
                        && time != null) {
                    addVcpAngleMapping(vcp, primaryAngle, trueAngle);
                    addVcpTime(vcp, time);
                }
            }
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug(icao + ": Loaded VCP times: " + vcpTimes
                        + ", VCP->primary->true angle mapping: "
                        + vcpPrimaryToTrueMap);
            }
            lastQueryTime = System.currentTimeMillis();
        }
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

        obj = alertMessage.decodedAlert.get(PluginDataObject.DATATIME_ID);
        if (!(obj instanceof DataTime)) {
            return;
        }
        DataTime time = (DataTime) obj;

        obj = alertMessage.decodedAlert.get(RadarAdapter.TILT_QUERY);
        if (!(obj instanceof Double)) {
            return;
        }
        double primaryTilt = (double) obj;

        processValidAlert(time, primaryTilt);
    }

    protected void processValidAlert(DataTime time, double primaryTilt) {
        synchronized (lock) {
            VcpTime last = vcpTimes.peekLast();
            // Alert can have spatial info, ignore it
            if (last != null && last.endTime.equals(time, true)
                    && getTrueAngle(last.vcp, primaryTilt) != null) {
                // Fully mapped already, nothing to do
            } else {
                /*
                 * VCP and true angle aren't in the alert, so just indicate that
                 * we need to re-query the DB
                 */
                lastQueryTime = Long.MIN_VALUE;
            }
        }
    }

    /**
     * Get the Volume Coverage Pattern used by this radar at the given time.
     *
     * @param time
     *            volume scan time
     * @return VCP ID if found, otherwise -1
     */
    protected int getVcp(DataTime time) {
        if (time.isSpatial()) {
            time = time.clone();
            time.clearLevel();
        }
        for (VcpTime vcpTime : vcpTimes) {
            if (!time.greaterThan(vcpTime.endTime)) {
                return vcpTime.vcp;
            }
        }
        if (!vcpTimes.isEmpty()) {
            // Fallback to most recent VCP
            return vcpTimes.getLast().vcp;
        }
        return -1;
    }

    /**
     * Get the true elevation angle that corresponds to the given primary
     * elevation angle for the given VCP.
     *
     * @param vcp
     *            Volume Coverage Pattern ID
     * @param primaryAngle
     *            primary elevation angle
     * @return true elevation angle mapping if found, otherwise null
     */
    protected Float getTrueAngle(int vcp, double primaryAngle) {
        Map<Double, Float> primaryToTrueMap = vcpPrimaryToTrueMap.get(vcp);
        if (primaryToTrueMap != null) {
            return primaryToTrueMap.get(primaryAngle);
        }
        return null;
    }

    /**
     * Update the internal tracking of which VCPs are used over time. Times must
     * be passed in in ascending order.
     *
     * @param vcp
     *            Volume Coverage Pattern ID
     *
     * @param time
     *            volume scan time that uses that VCP
     */
    protected void addVcpTime(int vcp, DataTime time) {
        VcpTime last = vcpTimes.peekLast();
        if (last == null) {
            vcpTimes.add(new VcpTime(vcp, time));
        } else if (last.vcp == vcp) {
            last.setEndTime(time);
        } else {
            vcpTimes.add(new VcpTime(vcp, time));
            if (last.endTime.equals(time)) {
                statusHandler.error(icao + ": Multiple VCPs for " + time
                        + " scan: " + last.vcp + ", " + vcp);
            }
        }
    }

    /**
     * Update the internal mapping of VCP -> primary elevation angle -> true
     * elevation angle.
     *
     * @param vcp
     *            VCP ID
     * @param primaryAngle
     *            primary elevation angle that maps to the given true elevation
     *            angle for this VCP
     * @param trueAngle
     *            true elevation angle
     */
    protected void addVcpAngleMapping(int vcp, double primaryAngle,
            float trueAngle) {
        Map<Double, Float> primaryToTrueMap = vcpPrimaryToTrueMap
                .computeIfAbsent(vcp, x -> new HashMap<>());
        Float prevTrueAngle = primaryToTrueMap.put(primaryAngle, trueAngle);
        if (prevTrueAngle != null
                && Math.abs(prevTrueAngle - trueAngle) >= 0.0001) {
            /*
             * Only log if difference is above some small threshold. We are
             * checking for incorrect code logic here, not for if the radar was
             * set at a negligibly different true angle for the same VCP.
             */
            statusHandler.error(icao + ": VCP " + vcp
                    + " primary elevation angle " + primaryAngle
                    + " maps to multiple true elevation angles: "
                    + prevTrueAngle + ", " + trueAngle);
        }
    }

    /**
     * Convenience class for holding a Volume Coverage Pattern ID and the last
     * scan time that the radar used that VCP.
     */
    protected static class VcpTime {

        protected final int vcp;

        protected DataTime endTime;

        public VcpTime(int vcp, DataTime endTime) {
            this.vcp = vcp;
            this.endTime = endTime;
        }

        public void setEndTime(DataTime endTime) {
            this.endTime = endTime;
        }

        @Override
        public String toString() {
            return vcp + " until " + endTime;
        }
    }
}
