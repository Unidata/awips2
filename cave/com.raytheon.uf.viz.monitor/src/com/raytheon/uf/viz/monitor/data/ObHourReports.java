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
package com.raytheon.uf.viz.monitor.data;

import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.monitor.xml.StationIdXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class is a container of ObZoneHourReports objects for a caller-specified
 * nominal date-time. (this class corresponds to the RcHourReports c++ class in
 * AWIPS-1)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date           Ticket#    Engineer   Description
 * ------------ ---------- ----------- --------------------------
 * Dec. 1, 2009  3424       zhao       Initial creation.
 * Oct.29, 2012  1297       skorolev   Changed HashMap to Map
 * Oct.31  2012  1297       skorolev   Cleaned code.
 * Sep 04  2014  3220       skorolev   Added updateZones method.
 * Mar 17  2015  3888       dhladky    check for nulls
 * Sep 25  2015  3873       skorolev   Corrected addReport for moving platforms.
 * Oct 19  2015  3841       skorolev   Added try to saveConfigXml
 * Nov 12  2015  3841       dhladky    Augmented Slav's fix for moving platforms.
 * Dec 02  2015  3873       dhladky    Pulled 3841 changes to 16.1.1.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */

public class ObHourReports {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ObHourReports.class);

    /**
     * the nominal time of this ObHourReports object
     */
    private final Date nominalTime;

    /**
     * application name (SNOW, FOG, SAFESEAS)
     */
    private final CommonConfig.AppName appName;

    /**
     * key is zone id, value is ObZoneHourReports object
     */
    private final Map<String, ObZoneHourReports> hourReports;

    /**
     * current threshold manager
     */
    private final AbstractThresholdMgr thresholdMgr;

    private final Set<String> zones = new HashSet<String>();

    private final FSSObsMonitorConfigurationManager configMgr;

    /**
     * constructor
     * 
     * @param appName
     */
    public ObHourReports(Date nominalTime, CommonConfig.AppName appName,
            AbstractThresholdMgr thresholdMgr) {
        this.nominalTime = nominalTime;
        this.appName = appName;
        this.thresholdMgr = thresholdMgr;
        configMgr = this.thresholdMgr.getCfgMgr();

        hourReports = new HashMap<String, ObZoneHourReports>();
        zones.clear();
        zones.addAll(configMgr.getAreaList());
        for (String zone : zones) {
            hourReports.put(zone, new ObZoneHourReports(nominalTime, zone,
                    appName, thresholdMgr));
        }
    }

    /**
     * Adds data to hourReports
     * 
     * @param report
     */
    public void addReport(ObReport report) {
        String station = report.getPlatformId();
        List<String> stationZones = configMgr.getAreaByStationId(station);
        // If station has no associated zone:
        if (stationZones.isEmpty()) {
            if (appName.equals(AppName.FOG) || appName.equals(AppName.SAFESEAS)) {
                // Associate moving platform with monitoring zones
                double shipDist = configMgr.getShipDistance();
                stationZones.addAll(findZoneForShip(report, shipDist));
            } else {
                statusHandler
                        .warn("Error: station: "
                                + station
                                + " is not associated with any zone in the monitoring area");
                return;
            }
        }
        // Add station report to all associated zones.
        for (String zone : stationZones) {
            if (hourReports.containsKey(zone)) {
                hourReports.get(zone).addReport(report);
            }
        }
        return;
    }

    /**
     * Find zones to include a moving platform.
     * 
     * @param report
     *            from moving platform
     * @param shipDist
     *            distance from area configuration file
     */
    private Set<String> findZoneForShip(ObReport report, double shipDist) {

        double latShip = report.getLatitude();
        double lonShip = report.getLongitude();
        Set<String> shipZones = new HashSet<String>();

        for (String zone : zones) {
            try {
                // use only marine zones
                if (zone.charAt(2) == 'Z') {

                    Coordinate zcoor = MonitorAreaUtils.getZoneCenter(zone);
                    double latZone;
                    double lonZone;

                    if (zcoor != null) {
                        latZone = zcoor.y;
                        lonZone = zcoor.x;
                    } else {
                        // Newly added zone
                        AreaIdXML zoneXML = configMgr.getAreaXml(zone);
                        latZone = zoneXML.getCLat();
                        lonZone = zoneXML.getCLon();
                    }
                    double shipToZone = distance(latShip, lonShip, latZone,
                            lonZone);
                    if (shipToZone <= shipDist) {
                        // associate moving platform with monitoring zone.
                        shipZones.add(zone);
                        statusHandler.handle(Priority.DEBUG,
                                "<<<======>>>" + zone + "\tplatform = "
                                        + report.getPlatformId() + "\tdist = "
                                        + shipToZone + "\t shipDist = "
                                        + shipDist);
                        StationIdXML stnXML = new StationIdXML();
                        stnXML.setName(report.getPlatformId());
                        stnXML.setType(ReportType.MARITIME.name());
                        configMgr.getAreaXml(zone).addStationIdXml(stnXML);
                    }
                } else {
                    continue;
                }
            } catch (SpatialException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not determine distance from moving platform to zone: Platform: "
                                + report.getPlatformId() + " Zone: " + zone, e);
            }
        }
        try {
            configMgr.saveConfigXml();
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, "Problem saving Localization file!", e);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, "Problem serializaing Localization File!", e);
        }
        return shipZones;
    }

    /**
     * Gets HourReports
     * 
     * @return hourReports
     */
    public Map<String, ObZoneHourReports> getHourReports() {
        return hourReports;
    }

    /**
     * Gets data for Zone table.
     * 
     * @return tblData
     */
    public TableData getZoneTableData() {
        TableData tblData = new TableData(appName);
        // Area configuration manager controls what gets displayed
        for (String zone : configMgr.getAreaList()) {
            if (hourReports.containsKey(zone)) {
                tblData.addTableRowData(this.getObZoneHourReports(zone)
                        .getZoneTableRowData());
            }
        }
        return tblData;
    }

    /**
     * Gets data for Fog Table.
     * 
     * @param algCellType
     * @return tblData
     */
    public TableData getFogZoneTableData(Map<String, CellType> algCellType) {
        TableData tblData = new TableData(AppName.FOG);
        // Area configuration manager controls what gets displayed
        for (String zone : configMgr.getAreaList()) {
            if (hourReports.containsKey(zone)) {
                CellType theAlgCellType;
                if (algCellType.containsKey(zone)) {
                    theAlgCellType = algCellType.get(zone);
                } else {
                    theAlgCellType = CellType.NotAvailable;
                }
                tblData.addTableRowData(this.getObZoneHourReports(zone)
                        .getFogZoneTableRowData(theAlgCellType));
            }
        }
        return tblData;
    }

    /**
     * Gets data for SAFESEAS table.
     * 
     * @param fogCellType
     * @return tblData
     */
    public TableData getSSZoneTableData(Map<String, CellType> fogCellType) {
        TableData tblData = new TableData(AppName.SAFESEAS);
        // Area configuration manager controls what gets displayed
        for (String zone : configMgr.getAreaList()) {
            if (hourReports.containsKey(zone)) {
                CellType theFogCellType;
                if (fogCellType.containsKey(zone)) {
                    theFogCellType = fogCellType.get(zone);
                } else {
                    theFogCellType = CellType.NotAvailable;
                }
                tblData.addTableRowData(this.getObZoneHourReports(zone)
                        .getSSZoneTableRowData(theFogCellType));
            }
        }
        return tblData;
    }

    /**
     * Gets ObZoneHourReports Returns the ObZoneHourReports object of a
     * caller-specified zone. If such object not available, returns null.
     * 
     * @param zone
     * @return hour reports
     */
    public ObZoneHourReports getObZoneHourReports(String zone) {
        if (!hourReports.containsKey(zone)) {
            return null;
        }
        return hourReports.get(zone);
    }

    /**
     * Gets NominalTime
     * 
     * @return nominalTime
     */
    public Date getNominalTime() {
        return nominalTime;
    }

    /**
     * Gets AppName
     * 
     * @return appName
     */
    public CommonConfig.AppName getAppName() {
        return appName;
    }

    /**
     * Updates zones in the Hour Reports
     * 
     * @param configMgr
     */
    public void updateZones() {
        // Updated list of zones
        List<String> updtZones = configMgr.getAreaList();
        // remove zones
        hourReports.keySet().retainAll(updtZones);
        // add zones
        for (String zone : updtZones) {
            if (!hourReports.keySet().contains(zone)) {
                hourReports.put(zone, new ObZoneHourReports(nominalTime, zone,
                        appName, thresholdMgr));
            } 
        }
        // add and(or) remove stations
        for (String zone : updtZones) {
            // Updated list of stations in this zone
            List<String> updtStns = configMgr.getAreaStations(zone);
            // remove stations
            hourReports.get(zone).getZoneHourReports().keySet()
                    .retainAll(updtStns);
            // add stations
            for (String stn : updtStns) {
                if (!hourReports.get(zone).getZoneHourReports()
                        .containsKey(stn)) {
                    hourReports
                            .get(zone)
                            .getZoneHourReports()
                            .put(stn,
                                    new ObStnHourReports(nominalTime, zone,
                                            stn, appName, thresholdMgr));
                }
            }
            // update hourReports for current zone
            hourReports.get(zone).getZoneHourReports();
        }
    }

    /**
     * Distance between two coordinates.
     * 
     * @param lat1
     * @param lon1
     * @param lat2
     * @param lon2
     * @return distance in km
     */
    public static double distance(double lat1, double lon1, double lat2,
            double lon2) {

        // Earth's radius of 6378.137 kilometers
        float EarthRadius = 6378.137f;

        double dLat = toRad(lat2 - lat1);
        double dLon = toRad(lon2 - lon1);
        lat1 = toRad(lat1);
        lat2 = toRad(lat2);

        double a = Math.sin(dLat / 2) * Math.sin(dLat / 2) + Math.sin(dLon / 2)
                * Math.sin(dLon / 2) * Math.cos(lat1) * Math.cos(lat2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return EarthRadius * c;
    }

    /**
     * From grad to radian.
     * 
     * @param value
     * @return
     */
    private static double toRad(double value) {
        return value * Math.PI / 180;
    }
}