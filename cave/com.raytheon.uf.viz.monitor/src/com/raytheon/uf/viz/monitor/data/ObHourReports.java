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
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;

/**
 * This class is a container of ObZoneHourReports objects for a caller-specified
 * nominal date-time. (this class corresponds to the RcHourReports c++ class in
 * AWIPS-1)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec. 1, 2009  3424       zhao       Initial creation.
 * Oct.29, 2012  1297       skorolev   Changed HashMap to Map
 * Oct.31  2012  1297       skorolev   Cleaned code.
 * Sep 04  2014  3220       skorolev   Added updateZones method.
 * Dec 18  2014  3841       skorolev   Corrected updateZones method.
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
    private Date nominalTime;

    /**
     * application name (SNOW, FOG, SAFESEAS)
     */
    private CommonConfig.AppName appName;

    /**
     * key is zone id, value is ObZoneHourReports object
     */
    private Map<String, ObZoneHourReports> hourReports;

    private AbstractThresholdMgr thresholdMgr;

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
        hourReports = new HashMap<String, ObZoneHourReports>();
        Map<String, List<String>> zoneStationMap = MonitoringArea
                .getPlatformMap();
        for (String zone : zoneStationMap.keySet()) {
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
        List<String> zones = MonitoringArea.getZoneIds(station);
        if (zones.size() == 0) {
            statusHandler
                    .error("Error: station: "
                            + station
                            + " is not associated with any zone in the monitoring area");
            return;
        }
        boolean hasZone = false;
        for (String zone : zones) {
            if (hourReports.containsKey(zone)) {
                hasZone = true;
                hourReports.get(zone).addReport(report);
            }
        }
        if (hasZone == false) {
            statusHandler
                    .error("Error in addreport() of ObHourReports: unable to add obs report to data archive");
        }
    }

    /**
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
        for (String zone : hourReports.keySet()) {
            tblData.addTableRowData(this.getObZoneHourReports(zone)
                    .getZoneTableRowData());
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
        for (String zone : hourReports.keySet()) {
            CellType theAlgCellType;
            if (algCellType.containsKey(zone)) {
                theAlgCellType = algCellType.get(zone);
            } else {
                theAlgCellType = CellType.NotAvailable;
            }
            tblData.addTableRowData(this.getObZoneHourReports(zone)
                    .getFogZoneTableRowData(theAlgCellType));
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
        for (String zone : hourReports.keySet()) {
            CellType theFogCellType;
            if (fogCellType.containsKey(zone)) {
                theFogCellType = fogCellType.get(zone);
            } else {
                theFogCellType = CellType.NotAvailable;
            }
            tblData.addTableRowData(this.getObZoneHourReports(zone)
                    .getSSZoneTableRowData(theFogCellType));
        }
        return tblData;
    }

    /**
     * Returns the ObZoneHourReports object of a caller-specified zone. If such
     * object not available, returns null.
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
     * @return nominalTime
     */
    public Date getNominalTime() {
        return nominalTime;
    }

    /**
     * @return appName
     */
    public CommonConfig.AppName getAppName() {
        return appName;
    }

    /**
     * Updates zones in the Hour Reports
     */
    public void updateZones() {
        Map<String, List<String>> zoneStationMap = MonitoringArea
                .getPlatformMap();
        // Updated list of zones
        Set<String> updtZones = zoneStationMap.keySet();
        // add zones
        for (String zone : updtZones) {
            if (!hourReports.keySet().contains(zone)) {
                hourReports.put(zone, new ObZoneHourReports(nominalTime, zone,
                        appName, thresholdMgr));
            }
        }
        // remove zones
        hourReports.keySet().retainAll(updtZones);
        // add and(or) remove stations
        for (String zone : updtZones) {
            // Updated list of stations in this zone
            List<String> updtStns = zoneStationMap.get(zone);
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
            // remove stations
            hourReports.get(zone).getZoneHourReports().keySet()
                    .retainAll(updtStns);
        }
    }
}
