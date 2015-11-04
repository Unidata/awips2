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

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;

/**
 * This class is a container of ObStnHourReports objects for caller-specified
 * zone and nominal date-time (this class corresponds to the RcZoneHourReports
 * c++ class in AWIPS 1)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec. 1, 2009  3424       zhao       Initial creation.
 * Jan 25, 2010  4281       zhao       Modified updateWorstValuesFog method.
 * Sep 18, 2015  3873       skorolev   Added moving platform's reports.Replaced MonitoringArea with areaConfig.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */

public class ObZoneHourReports {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ObZoneHourReports.class);

    /** the nominal time and zone ID of this ObZoneHourReports object */
    private Date nominalTime;

    /** Monitoring area */
    private String zone;

    /** Thresholds manager */
    private AbstractThresholdMgr thresholdMgr;

    /** application name (snow, fog, safeseas, etc) */
    private CommonConfig.AppName appName;

    /** key is station id, value is ObStnHourReports object */
    private HashMap<String, ObStnHourReports> zoneHourReports;

    /**
     * this object stores worst values of the observation variables reported
     * during this nominal hour for this zone
     */
    private ObReport worstValues;

    /**
     * Constructor
     * 
     * @param nominalTime
     * @param zone
     * @param appName
     * @param thresholdMgr
     */
    public ObZoneHourReports(Date nominalTime, String zone,
            CommonConfig.AppName appName, AbstractThresholdMgr thresholdMgr) {
        this.nominalTime = nominalTime;
        this.zone = zone;
        this.appName = appName;
        this.thresholdMgr = thresholdMgr;
        zoneHourReports = new HashMap<String, ObStnHourReports>();
        List<String> stations = thresholdMgr.getCfgMgr().getAreaStations(zone);
        if (!stations.isEmpty()) {
            for (String station : stations) {
                zoneHourReports.put(station, new ObStnHourReports(nominalTime,
                        zone, station, appName, thresholdMgr));
            }
        }
        InitWorstValues();
    }

    /**
     * Initiates the worst values.
     */
    private void InitWorstValues() {
        worstValues = new ObReport();
        worstValues.setZoneId(zone);
    }

    /**
     * The worst direction
     * 
     * @param xmlKeyFrom
     *            : XML key of a directional obs from variable
     * @param xmlKeyTo
     *            : XML key of a directional obs to variable
     * @param dirCurrent
     *            : the current worst value
     * @param dirNew
     *            : the value contained in a new obs report
     * @return : the value of worse threat level
     */
    private float worseDirection(String xmlKeyFrom, String xmlKeyTo,
            float dirCurrent, float dirNew) {
        /**
         * decide which one of the two wind directions is worse by comparing
         * their corresponding threat levels
         */
        if (dirNew == ObConst.MISSING) {
            return dirCurrent;
        }

        if (dirCurrent == ObConst.MISSING) {
            return dirNew;
        }

        CellType cellTypeCurrent = thresholdMgr
                .getDirectionalThresholdValueCellType(DataUsageKey.DISPLAY,
                        zone, xmlKeyFrom, xmlKeyTo, dirCurrent);

        if (cellTypeCurrent == CellType.R) {
            /**
             * already worst threat level
             */
            return dirCurrent;
        }

        CellType cellTypeNew = thresholdMgr
                .getDirectionalThresholdValueCellType(DataUsageKey.DISPLAY,
                        zone, xmlKeyFrom, xmlKeyTo, dirNew);

        /**
         * CellType is enumerated in the order R, Y, G, N/A, ...
         */
        if (cellTypeNew.compareTo(cellTypeCurrent) < 0) {
            /**
             * dirNew corresponds to a higher threat level
             */
            return dirNew;
        }

        return dirCurrent;
    }

    /**
     * Adds report
     * 
     * @param report
     */
    public void addReport(ObReport report) {
        String station = report.getPlatformId();
        nominalTime = report.getObservationTime();

        if (zoneHourReports.containsKey(station)) {
            zoneHourReports.get(station).addReport(report);
        } else {
            // Add report from moving platform.
            ObStnHourReports shipRpts = new ObStnHourReports(
                    report.getObservationTime(), this.getZone(), station,
                    this.getAppName(), this.thresholdMgr);
            shipRpts.addReport(report);
            zoneHourReports.put(station, shipRpts);
        }
    }

    /**
     * Gets StationTableData
     * 
     * @return
     */
    public TableData getStationTableData() {
        TableData tblData = new TableData(appName);
        for (String station : zoneHourReports.keySet()) {
            tblData.addTableRowData(this.getObStnHourReports(station)
                    .getStationTableRowData());
        }
        return tblData;
    }

    /**
     * Returns a row of a zone table based on all reports within the nominal
     * hour for the specified zone. If no data available, an empty/default row
     * of table data is returned.
     * 
     * @return
     */
    public TableRowData getZoneTableRowData() {
        TableRowData tblRowData = null;

        if (appName.equals(CommonConfig.AppName.FOG)) {
            updateFogWorstValues();
            tblRowData = TableUtil.getFogTableRowData(zone, zone, worstValues,
                    thresholdMgr, CellType.NotAvailable);
        } else if (appName.equals(CommonConfig.AppName.SAFESEAS)) {
            updateSafeseasWorstValues();
            tblRowData = TableUtil.getSafeseasTableRowData(zone, zone,
                    worstValues, thresholdMgr, CellType.NotAvailable);
        } else if (appName.equals(CommonConfig.AppName.SNOW)) {
            updateSnowWorstValues();
            tblRowData = TableUtil.getSnowTableRowData(zone, zone, worstValues,
                    thresholdMgr);
        } else {
            statusHandler.error("unrecognized appName: " + appName);
        }
        return tblRowData;
    }

    /**
     * Gets Fog Zone Table Row Data
     * 
     * @param algCellType
     * @return
     */
    public TableRowData getFogZoneTableRowData(CellType algCellType) {
        updateFogWorstValues();
        return TableUtil.getFogTableRowData(zone, zone, worstValues,
                thresholdMgr, algCellType);
    }

    /**
     * Gets SAFESEAS Zone Table Row Data
     * 
     * @param algCellType
     *            : Type of ALG cell in the table
     * @return
     */
    public TableRowData getSSZoneTableRowData(CellType algCellType) {
        updateSafeseasWorstValues();
        return TableUtil.getSafeseasTableRowData(zone, zone, worstValues,
                thresholdMgr, algCellType);
    }

    /**
     * Updates Fog Worst Values
     */
    private void updateFogWorstValues() {
        if (zoneHourReports.isEmpty()) {
            return;
        }
        InitWorstValues();
        for (String station : zoneHourReports.keySet()) {
            ObReport report = zoneHourReports.get(station).getLatestObReport();
            if (report != null) {
                if (report.getVisibility() != ObConst.MISSING) {
                    worstValues
                            .setVisibility(worstValues.getVisibility() == ObConst.MISSING ? report
                                    .getVisibility() : Math.min(
                                    worstValues.getVisibility(),
                                    report.getVisibility()));
                }

                worstValues.setPresentWx(worstValues.getPresentWx()
                        + report.getPresentWx());

                if (report.getCeiling() != ObConst.MISSING) {
                    worstValues
                            .setCeiling(worstValues.getCeiling() == ObConst.MISSING ? report
                                    .getCeiling() : Math.min(
                                    worstValues.getCeiling(),
                                    report.getCeiling()));
                }

                if (report.getWindDir() != ObConst.MISSING) {
                    worstValues
                            .setWindDir(worseDirection(
                                    MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_FROM
                                            .getXmlKey(),
                                    MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_TO
                                            .getXmlKey(), worstValues
                                            .getWindDir(), report.getWindDir()));
                }

                if (report.getWindSpeed() != ObConst.MISSING) {
                    worstValues
                            .setWindSpeed(worstValues.getWindSpeed() == ObConst.MISSING ? report
                                    .getWindSpeed() : Math.max(
                                    worstValues.getWindSpeed(),
                                    report.getWindSpeed()));
                }

                if (report.getMaxWindSpeed() != ObConst.MISSING) {
                    worstValues
                            .setMaxWindSpeed(worstValues.getMaxWindSpeed() == ObConst.MISSING ? report
                                    .getMaxWindSpeed() : Math.max(
                                    worstValues.getMaxWindSpeed(),
                                    report.getMaxWindSpeed()));
                }

                if (report.getWindGust() != ObConst.MISSING) {
                    worstValues
                            .setWindGust(worstValues.getWindGust() == ObConst.MISSING ? report
                                    .getWindGust() : Math.max(
                                    worstValues.getWindGust(),
                                    report.getWindGust()));
                }

                if (report.getTemperature() != ObConst.MISSING) {
                    worstValues
                            .setTemperature(worstValues.getTemperature() == ObConst.MISSING ? report
                                    .getTemperature() : Math.max(
                                    worstValues.getTemperature(),
                                    report.getTemperature()));
                }

                if (report.getDewpoint() != ObConst.MISSING) {
                    worstValues
                            .setDewpoint(worstValues.getDewpoint() == ObConst.MISSING ? report
                                    .getDewpoint() : Math.max(
                                    worstValues.getDewpoint(),
                                    report.getDewpoint()));
                }

                if (report.getDewpointDepr() != ObConst.MISSING) {
                    worstValues
                            .setDewpointDepr(worstValues.getDewpointDepr() == ObConst.MISSING ? report
                                    .getDewpointDepr() : Math.min(
                                    worstValues.getDewpointDepr(),
                                    report.getDewpointDepr()));
                }

                if (report.getRelativeHumidity() != ObConst.MISSING) {
                    worstValues.setRelativeHumidity(worstValues
                            .getRelativeHumidity() == ObConst.MISSING ? report
                            .getRelativeHumidity() : Math.max(
                            worstValues.getRelativeHumidity(),
                            report.getRelativeHumidity()));
                }
            }
        }
    }

    /**
     * Updates SNOW Worst Values
     */
    private void updateSnowWorstValues() {
        if (zoneHourReports.isEmpty()) {
            return;
        }
        InitWorstValues();
        for (String station : zoneHourReports.keySet()) {
            ObReport report = zoneHourReports.get(station).getLatestObReport();
            if (report != null) {
                worstValues.setPresentWx(worstValues.getPresentWx()
                        + report.getPresentWx());

                if (report.getWindDir() != ObConst.MISSING) {
                    worstValues
                            .setWindDir(worseDirection(
                                    MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_FROM
                                            .getXmlKey(),
                                    MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_TO
                                            .getXmlKey(), worstValues
                                            .getWindDir(), report.getWindDir()));
                }

                if (report.getWindSpeed() != ObConst.MISSING) {
                    worstValues
                            .setWindSpeed(worstValues.getWindSpeed() == ObConst.MISSING ? report
                                    .getWindSpeed() : Math.max(
                                    worstValues.getWindSpeed(),
                                    report.getWindSpeed()));
                }

                if (report.getMaxWindSpeed() != ObConst.MISSING) {
                    worstValues
                            .setMaxWindSpeed(worstValues.getMaxWindSpeed() == ObConst.MISSING ? report
                                    .getMaxWindSpeed() : Math.max(
                                    worstValues.getMaxWindSpeed(),
                                    report.getMaxWindSpeed()));
                }

                if (report.getWindGust() != ObConst.MISSING) {
                    worstValues
                            .setWindGust(worstValues.getWindGust() == ObConst.MISSING ? report
                                    .getWindGust() : Math.max(
                                    worstValues.getWindGust(),
                                    report.getWindGust()));
                }

                if (report.getTemperature() != ObConst.MISSING) {
                    worstValues
                            .setTemperature(worstValues.getTemperature() == ObConst.MISSING ? report
                                    .getTemperature() : Math.min(
                                    worstValues.getTemperature(),
                                    report.getTemperature()));
                }

                if (report.getDewpoint() != ObConst.MISSING) {
                    worstValues
                            .setDewpoint(worstValues.getDewpoint() == ObConst.MISSING ? report
                                    .getDewpoint() : Math.min(
                                    worstValues.getDewpoint(),
                                    report.getDewpoint()));
                }

                if (report.getVisibility() != ObConst.MISSING) {
                    worstValues
                            .setVisibility(worstValues.getVisibility() == ObConst.MISSING ? report
                                    .getVisibility() : Math.min(
                                    worstValues.getVisibility(),
                                    report.getVisibility()));
                }

                if (report.getSeaLevelPress() != ObConst.MISSING) {
                    worstValues
                            .setSeaLevelPress(worstValues.getSeaLevelPress() == ObConst.MISSING ? report
                                    .getSeaLevelPress() : Math.min(
                                    worstValues.getSeaLevelPress(),
                                    report.getSeaLevelPress()));
                }

                if (report.getWindChill() != ObConst.MISSING) {
                    worstValues
                            .setWindChill(worstValues.getWindChill() == ObConst.MISSING ? report
                                    .getWindChill() : Math.min(
                                    worstValues.getWindChill(),
                                    report.getWindChill()));
                }

                if (report.getFrostbiteTime() != ObConst.MISSING) {
                    worstValues
                            .setFrostbiteTime(worstValues.getFrostbiteTime() == ObConst.MISSING ? report
                                    .getFrostbiteTime() : Math.min(
                                    worstValues.getFrostbiteTime(),
                                    report.getFrostbiteTime()));
                }

                if (report.getHourlyPrecip() != ObConst.MISSING) {
                    worstValues
                            .setHourlyPrecip(worstValues.getHourlyPrecip() == ObConst.MISSING ? report
                                    .getHourlyPrecip() : Math.max(
                                    worstValues.getHourlyPrecip(),
                                    report.getHourlyPrecip()));
                }

                if (report.getSnowDepth() != ObConst.MISSING) {
                    worstValues
                            .setSnowDepth(worstValues.getSnowDepth() == ObConst.MISSING ? report
                                    .getSnowDepth() : Math.max(
                                    worstValues.getSnowDepth(),
                                    report.getSnowDepth()));
                }

                if (report.getSnincrHourly() != ObConst.MISSING) {
                    worstValues
                            .setSnincrHourly(worstValues.getSnincrHourly() == ObConst.MISSING ? report
                                    .getSnincrHourly() : Math.max(
                                    worstValues.getSnincrHourly(),
                                    report.getSnincrHourly()));
                }

                if (report.getSnincrTotal() != ObConst.MISSING) {
                    worstValues
                            .setSnincrTotal(worstValues.getSnincrTotal() == ObConst.MISSING ? report
                                    .getSnincrTotal() : Math.max(
                                    worstValues.getSnincrTotal(),
                                    report.getSnincrTotal()));
                }
            }
        }
    }

    /**
     * Updates SAFESEAS Worst Values
     */
    private void updateSafeseasWorstValues() {
        if (zoneHourReports.isEmpty()) {
            return;
        }
        InitWorstValues();
        for (String station : zoneHourReports.keySet()) {
            ObReport report = zoneHourReports.get(station).getLatestObReport();
            if (report != null) {
                if (report.getWindDir() != ObConst.MISSING) {
                    worstValues
                            .setWindDir(worstValues.getWindDir() == ObConst.MISSING ? report
                                    .getWindDir()
                                    : worseDirection(
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_TO
                                                    .getXmlKey(), worstValues
                                                    .getWindDir(), report
                                                    .getWindDir()));
                }

                if (report.getWindSpeed() != ObConst.MISSING) {
                    worstValues
                            .setWindSpeed(worstValues.getWindSpeed() == ObConst.MISSING ? report
                                    .getWindSpeed() : Math.max(
                                    worstValues.getWindSpeed(),
                                    report.getWindSpeed()));
                }

                if (report.getMaxWindSpeed() != ObConst.MISSING) {
                    worstValues
                            .setMaxWindSpeed(worstValues.getMaxWindSpeed() == ObConst.MISSING ? report
                                    .getMaxWindSpeed() : Math.max(
                                    worstValues.getMaxWindSpeed(),
                                    report.getMaxWindSpeed()));
                }

                if (report.getWindGust() != ObConst.MISSING) {
                    worstValues
                            .setWindGust(worstValues.getWindGust() == ObConst.MISSING ? report
                                    .getWindGust() : Math.max(
                                    worstValues.getWindGust(),
                                    report.getWindGust()));
                }

                if (report.getVisibility() != ObConst.MISSING) {
                    worstValues
                            .setVisibility(worstValues.getVisibility() == ObConst.MISSING ? report
                                    .getVisibility() : Math.min(
                                    worstValues.getVisibility(),
                                    report.getVisibility()));
                }

                if (report.getTemperature() != ObConst.MISSING) {
                    worstValues
                            .setTemperature(worstValues.getTemperature() == ObConst.MISSING ? report
                                    .getTemperature() : Math.max(
                                    worstValues.getTemperature(),
                                    report.getTemperature()));
                }

                if (report.getDewpoint() != ObConst.MISSING) {
                    worstValues
                            .setDewpoint(worstValues.getDewpoint() == ObConst.MISSING ? report
                                    .getDewpoint() : Math.max(
                                    worstValues.getDewpoint(),
                                    report.getDewpoint()));
                }

                if (report.getSeaLevelPress() != ObConst.MISSING) {
                    worstValues
                            .setSeaLevelPress(worstValues.getSeaLevelPress() == ObConst.MISSING ? report
                                    .getSeaLevelPress() : Math.min(
                                    worstValues.getSeaLevelPress(),
                                    report.getSeaLevelPress()));
                }

                if (report.getSeaSurfaceTemp() != ObConst.MISSING) {
                    worstValues.setSeaSurfaceTemp(worstValues
                            .getSeaSurfaceTemp() == ObConst.MISSING ? report
                            .getSeaSurfaceTemp() : Math.max(
                            worstValues.getSeaSurfaceTemp(),
                            report.getSeaSurfaceTemp()));
                }

                if (report.getHighResWaveHeight() != ObConst.MISSING) {
                    worstValues.setHighResWaveHeight(worstValues
                            .getHighResWaveHeight() == ObConst.MISSING ? report
                            .getHighResWaveHeight() : Math.max(
                            worstValues.getHighResWaveHeight(),
                            report.getHighResWaveHeight()));
                }

                if (report.getWaveSteepness() != ObConst.MISSING) {
                    worstValues
                            .setWaveSteepness(worstValues.getWaveSteepness() == ObConst.MISSING ? report
                                    .getWaveSteepness() : Math.max(
                                    worstValues.getWaveSteepness(),
                                    report.getWaveSteepness()));
                }

                if (report.getPSwellHeight() != ObConst.MISSING) {
                    worstValues
                            .setPSwellHeight(worstValues.getPSwellHeight() == ObConst.MISSING ? report
                                    .getPSwellHeight() : Math.max(
                                    worstValues.getPSwellHeight(),
                                    report.getPSwellHeight()));
                }

                if (report.getPSwellPeriod() != ObConst.MISSING) {
                    worstValues
                            .setPSwellPeriod(worstValues.getPSwellPeriod() == ObConst.MISSING ? report
                                    .getPSwellPeriod() : Math.max(
                                    worstValues.getPSwellPeriod(),
                                    report.getPSwellPeriod()));
                }

                if (report.getPSwellDir() != ObConst.MISSING) {
                    worstValues
                            .setPSwellDir(worstValues.getPSwellDir() == ObConst.MISSING ? report
                                    .getPSwellDir()
                                    : worseDirection(
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO
                                                    .getXmlKey(), worstValues
                                                    .getPSwellDir(), report
                                                    .getPSwellDir()));
                }

                if (report.getSSwellHeight() != ObConst.MISSING) {
                    worstValues
                            .setSSwellHeight(worstValues.getSSwellHeight() == ObConst.MISSING ? report
                                    .getSSwellHeight() : Math.max(
                                    worstValues.getSSwellHeight(),
                                    report.getSSwellHeight()));
                }

                if (report.getSSwellPeriod() != ObConst.MISSING) {
                    worstValues
                            .setSSwellPeriod(worstValues.getSSwellPeriod() == ObConst.MISSING ? report
                                    .getSSwellPeriod() : Math.max(
                                    worstValues.getSSwellPeriod(),
                                    report.getSSwellPeriod()));
                }

                if (report.getSSwellDir() != ObConst.MISSING) {
                    worstValues
                            .setSSwellDir(worstValues.getSSwellDir() == ObConst.MISSING ? report
                                    .getSSwellDir()
                                    : worseDirection(
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO
                                                    .getXmlKey(), worstValues
                                                    .getSSwellDir(), report
                                                    .getSSwellDir()));
                }
            }
        }
    }

    /**
     * Returns the ObStnHourReports object of a caller-specified station. If
     * such object not available, returns null.
     * 
     * @param station
     * @return
     */
    public ObStnHourReports getObStnHourReports(String station) {
        if (!zoneHourReports.containsKey(station)) {
            return null;
        }
        return zoneHourReports.get(station);
    }

    /**
     * Gets Zone Hour Reports
     * 
     * @return
     */
    public Map<String, ObStnHourReports> getZoneHourReports() {
        return zoneHourReports;
    }

    /**
     * Gets Nominal Time
     * 
     * @return
     */
    public Date getNominalTime() {
        return nominalTime;
    }

    /**
     * Gets Zone
     * 
     * @return
     */
    public String getZone() {
        return zone;
    }

    /**
     * Gets AppName
     * 
     * @return
     */
    public CommonConfig.AppName getAppName() {
        return appName;
    }

}
