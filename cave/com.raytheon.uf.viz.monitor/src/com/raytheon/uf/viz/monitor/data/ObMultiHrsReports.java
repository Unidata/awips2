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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager.MonName;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;

/**
 * This class is a container of ObHourReports objects; it contains all available
 * obs reports for the most recent MAX_FRAMES hours for the office's monitoring
 * area. (This class corresponds to the MA32HrsReports c++ class in AWIPS-1).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec. 1, 2009  3424       zhao       Initial creation.
 * Dec 24, 2009  3424       zhao       added getTrendDataSet() that returns ObTrendDataSet object
 * Jan 25, 2010  4281, 3888, 3877 wkwock/zhao added getHistTableData method
 * Oct 31, 2012  1297      skorolev    Clean code.
 * Jan 29, 2013 15654      zhao        add Wind Chill calculation for SNOW 
 * Sep 04, 2014  3220      skorolev    Updated getStationTableData method.
 * Sep 25, 2015  3873      skorolev    Added multiHrsTabData.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */

public class ObMultiHrsReports {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ObMultiHrsReports.class);

    /**
     * Thresholds manager
     */
    private AbstractThresholdMgr thresholdMgr;

    /**
     * Fog ALG CellType [key is zone, value is cell type]
     */
    private Map<String, CellType> fogAlgCellType;

    /**
     * application name (snow, fog, safeseas, etc)
     */
    private CommonConfig.AppName appName;

    /**
     * FSSObs records cache. Key is nominal time, value is ObHourReports object
     */
    private SortedMap<Date, ObHourReports> multiHrsReports = new TreeMap<Date, ObHourReports>();

    /**
     * Monitor Table data cache. Key is nominal time, value is TableData
     */
    private ConcurrentHashMap<Date, TableData> multiHrsTabData = new ConcurrentHashMap<Date, TableData>();

    private int maxFrames = ObConst.MAX_FRAMES;

    private FSSObsMonitorConfigurationManager cfgMgr = null;

    /**
     * Constructor
     * 
     * @param appName
     */
    public ObMultiHrsReports(CommonConfig.AppName appName) {
        this.appName = appName;

        if (appName.equals(AppName.FOG) || appName.equals(AppName.SAFESEAS)) {
            if (appName.equals(AppName.FOG)) {
                cfgMgr = FSSObsMonitorConfigurationManager
                        .getInstance(MonName.fog);
            } else if (appName.equals(AppName.SAFESEAS)) {
                cfgMgr = FSSObsMonitorConfigurationManager
                        .getInstance(MonName.ss);
            }
            initFogAlgCellType();
        }
    }

    /**
     * Adds an ObReport object to the ObMultiHrsReports object
     * 
     * @param report
     * @return returns multiHrsReports
     */
    public void addReport(ObReport report) {
        Date nominalTime = report.getRefHour();
        /**
         * DR #8723: if wind speed is zero, wind direction should be N/A, not 0
         */
        if (report.getWindSpeed() < 0.0001) { // zero wind speed
            report.setWindDir(ObConst.MISSING);
        }
        /**
         * DR#11462: set DewpointDepr now, avoid using DewpointDepr = worstTemp
         * - worstDewpoint for Zone/County Table, which causes mismatch between
         * Zone/County table and Station table
         */
        if (report.getTemperature() != ObConst.MISSING
                && report.getDewpoint() != ObConst.MISSING) {
            report.setDewpointDepr(report.getTemperature()
                    - report.getDewpoint());
        }
        /**
         * DR15654: set Wind Chill for SNOW
         */
        if (appName == AppName.SNOW) {
            if (report.getTemperature() != ObConst.MISSING
                    && report.getWindSpeed() != ObConst.MISSING) {
                report.setWindChill(calcWindChill(report.getTemperature(),
                        report.getWindSpeed()));
            }
        }
        ObHourReports obHourReports;
        // new nominal time; create a new ObHourReports object
        if (multiHrsReports.isEmpty()
                || !multiHrsReports.containsKey(nominalTime)) {
            obHourReports = new ObHourReports(nominalTime, appName,
                    thresholdMgr);
        } else {
            // the map is full; delete the oldest entry
            if (multiHrsReports.size() >= maxFrames) {
                multiHrsReports.remove(multiHrsReports.firstKey());
            }
            // update multiHrsReports with new data
            obHourReports = multiHrsReports.get(nominalTime);
        }
        obHourReports.addReport(report);
        // update data cache
        multiHrsReports.put(nominalTime, obHourReports);
        TableData tblData = obHourReports.getZoneTableData();
        multiHrsTabData.put(nominalTime, tblData);
    }

    /**
     * DR 15654: Wind Chill calculation formula based on
     * http://www.nws.noaa.gov/om/windchill/ as of Jan. 29, 2013
     * 
     * @param temp
     *            in degree F
     * @param windSpd
     *            in knots
     * @return wind chill in degree F
     */
    private float calcWindChill(float temp, float windSpd) {
        if (temp > 50.0 || windSpd < 3.0) {
            return ObConst.MISSING;
        }
        /**
         * 1 knots = 1.15078 mph
         */
        float spd = (float) Math.pow(1.15078 * windSpd, 0.16);
        return 35.74f + 0.6215f * temp - 35.75f * spd + 0.4275f * temp * spd;
    }

    /**
     * Returns a zone TableData object of the latest nominal time. If no data
     * available (the map is empty), returns an empty zone TableData object
     * (table cells filled with "N/A").
     * 
     * @return
     */
    public TableData getZoneTableData() {
        if (multiHrsReports.isEmpty()) {
            return getEmptyZoneTableData();
        }
        return getZoneTableData(multiHrsReports.lastKey());
    }

    /**
     * Returns a zone TableData object for a caller-specified nominal-time. If
     * no data available, returns an empty/default zone TableData object (table
     * cells filled with "N/A"). Updates multiHrsTabData table cache.
     * 
     * @param nominalTime
     * @return
     */
    public TableData getZoneTableData(Date nominalTime) {
        TableData tabData = null;
        if (nominalTime == null || !multiHrsReports.containsKey(nominalTime)) {
            return getEmptyZoneTableData();
        }
        if (appName == AppName.FOG) {
            tabData = this.getObHourReports(nominalTime).getFogZoneTableData(
                    fogAlgCellType);
        } else if (appName == AppName.SAFESEAS) {
            tabData = this.getObHourReports(nominalTime).getSSZoneTableData(
                    fogAlgCellType);

        } else {
            tabData = this.getObHourReports(nominalTime).getZoneTableData();
        }
        // update table data cache
        if (multiHrsTabData.replace(nominalTime, tabData) == null) {
            multiHrsTabData.put(nominalTime, tabData);
        }
        return tabData;
    }

    /**
     * Returns an empty zone TableData object (table cells filled with "N/A").
     * 
     * @return
     */
    public TableData getEmptyZoneTableData() {
        Date nominalTime = TableUtil.getNominalTime(SimulatedTime
                .getSystemTime().getTime());
        ObHourReports hourReports = new ObHourReports(nominalTime, appName,
                thresholdMgr);
        TableData tabData = null;
        if (appName == AppName.FOG) {
            tabData = hourReports.getFogZoneTableData(fogAlgCellType);
        } else {
            tabData = hourReports.getZoneTableData();
        }
        // update data cache
        multiHrsReports.put(nominalTime, hourReports);
        // update cache with empty table data
        if (multiHrsTabData.replace(nominalTime, tabData) == null) {
            multiHrsTabData.put(nominalTime, tabData);
        }
        return tabData;
    }

    /**
     * Returns the station TableData object for the latest nominal time. If no
     * data available, an empty/default station TableData object is returned
     * 
     * @param zone
     * @return
     */
    public TableData getStationTableData(String zone) {
        if (multiHrsReports.isEmpty()) {
            return getEmptyStationTableData(zone);
        }
        return getStationTableData(multiHrsReports.lastKey(), zone);
    }

    /**
     * Returns a station TableData object for a caller-specified nominal-time
     * and zone ID. If no data available, an empty/default station TableData
     * object is returned.
     * 
     * @param nominalTime
     * @param zone
     * @return
     */
    public TableData getStationTableData(Date nominalTime, String zone) {
        if (zone.equals("")) {
            return this.getEmptyZoneTableData();
        }
        if (nominalTime == null) {
            return getEmptyStationTableData(zone);
        }
        return this.getObHourReports(nominalTime).getObZoneHourReports(zone)
                .getStationTableData();
    }

    /**
     * Gets data for station table
     * 
     * @param zone
     * @return station table data
     */
    public TableData getEmptyStationTableData(String zone) {
        Date nominalTime = TableUtil.getNominalTime(SimulatedTime
                .getSystemTime().getTime());
        ObZoneHourReports zoneHourReports = new ObZoneHourReports(nominalTime,
                zone, appName, thresholdMgr);
        return zoneHourReports.getStationTableData();
    }

    /**
     * Gets data for trend plots
     * 
     * @param zone
     * @param Station
     * @param varName
     * @param productName
     * @return ObTrendDataSet object, or null if no data available
     */
    public ObTrendDataSet getTrendDataSet(String zone, String station,
            ObConst.VarName varName, ObConst.ProductName productName) {

        if (multiHrsReports.isEmpty()) {
            return null;
        }
        // get data
        ObTrendDataSet trendData = new ObTrendDataSet(zone, varName,
                productName, appName, thresholdMgr);

        Iterator<Date> nominalTimeIterator = multiHrsReports.keySet()
                .iterator();
        Date start = findStartNominalTime();
        while (nominalTimeIterator.hasNext()) {
            Date nominalTime = nominalTimeIterator.next();
            if (nominalTime.compareTo(start) >= 0) {
                ObStnHourReports stnHrRpts = this.getObHourReports(nominalTime)
                        .getObZoneHourReports(zone)
                        .getObStnHourReports(station);
                if (stnHrRpts != null) {
                    Set<Date> obsTimes = stnHrRpts.getObsTimes();
                    if (obsTimes != null) {
                        for (Date obsTime : obsTimes) {
                            trendData.addDataPoint(obsTime, new Float(stnHrRpts
                                    .getObReport(obsTime).get(varName)));
                        }
                    } else {
                        continue;
                    }
                }
            }
        }
        return trendData;
    }

    /**
     * Gets History Table Data
     * 
     * @param zone
     *            : current zone
     * @param station
     *            : station ID
     * @param obsType
     *            : ObsHistType
     * @return
     */
    public TableData getHistTableData(String zone, String station,
            ObsHistType obsType) {

        TableData tblData = new TableData(appName);
        if (multiHrsReports.isEmpty()) {
            return tblData;
        }
        ArrayList<TableRowData> tblRows = new ArrayList<TableRowData>();
        Iterator<Date> nominalTimeIterator = multiHrsReports.keySet()
                .iterator();
        Date start = findStartNominalTime();
        while (nominalTimeIterator.hasNext()) {
            Date nominalTime = nominalTimeIterator.next();
            if (nominalTime.compareTo(start) >= 0) {
                ObStnHourReports stnHrRpts = this.getObHourReports(nominalTime)
                        .getObZoneHourReports(zone)
                        .getObStnHourReports(station);
                if (stnHrRpts != null) {
                    Set<Date> obsTimes = stnHrRpts.getObsTimes();
                    if (obsTimes != null) {
                        for (Date obsTime : obsTimes) {
                            ObReport report = stnHrRpts.getObReport(obsTime);
                            tblRows.add(TableUtil.getHistTableRowData(appName,
                                    obsType, report));
                        }
                    } else {
                        continue;
                    }
                }
            }
        }

        /**
         * sort first column descending in obs time
         */
        ArrayList<TableRowData> tblRows2 = new ArrayList<TableRowData>();
        for (int i = 0; i < tblRows.size(); i++) {
            tblRows2.add(tblRows.get(tblRows.size() - 1 - i));
        }
        tblData.setTableRows(tblRows2);
        return tblData;
    }

    private Date findStartNominalTime() {
        // Trend plot for the past 24 hours.
        // Instead of using present time as the latest time,
        // here we use the latest nominal time as the latest time
        // and get data within 24 hours before the latest nominal time
        // [probably present time should be used as the latest time for trending
        // plots-- fix this later]
        Date latestNominalTime = multiHrsReports.lastKey();
        // determine trending start nominal time
        Date startNominalTime = multiHrsReports.firstKey();
        // startNominalTime must be within 24 hours before latestNominalTime
        long diff = (latestNominalTime.getTime() - startNominalTime.getTime())
                / TimeUtil.MILLIS_PER_HOUR;
        // difference in hours between the two dates
        if (diff > 24) {
            // find the startNominalTime
            Calendar cal = Calendar.getInstance();
            cal.setTime(latestNominalTime);
            cal.add(Calendar.DAY_OF_YEAR, -1);
            // expected, but may not exist in multiHrsReports
            Date expectedStartNominalTime = cal.getTime();
            if (multiHrsReports.containsKey(expectedStartNominalTime)) {
                startNominalTime = expectedStartNominalTime;
            } else {
                // this iterator is ordered since multiHrsReports is a sorted
                // map
                Iterator<Date> iterator = multiHrsReports.keySet().iterator();
                while (iterator.hasNext()) {
                    Date nominalTime = iterator.next();
                    if (nominalTime.compareTo(expectedStartNominalTime) >= 0) {
                        startNominalTime = nominalTime;
                        break;
                    }
                }
            }
        }
        return startNominalTime;
    }

    /**
     * Gets table cache
     * 
     * @return
     */
    public ConcurrentHashMap<Date, TableData> getMultiHrsTabData() {
        return multiHrsTabData;
    }

    /**
     * Sets table cache
     * 
     * @param multiHrsTabData
     */
    public void setMultiHrsTabData(
            ConcurrentHashMap<Date, TableData> multiHrsTabData) {
        this.multiHrsTabData = multiHrsTabData;
    }

    /**
     * Gets data cache
     * 
     * @return SortedMap object <nominal time, ObHourReports object>
     */
    public SortedMap<Date, ObHourReports> getMultiHrsReports() {
        return multiHrsReports;
    }

    /**
     * Sets data cache
     * 
     * @param multiHrsReports
     */
    public void setMultiHrsReports(
            SortedMap<Date, ObHourReports> multiHrsReports) {
        this.multiHrsReports = multiHrsReports;
    }

    /**
     * Gets the Latest NominalTime Returns the latest nominal time if the map is
     * not empty; otherwise, returns the nominal time of the present date-time
     * 
     * @return
     */
    public Date getLatestNominalTime() {
        Date latestNominalTime = null;
        if (multiHrsReports.isEmpty()) {
            latestNominalTime = TableUtil.getNominalTime(SimulatedTime
                    .getSystemTime().getTime());
        } else {
            latestNominalTime = multiHrsReports.lastKey();
        }
        return latestNominalTime;
    }

    /**
     * Gets Nominal Times
     * 
     * @return a set of nominal times
     */
    public Set<Date> getNominalTimes() {
        return multiHrsReports.keySet();
    }

    /**
     * Gets ObHourReports Returns the ObHourReports object of the latest nominal
     * time. If no data available, returns an empty ObHourReports object.
     * 
     * @return
     */
    public ObHourReports getObHourReports() {
        if (multiHrsReports.isEmpty()) {
            ObHourReports obHrsReps = new ObHourReports(
                    TableUtil.getNominalTime(SimulatedTime.getSystemTime()
                            .getTime()), appName, thresholdMgr);
            // Save table data cache.
            Date refTm = obHrsReps.getNominalTime();
            TableData tabData = obHrsReps.getZoneTableData();
            multiHrsTabData.clear();
            multiHrsTabData.put(refTm, tabData);
            return obHrsReps;
        }
        return multiHrsReports.get(multiHrsReports.lastKey());
    }

    /**
     * Gets ObHourReports Returns an ObHourReports object of a caller-specified
     * nominal time. If no data available, returns an empty ObHourReports
     * object.
     * 
     * @param nominalTime
     * @return
     */
    public ObHourReports getObHourReports(Date nominalTime) {
        if (nominalTime == null || !multiHrsReports.containsKey(nominalTime)) {
            return new ObHourReports(TableUtil.getNominalTime(SimulatedTime
                    .getSystemTime().getTime()), appName, thresholdMgr);
        }

        return multiHrsReports.get(nominalTime);
    }

    /**
     * Gets application name
     * 
     * @return application name
     */
    public CommonConfig.AppName getAppName() {
        return appName;
    }

    /**
     * Set the Threshold manager
     * 
     * @param thresholdMgr
     */
    public void setThresholdMgr(AbstractThresholdMgr thresholdMgr) {
        this.thresholdMgr = thresholdMgr;
    }

    /**
     * Gets Threshold Manager
     * 
     * @return the threshold manager
     */
    public AbstractThresholdMgr getThresholdMgr() {
        return thresholdMgr;
    }

    /**
     * Set the Fog ALG CellType map
     * 
     * @param fogAlgCellType2
     */
    public void setFogAlgCellType(Map<String, CellType> fogAlgCellType2) {
        this.fogAlgCellType = fogAlgCellType2;
    }

    /**
     * Gets map of types for ALG cell
     * 
     * @return fogAlgCellType
     */
    public Map<String, CellType> getFogAlgCellType() {
        return fogAlgCellType;
    }

    /**
     * Initiates ALG cells for Fog table. Sets all as NotAvailable.
     */
    private void initFogAlgCellType() {
        fogAlgCellType = new HashMap<String, CellType>();
        List<String> zones = cfgMgr.getAreaList();
        Iterator<String> itr = zones.iterator();
        while (itr.hasNext()) {
            fogAlgCellType.put(itr.next(), CellType.NotAvailable);
        }
        setFogAlgCellType(fogAlgCellType);
    }

    /**
     * Updates table cache
     */
    public void updateTableCache() {
        for (Date time : multiHrsReports.keySet()) {
            getZoneTableData(time);
        }
    }
}
