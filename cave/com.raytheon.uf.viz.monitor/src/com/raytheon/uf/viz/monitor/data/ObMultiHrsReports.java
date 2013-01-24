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
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.ObsHistType;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.time.SimulatedTime;
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
 * Oct.31, 2012  1297       skorolev    Clean code.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */

public class ObMultiHrsReports {

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
     * key is nominal time, value is ObHourReports object
     */
    private SortedMap<Date, ObHourReports> multiHrsReports;

    /**
     * The maximum number of most recent hours within which observation reports
     * are to be archived. TODO: move MAX_FRAMES to a configuration file?
     */
    private final int MAX_FRAMES = 64;

    private int maxFrames = MAX_FRAMES;

    /**
     * Constructor
     * 
     * @param appName
     */
    public ObMultiHrsReports(CommonConfig.AppName appName) {
        this.appName = appName;
        multiHrsReports = new TreeMap<Date, ObHourReports>();
        if (appName.equals(AppName.FOG) || appName.equals(AppName.SAFESEAS)) {
            initFogAlgCellType();
        }
    }

    /**
     * Add an array of ObReport objects to the ObMultiHrsReports object (Don't
     * use! VK)
     * 
     * @param result
     */
    public void addReports(ObReport[] results) {
        for (ObReport report : results) {
            /**
             * DR #8723: if wind speed is zero, wind direction should be N/A,
             * not 0
             */
            if (report.getWindSpeed() < 0.0001) { // zero wind speed
                report.setWindDir(ObConst.MISSING);
            }
            addReport(report);
        }
    }

    /**
     * Adds an ObReport object to the ObMultiHrsReports object
     * 
     * @param report
     * @return returns multiHrsReports
     */
    public void addReport(ObReport report) {
        // Date nominalTime = TableUtil
        // .getNominalTime(report.getObservationTime());
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
        if (multiHrsReports.containsKey(nominalTime)) {
            multiHrsReports.get(nominalTime).addReport(report);
        } else {
            // new nominal time; create a new ObHourReports object
            if (multiHrsReports.size() >= maxFrames) {
                // the map is full; delete the oldest entry
                multiHrsReports.remove(multiHrsReports.firstKey());
            }
            ObHourReports obHourReports = new ObHourReports(nominalTime,
                    appName, thresholdMgr);
            obHourReports.addReport(report);
            multiHrsReports.put(nominalTime, obHourReports);
        }
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
     * cells filled with "N/A").
     * 
     * @param nominalTime
     * @return
     */
    public TableData getZoneTableData(Date nominalTime) {
        if (nominalTime == null || !multiHrsReports.containsKey(nominalTime)) {
            return getEmptyZoneTableData();
        }
        if (appName == AppName.FOG) {
            return this.getObHourReports(nominalTime).getFogZoneTableData(
                    fogAlgCellType);
        }
        if (appName == AppName.SAFESEAS) {
            return this.getObHourReports(nominalTime).getSSZoneTableData(
                    fogAlgCellType);
        }

        return this.getObHourReports(nominalTime).getZoneTableData();
        // return multiHrsReports.get(nominalTime).getZoneTableData();
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
        if (appName == AppName.FOG) {
            return hourReports.getFogZoneTableData(fogAlgCellType);
        }
        return hourReports.getZoneTableData();
    }

    /**
     * Returns the station TableData object for the latest nominal time. If no
     * data available, an empty/default station TableData object is returned
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
     */
    public TableData getStationTableData(Date nominalTime, String zone) {
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
     * @return ObTrendDataSet object, or null if no data available
     */
    public ObTrendDataSet getTrendDataSet(String zone, String station,
            ObConst.VarName varName, ObConst.ProductName productName) {

        if (multiHrsReports.isEmpty()) {
            return null;
        }

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
                / (60 * 60 * 60); // difference in hour between the two dates
        if (diff > 24) {
            // find the startNominalTime
            Calendar cal = Calendar.getInstance();
            cal.setTime(latestNominalTime);
            cal.add(Calendar.DAY_OF_YEAR, -1);
            Date expectedStartNominalTime = cal.getTime(); // expected, but may
                                                           // not exist in
                                                           // multiHrsReports
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

        // get data
        ObTrendDataSet trendData = new ObTrendDataSet(zone, varName,
                productName, appName, thresholdMgr);
        // trendData.setThresholdMgr(thresholdMgr);
        Iterator<Date> nominalTimeIterator = multiHrsReports.keySet()
                .iterator();
        while (nominalTimeIterator.hasNext()) {
            Date nominalTime = nominalTimeIterator.next();
            if (nominalTime.compareTo(startNominalTime) >= 0) {
                Set<Date> obsTimes = this.getObHourReports(nominalTime)
                        .getObZoneHourReports(zone)
                        .getObStnHourReports(station).getObsTimes();
                if (obsTimes != null) {
                    for (Date obsTime : obsTimes) {
                        trendData.addDataPoint(obsTime,
                                new Float(this.getObHourReports(nominalTime)
                                        .getObZoneHourReports(zone)
                                        .getObStnHourReports(station)
                                        .getObReport(obsTime).get(varName)));
                    }
                }
            }
        }

        return trendData;
    }

    /**
     * 
     * @param station
     *            station ID
     * @param obsType
     *            ObsHistType
     * @return TableData object for obs history table
     */
    public TableData getHistTableData(String zone, String station,
            ObsHistType obsType) {

        TableData tblData = new TableData(appName);
        if (multiHrsReports.isEmpty()) {
            return tblData;
        }

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
                / (60 * 60 * 60); // difference in hour between the two dates
        if (diff > 24) {
            // find the startNominalTime
            Calendar cal = Calendar.getInstance();
            cal.setTime(latestNominalTime);
            cal.add(Calendar.DAY_OF_YEAR, -1);
            Date expectedStartNominalTime = cal.getTime(); // expected, but may
                                                           // not exist in
                                                           // multiHrsReports
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

        // get data
        ArrayList<TableRowData> tblRows = new ArrayList<TableRowData>();
        Iterator<Date> nominalTimeIterator = multiHrsReports.keySet()
                .iterator();
        while (nominalTimeIterator.hasNext()) {
            Date nominalTime = nominalTimeIterator.next();
            if (nominalTime.compareTo(startNominalTime) >= 0) {
                Set<Date> obsTimes = this.getObHourReports(nominalTime)
                        .getObZoneHourReports(zone)
                        .getObStnHourReports(station).getObsTimes();
                if (obsTimes != null) {
                    for (Date obsTime : obsTimes) {
                        ObReport report = getObHourReports(nominalTime)
                                .getObZoneHourReports(zone)
                                .getObStnHourReports(station)
                                .getObReport(obsTime);

                        tblRows.add(TableUtil.getHistTableRowData(appName,
                                obsType, report));
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

    /**
     * Returns a SortedMap object <nominal time, ObHourReports object>
     * 
     * @return multiHrsReports
     */
    public SortedMap<Date, ObHourReports> getMultiHrsReports() {
        return multiHrsReports;
    }

    /**
     * Returns a SortedMap object (key is nominal time, value is zone TableData
     * object)
     * 
     * @return
     */
    public SortedMap<Date, TableData> getMultiHrsTableData() {
        SortedMap<Date, TableData> multiHrsTblData = new TreeMap<Date, TableData>();
        if (appName == AppName.FOG) {
            for (Date nominalTime : multiHrsReports.keySet()) {
                multiHrsTblData.put(
                        nominalTime,
                        multiHrsReports.get(nominalTime).getFogZoneTableData(
                                fogAlgCellType));
            }
            return multiHrsTblData;
        }
        for (Date nominalTime : multiHrsReports.keySet()) {
            multiHrsTblData.put(nominalTime, multiHrsReports.get(nominalTime)
                    .getZoneTableData());
        }
        return multiHrsTblData;
    }

    /**
     * Returns the latest nominal time if the map is not empty; otherwise,
     * returns the nominal time of the present date-time
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
     * Returns a set of nominal times
     * 
     * @return
     */
    public Set<Date> getNominalTimes() {
        return multiHrsReports.keySet();
    }

    /**
     * Returns the ObHourReports object of the latest nominal time. If no data
     * available, returns an empty ObHourReports object.
     * 
     * @return
     */
    public ObHourReports getObHourReports() {
        if (multiHrsReports.isEmpty()) {
            return new ObHourReports(TableUtil.getNominalTime(SimulatedTime
                    .getSystemTime().getTime()), appName, thresholdMgr);
        }
        return multiHrsReports.get(multiHrsReports.lastKey());
    }

    /**
     * Returns an ObHourReports object of a caller-specified nominal time. If no
     * data available, returns an empty ObHourReports object.
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
        Set<String> zones = MonitoringArea.getPlatformMap().keySet();
        Iterator<String> itr = zones.iterator();
        while (itr.hasNext()) {
            fogAlgCellType.put(itr.next(), CellType.NotAvailable);
        }
        setFogAlgCellType(fogAlgCellType);
    }
}
