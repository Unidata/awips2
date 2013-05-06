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
package com.raytheon.uf.viz.monitor.scan.tables;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.swt.SWT;

import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.data.ScanDataGenerator;

/**
 * Manager class to hold between scan dialog and alarming capabilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2010            mnash     Initial creation
 * 
 * 03/15/2012	13939	   Mike Duff    For a SCAN Alarms issue
 * Apr 18, 2013   1926    njensen      Update for Long keys
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SCANAlarmAlertManager {

    public static enum AlarmType {
        AbsVal("Abs"), RateOfChange("ROC");

        private String name;

        AlarmType(String text) {
            this.name = text;
        }

        public String getName() {
            return this.name;
        }
    }

    private static SCANAlarmAlertManager instance = null;

    private Map<String, Map<ScanTables, List<ScheduledAlarms>>> scheduledAlarmsMap;

    private Map<String, Map<ScanTables, Set<AlertedAlarms>>> alertedAlarmsMap;

    private Map<String, Map<ScanTables, List<String>>> idents;

    private boolean ring = false;

    /**
     * @return the instance
     */
    public static SCANAlarmAlertManager getInstance(String site) {
        if (instance == null) {
            instance = new SCANAlarmAlertManager();
        }

        instance.addSite(site);

        return instance;
    }

    public void addSite(String site) {

        if (!scheduledAlarmsMap.containsKey(site)) {
            Map<ScanTables, List<ScheduledAlarms>> siteScheduledAlarmsMap = Collections
                    .synchronizedMap(new HashMap<ScanTables, List<ScheduledAlarms>>());
            siteScheduledAlarmsMap.put(ScanTables.DMD,
                    new CopyOnWriteArrayList<ScheduledAlarms>());
            siteScheduledAlarmsMap.put(ScanTables.CELL,
                    new CopyOnWriteArrayList<ScheduledAlarms>());
            scheduledAlarmsMap.put(site, siteScheduledAlarmsMap);
        }

        if (!alertedAlarmsMap.containsKey(site)) {
            Map<ScanTables, Set<AlertedAlarms>> siteAlertedAlarmsSet = Collections
                    .synchronizedMap(new HashMap<ScanTables, Set<AlertedAlarms>>());
            siteAlertedAlarmsSet.put(ScanTables.DMD,
                    new HashSet<AlertedAlarms>());
            siteAlertedAlarmsSet.put(ScanTables.CELL,
                    new HashSet<AlertedAlarms>());
            alertedAlarmsMap.put(site, siteAlertedAlarmsSet);
        }

        if (!idents.containsKey(site)) {
            Map<ScanTables, List<String>> siteIdents = new HashMap<ScanTables, List<String>>();
            siteIdents.put(ScanTables.CELL, new ArrayList<String>());
            siteIdents.put(ScanTables.DMD, new ArrayList<String>());
            idents.put(site, siteIdents);
        }
    }

    private SCANAlarmAlertManager() {

        if (scheduledAlarmsMap == null) {
            scheduledAlarmsMap = Collections
                    .synchronizedMap(new HashMap<String, Map<ScanTables, List<ScheduledAlarms>>>());
        }

        if (alertedAlarmsMap == null) {
            alertedAlarmsMap = Collections
                    .synchronizedMap(new HashMap<String, Map<ScanTables, Set<AlertedAlarms>>>());
        }

        if (idents == null) {
            idents = new HashMap<String, Map<ScanTables, List<String>>>();
        }

    }

    /**
     * Calculate if the cells need to blink as an alarm is going off
     * 
     * @param data
     * @param tableType
     * @param sdg
     */
    public void calculateScanCells(SCANTableData data, ScanTables tableType,
            ScanDataGenerator sdg, Date latestTime) {
        int colLength = 0;
        if (tableType == ScanTables.CELL) {
            colLength = SCANConfigEnums.CELLTable.values().length;
        } else if (tableType == ScanTables.DMD) {
            colLength = SCANConfigEnums.DMDTable.values().length;
        }

        String site = sdg.getSite();
        clearOldAlarms(site, tableType, latestTime);

        SCANConfig config = SCANConfig.getInstance();

        for (ScheduledAlarms alarm : scheduledAlarmsMap.get(site)
                .get(tableType)) {
            int index = config.getColumnIndex(tableType, alarm.colName);
            if (index == -1) {
                continue;
            }
            int row = 0;
            int sortDir = config.getSortDirection(tableType, alarm.colName);
            if (alarm.type == AlarmType.AbsVal) {
                for (SCANTableRowData scanData : data.getTableRows()) {
                    if (!idents
                            .get(site)
                            .get(tableType)
                            .contains(
                                    scanData.getTableCellData(0).getCellText())) {
                        addIdent(site, tableType, scanData.getTableCellData(0)
                                .getCellText());
                    } else if (containsAlarm(site, tableType, alarm.type,
                            alarm.colName, scanData.getTableCellData(0)
                                    .getCellText())) {
                        continue;
                    }
                    boolean valCompare = false;
                    if (sortDir == SWT.DOWN) {
                        valCompare = scanData.getTableCellData(index)
                                .getValueAsDouble() >= alarm.value;
                    } else {
                        valCompare = scanData.getTableCellData(index)
                                .getValueAsDouble() <= alarm.value;
                    }
                    if (valCompare) {
                        addAlertedAlarm(site, tableType, scanData
                                .getTableCellData(0).getCellText(),
                                alarm.colName, AlarmType.AbsVal, row, index,
                                latestTime);
                    }
                    row++;
                }
            } else if ((tableType != ScanTables.DMD)
                    && (alarm.type == AlarmType.RateOfChange)) {
                ScanMonitor monitor = ScanMonitor.getInstance();

                if (monitor.cellData != null) {
                    if (monitor.cellData.containsKey(site)) {
                        Set<Long> cellData = monitor.cellData.get(site)
                                .keySet();
                        Date previous = null;
                        if (!cellData.isEmpty()) {
                            Date[] times = new Date[cellData.size()];
                            Iterator<Long> itr = cellData.iterator();
                            for (int i = 0; i < times.length; i++) {
                                times[i] = new Date(itr.next());
                            }
                            if (times.length > 1) {
                                Arrays.sort(times);
                                previous = times[times.length - 2];
                                ScanTableData<?> tableDataPrev = monitor
                                        .getTableData(tableType, site, previous);
                                getScanTableDiff(data, tableDataPrev,
                                        tableType, colLength, sdg, alarm,
                                        config, latestTime);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Will determine the rate of change and set the correct values into the
     * table for alarms
     * 
     * @param data
     * @param otherData
     * @param tableType
     * @param length
     * @param sdg
     * @param alarm
     */
    public void getScanTableDiff(SCANTableData data,
            ScanTableData<?> otherData, ScanTables tableType, int length,
            ScanDataGenerator sdg, ScheduledAlarms alarm, SCANConfig config,
            Date latestTime) {
        SCANTableData scanOtherData = null;
        if (tableType == ScanTables.CELL) {
            scanOtherData = sdg.generateCellData(otherData);
        } else if (tableType == ScanTables.DMD) {
            scanOtherData = sdg.generateDMDData(otherData);
        } else if (tableType == ScanTables.MESO) {
            scanOtherData = sdg.generateMesoData(otherData);
        } else if (tableType == ScanTables.TVS) {
            scanOtherData = sdg.generateTVSData(otherData);
        }

        int row = 0;
        int sortDir = config.getSortDirection(tableType, alarm.colName);
        String site = sdg.getSite();
        // starting at the first row on the current table
        for (SCANTableRowData currentRow : data.getTableRows()) {
            if (!idents.get(site).get(tableType)
                    .contains(currentRow.getTableCellData(0).getCellText())) {
                addIdent(site, tableType, currentRow.getTableCellData(0)
                        .getCellText());
            } else if (containsAlarm(site, tableType, alarm.type,
                    alarm.colName, currentRow.getTableCellData(0).getCellText())) {
                continue;
            }
            // grab the whole row
            SCANTableCellData[] currentTableCells = currentRow
                    .getTableCellDataArray();
            // grab the first row on the previous table
            for (SCANTableRowData previousRow : scanOtherData.getTableRows()) {
                // grab the whole row
                SCANTableCellData[] previousTableCells = previousRow
                        .getTableCellDataArray();

                // see if the idents are equal ([0])
                if (currentTableCells[0].getCellText().equals(
                        previousTableCells[0].getCellText())) {
                    // move through the cells by column
                    for (int i = 0; i < currentRow.getNumberOfCellData(); i++) {
                        // check if the columns equal the current alarm being
                        // tested
                        if (currentTableCells[i].getColumnName().equals(
                                alarm.colName)) {
                            // if columns are equal to alarm being tested, grab
                            // values from both tables run test
                            boolean valCompare = false;
                            if (sortDir == SWT.DOWN) {
                                valCompare = (currentTableCells[i]
                                        .getValueAsDouble() - previousTableCells[i]
                                        .getValueAsDouble()) > alarm.value;
                            } else {
                                valCompare = (currentTableCells[i]
                                        .getValueAsDouble() - previousTableCells[i]
                                        .getValueAsDouble()) < alarm.value;
                            }
                            if (valCompare) {
                                // add alarm
                                addAlertedAlarm(site, tableType,
                                        currentTableCells[0].getCellText(),
                                        alarm.colName, alarm.type, row, i,
                                        latestTime);
                            }
                        }
                    }
                }
            }
            row++;
        }
    }

    /**
     * Remove the alarm and set the indices to false
     * 
     * @param alarm
     */
    public void clearAlarm(String site, ScanTables tableType,
            AlertedAlarms alarm) {
        Set<AlertedAlarms> alarms = alertedAlarmsMap.get(site).get(tableType);
        for (AlertedAlarms aa : alarms) {
            if (alarm.ident.equalsIgnoreCase(aa.ident)
                    && alarm.colName.equalsIgnoreCase(aa.colName)
                    && (alarm.type == aa.type) && (alarm.row == aa.row)) {
                aa.cleared = true;
                break;
            }
        }
    }

    /**
     * Check the list for the current alarm, and update
     * 
     * @param name
     * @param type
     * @param value
     * @param bell
     */
    public void updateScheduledAlarm(String site, ScanTables tableType,
            String name, AlarmType type, int value) {
        ScheduledAlarms alarm = new ScheduledAlarms(name, type, value);
        for (ScheduledAlarms theAlarm : scheduledAlarmsMap.get(site).get(
                tableType)) {
            if (alarm.colName.equals(theAlarm.colName)
                    && (alarm.type == theAlarm.type)) {
                if ((alarm.type == AlarmType.RateOfChange)
                        && (tableType == ScanTables.DMD)) {
                    return;
                } else {
                    scheduledAlarmsMap.get(site).get(tableType)
                            .remove(theAlarm);
                    scheduledAlarmsMap.get(site).get(tableType).add(alarm);
                    return;
                }
            } else {
                continue;
            }
        }
        scheduledAlarmsMap.get(site).get(tableType).add(alarm);
    }

    /**
     * Add alarm to the scheduled list
     * 
     * @param name
     * @param type
     * @param value
     * @param bell
     */
    public void addScheduledAlarm(String site, ScanTables tableType,
            String name, AlarmType type, int value) {
        // set time to zero as this only happens once
        ScheduledAlarms alarm = new ScheduledAlarms(name, type, value);
        if (!scheduledAlarmsMap.get(site).get(tableType).contains(alarm)) {
            scheduledAlarmsMap.get(site).get(tableType).add(alarm);
        }
    }

    /**
     * Add alarm to the alerted list
     * 
     * @param ident
     * @param colName
     * @param type
     * @param row
     * @param col
     */
    public void addAlertedAlarm(String site, ScanTables tableType,
            String ident, String colName, AlarmType type, int row, int col,
            Date validTime) {
        Set<AlertedAlarms> alarms = alertedAlarmsMap.get(site).get(tableType);
        if (alarms.size() == 0) {
            alarms.add(new AlertedAlarms(ident, colName, type, row, col,
                    validTime));
            return;
        }
        for (AlertedAlarms alarm : alarms) {
            if (alarm.ident.equalsIgnoreCase(ident)
                    && alarm.colName.equalsIgnoreCase(colName)
                    && (alarm.type == type) && (alarm.row == row)) {
                if (alarm.cleared) {
                    break;
                }
            } else {
                alarms.add(new AlertedAlarms(ident, colName, type, row, col,
                        validTime));
                break;
            }
        }
    }

    /**
     * return the scheduled alarms
     * 
     * @return
     */
    public List<ScheduledAlarms> getScheduledAlarms(String site,
            ScanTables tableType) {
        return scheduledAlarmsMap.get(site).get(tableType);
    }

    /**
     * return the alerted alarms
     * 
     * @return
     */
    public Set<AlertedAlarms> getAlertedAlarms(String site, ScanTables tableType) {
        return alertedAlarmsMap.get(site).get(tableType);
    }

    public int getAlertedAlarmCount(String site, ScanTables tableType) {
        int count = 0;
        for (AlertedAlarms alarm : alertedAlarmsMap.get(site).get(tableType)) {
            if (!alarm.cleared) {
                count++;
            }
        }

        return count;
    }

    public boolean containsAlarm(String site, ScanTables tableType,
            AlarmType type, String colName, String ident) {
        for (AlertedAlarms alarm : alertedAlarmsMap.get(site).get(tableType)) {
            if (ident.equals(alarm.ident) && (type == alarm.type)
                    && colName.equals(alarm.colName)) {
                return true;
            }
        }
        return false;
    }

    public boolean isRing() {
        return ring;
    }

    public void setRing(boolean ring) {
        this.ring = ring;
    }

    public void addIdent(String site, ScanTables tableType, String ident) {
        idents.get(site).get(tableType).add(ident);
    }

    public void clearIdents(String site, ScanTables tableType) {
        idents.get(site).get(tableType).clear();
    }

    /**
     * Remove any old alarms
     * 
     * @param site
     * @param type
     */
    private void clearOldAlarms(String site, ScanTables type, Date latestTime) {
        List<AlertedAlarms> clearList = new ArrayList<AlertedAlarms>();
        for (AlertedAlarms alarm : alertedAlarmsMap.get(site).get(type)) {
            if (latestTime.getTime() > alarm.validTime.getTime()) {
                clearList.add(alarm);
            }
        }

        for (AlertedAlarms alarm : clearList) {
            alertedAlarmsMap.get(site).get(type).remove(alarm);
        }
    }

    /**
     * clear the alerted alarms and the indices
     */
    public void clearAlertedAlarms(String site, ScanTables tableType) {
        for (AlertedAlarms alarm : alertedAlarmsMap.get(site).get(tableType)) {
            alarm.cleared = true;
        }
    }

    public void removeAlertedAlarms(String site, ScanTables tableType) {
        alertedAlarmsMap.get(site).get(tableType).clear();
        setRing(false);
    }

    /**
     * Clear the scheduled alarms
     */
    public void clearScheduledAlarms(String site, ScanTables tableType) {
        scheduledAlarmsMap.get(site).get(tableType).clear();
    }

    /**
     * Scheduled alarms are created in the dialog and held in memory.
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Dec 3, 2010            mnash     Initial creation
     * 
     * </pre>
     * 
     * @author mnash
     * @version 1.0
     */
    public class ScheduledAlarms {

        String colName;

        AlarmType type;

        int value;

        public ScheduledAlarms(String colName, AlarmType type, int value) {
            this.colName = colName;
            this.type = type;
            this.value = value;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Column : " + this.colName + "\nType : "
                    + this.type.getName() + "\nValue : " + this.value;
        }
    }

    /**
     * 
     * Alerted alarms are built from the scheduled alarms. These alarms have no
     * timer, they just hold state on what cells in the table are currently
     * alerting.
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Dec 3, 2010            mnash     Initial creation
     * 
     * </pre>
     * 
     * @author mnash
     * @version 1.0
     */
    public class AlertedAlarms {
        String ident;

        String colName;

        AlarmType type;

        int row;

        int col;

        boolean cleared = false;

        Date validTime;

        public AlertedAlarms(String ident, String colName, AlarmType type,
                int row, int col, Date validTime) {
            this.ident = ident;
            this.colName = colName;
            this.type = type;
            this.row = row;
            this.col = col;
            this.validTime = validTime;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Identifier : " + this.ident + "\nColumn : " + this.colName
                    + "\nType : " + this.type.getName() + "\nRow #: "
                    + this.row + "\nCol #:" + this.col + "\nCleared: "
                    + cleared + "\n";
        }
    }

}
