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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;

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

    private HashMap<String, HashMap<ScanTables, List<ScheduledAlarms>>> scheduledAlarmsMap;

    private HashMap<String, HashMap<ScanTables, List<AlertedAlarms>>> alertedAlarmsMap;

    private HashMap<String, HashMap<ScanTables, boolean[][]>> indicesMap;

    private HashMap<String, HashMap<ScanTables, List<Point>>> toReset;

    private HashMap<String, HashMap<ScanTables, List<String>>> idents;

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
            HashMap<ScanTables, List<ScheduledAlarms>> siteScheduledAlarmsMap = new HashMap<ScanTables, List<ScheduledAlarms>>();
            siteScheduledAlarmsMap.put(ScanTables.DMD,
                    new CopyOnWriteArrayList<ScheduledAlarms>());
            siteScheduledAlarmsMap.put(ScanTables.CELL,
                    new CopyOnWriteArrayList<ScheduledAlarms>());
            scheduledAlarmsMap.put(site, siteScheduledAlarmsMap);
        }

        if (!alertedAlarmsMap.containsKey(site)) {
            HashMap<ScanTables, List<AlertedAlarms>> siteAlertedAlarmsMap = new HashMap<ScanTables, List<AlertedAlarms>>();
            siteAlertedAlarmsMap.put(ScanTables.DMD,
                    new CopyOnWriteArrayList<AlertedAlarms>());
            siteAlertedAlarmsMap.put(ScanTables.CELL,
                    new CopyOnWriteArrayList<AlertedAlarms>());
            alertedAlarmsMap.put(site, siteAlertedAlarmsMap);
        }

        if (!indicesMap.containsKey(site)) {
            HashMap<ScanTables, boolean[][]> siteIndicesMap = new HashMap<ScanTables, boolean[][]>();
            indicesMap.put(site, siteIndicesMap);
        }

        if (!toReset.containsKey(site)) {
            HashMap<ScanTables, List<Point>> siteToReset = new HashMap<ScanTables, List<Point>>();
            siteToReset.put(ScanTables.CELL, new ArrayList<Point>());
            siteToReset.put(ScanTables.DMD, new ArrayList<Point>());
            toReset.put(site, siteToReset);
        }

        if (!idents.containsKey(site)) {
            HashMap<ScanTables, List<String>> siteIdents = new HashMap<ScanTables, List<String>>();
            siteIdents.put(ScanTables.CELL, new ArrayList<String>());
            siteIdents.put(ScanTables.DMD, new ArrayList<String>());
            idents.put(site, siteIdents);
        }
    }

    private SCANAlarmAlertManager() {

        if (scheduledAlarmsMap == null) {
            scheduledAlarmsMap = new HashMap<String, HashMap<ScanTables, List<ScheduledAlarms>>>();
        }

        if (alertedAlarmsMap == null) {
            alertedAlarmsMap = new HashMap<String, HashMap<ScanTables, List<AlertedAlarms>>>();
        }

        if (indicesMap == null) {
            indicesMap = new HashMap<String, HashMap<ScanTables, boolean[][]>>();
        }

        if (toReset == null) {
            toReset = new HashMap<String, HashMap<ScanTables, List<Point>>>();
        }

        if (idents == null) {
            idents = new HashMap<String, HashMap<ScanTables, List<String>>>();
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
            ScanDataGenerator sdg) {
        int colLength = 0;
        if (tableType == ScanTables.CELL) {
            colLength = SCANConfigEnums.CELLTable.values().length;
        } else if (tableType == ScanTables.DMD) {
            colLength = SCANConfigEnums.DMDTable.values().length;
        }

        String site = sdg.getSite();
        clearAlertedAlarms(site, tableType);

        if (tableType == ScanTables.CELL || tableType == ScanTables.DMD) {
            indicesMap.get(sdg.getSite()).put(tableType,
                    new boolean[data.getTableRows().size()][colLength]);
        }
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
                                alarm.colName, AlarmType.AbsVal, row, index);
                        indicesMap.get(site).get(tableType)[row][index] = true;
                    } else if (indicesMap.get(site).get(tableType)[row][index] != true) {
                        indicesMap.get(site).get(tableType)[row][index] = false;
                    }
                    row++;
                }
            } else if (tableType != ScanTables.DMD
                    && alarm.type == AlarmType.RateOfChange) {
                ScanMonitor monitor = ScanMonitor.getInstance();

                if (monitor.cellData != null) {
                    if (monitor.cellData.containsKey(site)) {
                        Set<Date> cellData = monitor.cellData.get(site)
                                .keySet();
                        Date previous = null;
                        if (!cellData.isEmpty()) {
                            Date[] times = cellData.toArray(new Date[cellData
                                    .size()]);
                            if (times.length > 1) {
                                Arrays.sort(times);
                                previous = times[times.length - 2];
                                ScanTableData<?> tableDataPrev = monitor
                                        .getTableData(tableType, site, previous);
                                getScanTableDiff(data, tableDataPrev,
                                        tableType, colLength, sdg, alarm,
                                        config);
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
            ScanDataGenerator sdg, ScheduledAlarms alarm, SCANConfig config) {
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
                                        alarm.colName, alarm.type, row, i);
                                // set the blinkability = true
                                indicesMap.get(site).get(tableType)[row][i] = true;
                            }
                            // don't want to overwrite any current alarms
                            else if (indicesMap.get(site).get(tableType)[row][i] != true) {
                                indicesMap.get(site).get(tableType)[row][i] = false;
                            }
                        }
                        // don't want to overwrite any current alarms
                        else if (indicesMap.get(site).get(tableType)[row][i] != true) {
                            indicesMap.get(site).get(tableType)[row][i] = false;
                        }
                    }
                }
            }
            row++;
        }
    }

    /**
     * return the indices of blinkable elements (boolean[][])
     * 
     * @return
     */
    public boolean[][] getIndices(String site, ScanTables tableType) {
        if (indicesMap.get(site).get(tableType) == null) {
            return new boolean[0][0];
        }
        return indicesMap.get(site).get(tableType);
    }

    /**
     * Remove the alarm and set the indices to false
     * 
     * @param alarm
     */
    public void removeAlarm(String site, ScanTables tableType,
            AlertedAlarms alarm) {
        alertedAlarmsMap.get(site).get(tableType).remove(alarm);
        indicesMap.get(site).get(tableType)[alarm.row][alarm.col] = false;
        toReset.get(site).get(tableType).add(new Point(alarm.row, alarm.col));
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
                    && alarm.type == theAlarm.type) {
                if (alarm.type == AlarmType.RateOfChange
                        && tableType == ScanTables.DMD) {
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
            String ident, String colName, AlarmType type, int row, int col) {
        alertedAlarmsMap.get(site).get(tableType)
                .add(new AlertedAlarms(ident, colName, type, row, col));
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
    public List<AlertedAlarms> getAlertedAlarms(String site,
            ScanTables tableType) {
        return alertedAlarmsMap.get(site).get(tableType);
    }

    public boolean containsAlarm(String site, ScanTables tableType,
            AlarmType type, String colName, String ident) {
        for (AlertedAlarms alarm : alertedAlarmsMap.get(site).get(tableType)) {
            if (ident.equals(alarm.ident) && type == alarm.type
                    && colName.equals(alarm.colName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns the points to reset
     * 
     * @return
     */
    public List<Point> getPointsToReset(String site, ScanTables tableType) {
        return toReset.get(site).get(tableType);
    }

    public void clearToReset(String site, ScanTables tableType) {
        toReset.get(site).get(tableType).clear();
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
     * clear the alerted alarms and the indices
     */
    public void clearAlertedAlarms(String site, ScanTables tableType) {
        for (AlertedAlarms alarm : alertedAlarmsMap.get(site).get(tableType)) {
            alertedAlarmsMap.get(site).get(tableType).remove(alarm);
            indicesMap.get(site).get(tableType)[alarm.row][alarm.col] = false;
            toReset.get(site).get(tableType)
                    .add(new Point(alarm.row, alarm.col));
        }
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

        public AlertedAlarms(String ident, String colName, AlarmType type,
                int row, int col) {
            this.ident = ident;
            this.colName = colName;
            this.type = type;
            this.row = row;
            this.col = col;
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
                    + this.row + "\nCol #:" + this.col;
        }
    }
}
