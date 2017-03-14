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
package com.raytheon.viz.ghg.monitor.data;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.ghg.monitor.IGhgSelectedTableColumn;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;

/**
 * This class contains a row of data in the GHG table. The data consists of the
 * GHG data and all of the color, font, and sorting information.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 10 Apr 2014  15769      ryu         Changed isTestData() due to move of identifyTestEvents
 *                                     to config data.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgTableRowData implements Comparable<GhgTableRowData> {
    private static final int MILLIS_PER_MINUTE = 1000 * 60;

    private static final int[] dateIndices = { DataEnum.START.ordinal(),
            DataEnum.END.ordinal(), DataEnum.PURGE.ordinal(),
            DataEnum.ISSUE_TIME.ordinal() };

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "HH:mm'Z' dd-MMM-yy");

    /**
     * Table item containing the text displayed in the table and the cell
     * colors.
     */
    private TableItem tableItem;

    /**
     * GHG data that will be displayed in the table.
     */
    private GhgData ghgData;

    /**
     * Table that displays the data.
     */
    private Table parentTable;

    /**
     * Table item font.
     */
    private Font tableItemFont;

    /**
     * Callback used to get the selected table column.
     */
    private IGhgSelectedTableColumn tableCallback;

    /**
     * Flag to determine if the row is selected.
     */
    private boolean selected = false;

    /**
     * Flag to determine if the row is selected via map click.
     */
    private boolean mapSelected = false;

    /**
     * Constructor.
     * 
     * @param parentTable
     *            Parent table.
     * @param ghgData
     *            GHG data displayed in a table row.
     * @param selTableColumnCb
     *            Selected table column callback.
     * @param fontData
     */
    public GhgTableRowData(Table parentTable, GhgData ghgData,
            IGhgSelectedTableColumn selTableColumnCb, FontData fontData) {

        this.ghgData = ghgData;
        tableCallback = selTableColumnCb;
        this.parentTable = parentTable;
        this.setMapSelected(ghgData.getSelection() == SelectionEnum.MapSelection);
        this.setSelected(ghgData.getSelection() == SelectionEnum.MonitorSelection);
        tableItemFont = new Font(this.parentTable.getParent().getDisplay(),
                fontData);

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        tableItem = new TableItem(parentTable, SWT.NONE);
        tableItem.setFont(tableItemFont);
        tableItem.setText(ghgData.getDataCellNames());
    }

    /**
     * Get the table item.
     * 
     * @return The table item.
     */
    public TableItem getTableItem() {
        return tableItem;
    }

    public void setTableItem(TableItem tableItem) {
        this.tableItem = tableItem;
    }

    /**
     * Dispose of the table item.
     */
    public void disposeTableItem() {
        tableItem.dispose();
    }

    /**
     * Set the table item font.
     * 
     * @param fontData
     *            Font information.
     */
    public void setTableItemFont(FontData fontData) {
        if (tableItemFont != null) {
            tableItemFont.dispose();
        }

        tableItemFont = new Font(parentTable.getParent().getDisplay(), fontData);
        tableItem.setFont(tableItemFont);
    }

    /**
     * Regenerate the table item.
     */
    public void regenerateTableItem() {
        tableItem.dispose();

        tableItem = new TableItem(parentTable, SWT.NONE);
        tableItem.setFont(tableItemFont);
        tableItem.setText(ghgData.getDataCellNames());

        setHighlight();
    }

    /**
     * Set the standard colors. Check for alert status.
     * 
     * @param tItem
     * @param index
     */
    public void setRegularColors(TableItem tItem, int index, GhgData record) {
        List<Integer> visibleCols = tableCallback.getVisibleColumnIndexes();
        Display display = parentTable.getDisplay();

        GhgConfigData config = GhgConfigData.getInstance();

        // Get the alert color if it exists
        GhgColorData cellColor = getAlertColor(record);

        // No alert color, use regular entries color
        if (cellColor == null) {
            cellColor = config.getRegularEntriesColors();
        }

        Color fgColor = new Color(display, cellColor.getForegroundRgb());
        Color bgColor = new Color(display, cellColor.getBackgroundRgb());

        for (int i = 0; i < visibleCols.size(); i++) {
            tItem.setBackground(visibleCols.get(i), bgColor);
            tItem.setForeground(visibleCols.get(i), fgColor);
        }

        fgColor.dispose();
        bgColor.dispose();
    }

    /**
     * Determine how to highlight the row.
     */
    public void setHighlight() {
        boolean alertFlag = false;
        List<Integer> visibleCols = tableCallback.getVisibleColumnIndexes();
        Display display = parentTable.getDisplay();

        List<GhgColorData> colorList = new ArrayList<GhgColorData>();

        GhgColorData cellColor = null;
        GhgConfigData config = GhgConfigData.getInstance();

        /* Determine what colors are appropriate for highlighting */

        // Get the alert color if it exists
        cellColor = getAlertColor(ghgData);

        // alert status, add the alert color to the list
        if (cellColor != null) {
            colorList.add(cellColor);
            alertFlag = true;
        }

        // Add selection state color to the color list
        if (isMapSelected()) {
            cellColor = config.getMapSelectionsColors();
            colorList.add(cellColor);
            selected = false;
            mapSelected = true;
        } else if (isSelected()) {
            cellColor = config.getMonitorSelectionsColors();
            colorList.add(cellColor);
            selected = true;
            mapSelected = false;
        } else {
            if (alertFlag == false) {
                cellColor = config.getRegularEntriesColors();
                colorList.add(cellColor);
            }
            selected = false;
        }

        if (isTestData()) {
            // test products with VTEC "T" codes get "Test" highlighting
            String vtecStr = ghgData.getVtecString();
            if (vtecStr.substring(1, 2).equals("T")) {
                cellColor = config.getTestProductsColors();
                colorList.add(cellColor);
            }
        }

        Color fgColor = null;
        Color bgColor = null;
        int colorIdx = 0;
        for (int i = 0; i < visibleCols.size(); i++) {
            fgColor = new Color(display, colorList.get(colorIdx)
                    .getForegroundRgb());
            bgColor = new Color(display, colorList.get(colorIdx)
                    .getBackgroundRgb());
            tableItem.setForeground(visibleCols.get(i), fgColor);
            tableItem.setBackground(visibleCols.get(i), bgColor);
            colorIdx++;
            if (colorIdx >= colorList.size()) {
                colorIdx = 0;
            }

            fgColor.dispose();
            bgColor.dispose();
        }
    }

    /**
     * Gets alert color for the appropriate alert.
     * 
     * @param record
     *            The GhgData
     * @return GhgColorData object, null if no alert status
     */
    private GhgColorData getAlertColor(GhgData record) {
        GhgConfigData config = GhgConfigData.getInstance();
        GhgAlertsConfigData alerts = config.getAlerts();

        if (alerts == null) {
            return null;
        }

        int currentMinute = (int) (SimulatedTime.getSystemTime().getTime()
                .getTime() / MILLIS_PER_MINUTE);

        // NATIONAL WEATHER SERVICE INSTRUCTION 10-1703
        GhgAlertData alert1 = alerts.getAlert(AlertsEnum.AlertLvl1);
        int minutesToAlert1 = alert1.getTime();

        GhgAlertData alert2 = alerts.getAlert(AlertsEnum.AlertLvl2);
        int minutesToAlert2 = alert2.getTime();

        Date endDate = record.getEndDate();
        int purgeTimeMinutes = (int) (endDate.getTime() / MILLIS_PER_MINUTE);
        int deltaMinP = purgeTimeMinutes - currentMinute;
        if (deltaMinP <= 0) {
            return config.getExpiredAlertColors();
        } else if (deltaMinP <= minutesToAlert2) {
            return config.getAlertLvl2Colors();
        } else if (deltaMinP <= minutesToAlert1) {
            return config.getAlertLvl1Colors();
        }

        return null;
    }

    /**
     * Compare method use for sorting the table data.
     * 
     * @param obj
     *            Other object to compare to.
     * @return -1 for less than, 0 for equal to, and 1 for greater than.
     */
    public int compareTo(GhgTableRowData obj) {
        GhgTableRowData otherObject = obj;

        int selectedIndex = tableCallback.getSelectedColumn();

        int x;
        if (Arrays.binarySearch(dateIndices, selectedIndex) >= 0) {
            Date thisDate = null;
            Date otherDate = null;
            DataEnum fieldEnum = DataEnum.values()[selectedIndex];
            switch (fieldEnum) {
            case START:
                thisDate = ghgData.getStartDate();
                otherDate = otherObject.ghgData.getStartDate();
                break;
            case END:
                thisDate = ghgData.getEndDate();
                otherDate = otherObject.ghgData.getEndDate();
                break;
            case PURGE:
                thisDate = ghgData.getPurgeDate();
                otherDate = otherObject.ghgData.getPurgeDate();
                break;
            case ISSUE_TIME:
                thisDate = ghgData.getIssueTime();
                otherDate = otherObject.ghgData.getIssueTime();
                break;
            default:
                throw new IllegalArgumentException("Column " + selectedIndex
                        + " is in dateIndices, but is not a date column.");
            }
            x = thisDate.compareTo(otherDate);
        } else if (selectedIndex == DataEnum.ETN.ordinal()) {
            Integer thisETN = Integer.valueOf("0"
                    + ghgData.getDataCellNames()[selectedIndex].trim());
            Integer otherETN = Integer.valueOf("0"
                    + otherObject.ghgData.getDataCellNames()[selectedIndex]
                            .trim());
            x = thisETN.compareTo(otherETN);
        } else {
            String thisData = (ghgData.getDataCellNames())[selectedIndex];
            String otherData = (otherObject.ghgData.getDataCellNames())[selectedIndex];

            x = thisData.compareTo(otherData);
        }

        if (x == 0) {
            x = ghgData.compareTo(otherObject.ghgData);
        }

        if (parentTable.getSortDirection() == SWT.DOWN) {
            if (x < 0) {
                return 1;
            } else if (x > 0) {
                return -1;
            }
        }

        return x;
    }

    public GhgData getGhgData() {
        return ghgData;
    }

    public void setGhgData(GhgData ghgData) {
        this.ghgData = ghgData;
    }

    /**
     * @return the selected
     */
    public boolean isSelected() {
        return selected;
    }

    /**
     * @param selected
     *            the selected to set
     */
    public void setSelected(boolean selected) {
        this.selected = selected;
        if (selected == true) {
            mapSelected = false;
        }
    }

    /**
     * @return the mapSelected
     */
    public boolean isMapSelected() {
        return mapSelected;
    }

    public void setMapSelected(boolean mapSelected) {
        this.mapSelected = mapSelected;
        if (mapSelected == true) {
            selected = false;
        }
    }

    /**
     * @return the testData
     */
    public boolean isTestData() {
        return GhgConfigData.getInstance().isIdentifyTestEvents();
    }
}