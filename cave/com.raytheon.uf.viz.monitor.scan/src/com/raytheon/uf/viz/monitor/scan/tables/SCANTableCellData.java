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

import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.CELLTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.MESOTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanThresholdColor;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.TVSTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.WARN_TYPE;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;

/**
 * 
 * Class containing information about the data in a single table cell in the
 * CELL, DMD, MESO, or TVS table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANTableCellData {
    /**
     * Text in the table cell.
     */
    private String cellText = null;

    /**
     * Numerical value.
     */
    private Double value = Double.NaN;

    /**
     * Non-numeric value.
     */
    private String strVal = "";

    /**
     * Flag indicating the value is of type integer.
     */
    private boolean displayAsInt = false;

    /**
     * Table cell background color.
     */
    private Color backgroundColor;

    /**
     * Name of the column.
     */
    private final String columnName;

    private String shortText = null;

    /**
     * Suffix on the data.
     */
    private String suffix = null;

    private String leftJustified = "%-20S";

    private String rightJustified = "%20S";

    private Pattern p = Pattern.compile("[0-9.]+");

    /**
     * This variable is used to give "weight" to the different suffix
     * characters. This is used for sorting and will be combined with the value.
     */
    private double suffixWeightedValue = 0.0;

    /**
     * SCAN table identifier.
     */
    private final ScanTables scanTable;

    /**
     * Instance of the SCAN configuration.
     */
    private final SCANConfig scanCfg = SCANConfig.getInstance();

    private WARN_TYPE warnType = null;

    private Color foregroundColor = SCANConfig.getInstance()
            .getCellForegroundColor();

    private boolean isCounty = false;

    /**
     * Constructor.
     * 
     * @param scanTable
     *            SCAN table identifier.
     * @param columnName
     *            Name of the column.
     * @param value
     *            Data as a double.
     */
    public SCANTableCellData(ScanTables scanTable, String columnName,
            double value) {
        this.scanTable = scanTable;
        this.columnName = columnName;
        this.value = value;
        this.setColor();
        displayAsInt = scanCfg.displayAsIntTable(scanTable, columnName);
    }

    /**
     * Constructor.
     * 
     * @param scanTable
     *            SCAN table identifier.
     * @param columnName
     *            Name of the column.
     * @param value
     *            Data as a double.
     */
    public SCANTableCellData(ScanTables scanTable, String columnName,
            double value, String strVal) {
        this.scanTable = scanTable;
        this.columnName = columnName;
        this.value = value;
        this.strVal = strVal;
        this.setColor();
        displayAsInt = scanCfg.displayAsIntTable(scanTable, columnName);
    }

    public SCANTableCellData(ScanTables scanTable, String columnName,
            String text) {
        this(scanTable, columnName, text, null);
    }

    /**
     * Constructor.
     * 
     * @param scanTable
     *            SCAN table identifier.
     * @param columnName
     *            Name of the column.
     * @param text
     *            Data as a String.
     */
    public SCANTableCellData(ScanTables scanTable, String columnName,
            String text, WARN_TYPE wrnType) {
        this.warnType = wrnType;
        this.scanTable = scanTable;
        this.columnName = columnName;
        this.cellText = text.trim();

        if (columnName.compareTo(CELLTable.COUNTY.getColName()) == 0) {
            isCounty = true;
            // if (cellText.length() > 6) {
            // shortText = cellText.substring(0, 6);
            // }
        }

        if ((this.scanTable == ScanTables.TVS)
                && (this.columnName.compareTo(TVSTable.TYPE.getColName()) == 0)) {
            if (this.cellText.trim().endsWith("TVS") == true) {
                backgroundColor = Display.getCurrent().getSystemColor(
                        SWT.COLOR_MAGENTA);
                return;
            }
        } else if ((this.scanTable == ScanTables.MESO)
                && (this.columnName.compareTo(MESOTable.TVS.getColName()) == 0)) {
            if (this.cellText.trim().endsWith("Y") == true) {
                backgroundColor = scanCfg.getTableItemColor(scanTable,
                        columnName, ScanThresholdColor.Upper);
                return;
            }
        } else if ((this.scanTable == ScanTables.DMD)
                && (this.columnName.compareTo(DMDTable.TVS.getColName()) == 0)) {
            if (this.cellText.trim().equalsIgnoreCase("Y")) {
                backgroundColor = Display.getCurrent().getSystemColor(
                        SWT.COLOR_MAGENTA);
                return;
            }
        } else if ((columnName.compareTo(CELLTable.TVS.getColName()) == 0)
                || (columnName.compareTo(DMDTable.TVS.getColName()) == 0)) {
            if (this.cellText.trim().endsWith("TVS") == true) {
                // backgroundColor = scanCfg.getScanColor(ScanColors.TVS);
                backgroundColor = Display.getCurrent().getSystemColor(
                        SWT.COLOR_MAGENTA);
                return;
            }
        } else if (columnName.compareTo(DMDTable.CLASS.getColName()) == 0) {
            setStrankBackgroundColor();
            if (scanTable == ScanTables.DMD) {
                this.cellText = scanCfg.getDmdClassification(String
                        .valueOf(Math.round(value)));
            } else {
                this.cellText = scanCfg.getMesoClassification(String
                        .valueOf(Math.round(value)));
            }
            return;
        } else if (columnName.compareTo(DMDTable.STRANK.getColName()) == 0) {
            setStrankBackgroundColor();
            return;
        } else if ((columnName.compareTo(CELLTable.MDASR.getColName()) == 0)
                || (columnName.compareTo(MESOTable.MDASR.getColName()) == 0)) {
            if (this.cellText == null) {
                backgroundColor = scanCfg.getTableItemColor(scanTable,
                        columnName, ScanThresholdColor.Default);
                return;
            }

            determineValueAndSuffix();

            this.setColor();
            displayAsInt = scanCfg.displayAsIntTable(scanTable, columnName);
            this.cellText = null;
            return;
        }

        if (columnName.compareTo("ident") == 0) {
            if (warnType == null) {
                backgroundColor = scanCfg.getTableItemColor(scanTable,
                        columnName, ScanThresholdColor.Ident);
            } else {
                foregroundColor = scanCfg.getUnwarnedColor(warnType);
                backgroundColor = scanCfg.getCellForegroundColor();
            }
        } else {
            backgroundColor = scanCfg.getTableItemColor(scanTable, columnName,
                    ScanThresholdColor.Default);
        }
    }

    /**
     * Get the value as a double.
     * 
     * @return The value as a double.
     */
    public double getValueAsDouble() {
        return value;
    }

    /**
     * Get the table cell text string.
     * 
     * @return The table cell text string.
     */
    public String getCellText() {
        return cellText;
    }

    /**
     * Set the table cell text string.
     * 
     * @cellText The table cell text string.
     */
    public void setCellText(String cellText) {
        this.cellText = cellText;
    }

    /**
     * Set the background color of the table cell.
     */
    public void setColor() {
        if (columnName.compareTo(DMDTable.STRANK.getColName()) == 0) {
            setStrankBackgroundColor();
            return;
        }

        if ((this.scanTable == ScanTables.DMD)) {
            if (columnName.compareTo(DMDTable.BASE.getColName()) == 0) {
                setBaseBackgroundColor();
                return;
            }
        }

        // value rounded to int
        if (scanCfg.displayAsIntTable(scanTable, columnName)) {
            backgroundColor = scanCfg.getThresholdColor(scanTable, columnName,
                    Math.round(value));
        } else {
            // value rounded to tenth
            double basisValue = Math.round(value * 10) / 10.0;
            backgroundColor = scanCfg.getThresholdColor(scanTable, columnName,
                    basisValue);
        }
    }

    /**
     * Set the stRank background cell color.
     */
    private void setStrankBackgroundColor() {

        determineValueAndSuffix();

        if (scanTable == ScanTables.MESO) {
            backgroundColor = scanCfg.getThresholdColor(scanTable,
                    MESOTable.MDASR.getColName(), value);
        } else {
            backgroundColor = scanCfg.getThresholdColor(scanTable,
                    DMDTable.STRANK.getColName(), value);
        }
    }

    /**
     * The cell text passed in could have a number and a character suffix. If
     * there is only a number then the value will be set to the number and the
     * suffix will be null. If there is a number and suffix the number will be
     * stored as a double and the suffix will be stored as a separate string.
     */
    private void determineValueAndSuffix() {
        if (this.cellText != null) {
            if (this.cellText.matches("[0-9.]+") == true) {
                value = Double.valueOf(this.cellText);
            } else if (this.cellText.endsWith("L")
                    || this.cellText.endsWith("S")) {
                try {
                    suffix = cellText.substring(cellText.length() - 1);
                    value = Double.valueOf(cellText.substring(0,
                            cellText.length() - 1));

                    if (suffix.compareTo("L") == 0) {
                        suffixWeightedValue = 0.5;
                    } else if (suffix.compareTo("S") == 0) {
                        suffixWeightedValue = 0.1;
                    }
                } catch (Exception ex) {
                    value = -999.0;
                    suffix = null;
                }
            } else {
                value = Double.MIN_VALUE;
            }
        }
        // Nullify the cell text.
        this.cellText = null;
        // The value needs to be displayed as an integer.
        displayAsInt = true;
    }

    /**
     * Set the base background cell color in SCAN DMD table per DR #5388.
     */
    private void setBaseBackgroundColor() {
        backgroundColor = scanCfg.getDMDBaseBGColor(strVal, value);
    }

    /**
     * Data as a string so it can be sorted.
     * 
     * @param direction
     *            Sort direction.
     * @return Data as a string.
     */
    public Object sortByObject(int direction) {
        if (cellText != null) {

            if (p.matcher(cellText).matches() == true) {
                return String.format(rightJustified, cellText);
            }

            return String.format(leftJustified, cellText);
        } else if (value.isNaN() == false) {

            if (suffix != null) {
                return new Double(value + suffixWeightedValue);
            }

            if (displayAsInt == true) {
                return new Double(Math.round(value));
            }

            return value;
        } else if (value.isNaN() == true) {
            if (direction == SWT.DOWN) {
                return Double.MIN_VALUE * -1.0;
                // return new Double(-99999999999.9);
            }
            // return new Double(99999999999.9);
            return Double.MAX_VALUE;
        }

        return "Unknown";
    }

    /**
     * String to be displayed in the table cell.
     * 
     * @return The display string.
     */
    public String displayString() {
        /*
         * Format the data for the display.
         */
        if (cellText != null) {
            if (shortText != null) {
                return shortText;
            }
            return cellText;
        } else if ((value.isNaN() == false) && (value != Double.MIN_VALUE)) {
            StringBuilder strBld = new StringBuilder();

            if (displayAsInt == true) {
                strBld.append(String.valueOf((int) Math.round(value)));
            } else {
                if (scanTable == ScanTables.DMD) {
                    strBld.append(String.format("%1.1f", value));
                } else {
                    strBld.append(String.format("%1.2f", value));
                }
            }

            if (suffix != null) {
                strBld.append(suffix);
            }

            return strBld.toString();
        } else if ((value.isNaN() == true) || (value == Double.MIN_VALUE)) {
            return "NONE";
        }

        return "Unknown";
    }

    /**
     * Get the table cell background color.
     * 
     * @return The background color.
     */
    public Color getBackgroundColor() {
        return backgroundColor;
    }

    public Color getForegroundColor() {
        return foregroundColor;
    }

    /**
     * Set the table cell background color.
     * 
     * @color The background color.
     */
    public void setBackgroundColor(Color color) {
        backgroundColor = color;
    }

    /**
     * Get the column name.
     * 
     * @return The column name.
     */
    public String getColumnName() {
        return columnName;
    }

    /**
     * @return the warnType
     */
    public WARN_TYPE getWarnType() {
        return warnType;
    }

    public boolean isCounty() {
        return isCounty;
    }
}
