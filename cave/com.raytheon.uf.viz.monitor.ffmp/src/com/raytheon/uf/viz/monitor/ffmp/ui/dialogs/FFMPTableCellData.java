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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.TableCellColor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.ThreshColNames;

/**
 * Table cell data class to define how the data is to be displayed in a table.
 * At this time cell data only supports black text (default) but can be modified
 * to handle foreground (text) color.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 * Apr 12, 2013   1902    mpduff       Optimized the color assignments.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FFMPTableCellData {
    /**
     * Cell text.
     */
    private String cellText = null;

    /**
     * Text to be displayed when the mouse hovers over the table cell.
     */
    private String hoverText = null;

    /**
     * Numerical value.
     */
    private Float value = Float.NaN;

    /**
     * Flag indicating the value is of type integer.
     */
    private boolean displayAsInt = false;

    private Color backgroundColor;

    private FIELDS columnName;

    private static final FFMPConfig ffmpCfg = FFMPConfig.getInstance();

    private boolean vgbFlag = false;

    private boolean guidForcedFlag = false;

    private String displayStr = null;

    private double colorValue;

    /**
     * Constructor.
     * 
     * @param columnName
     *            The column name
     * @param value
     *            The column value
     */
    public FFMPTableCellData(FIELDS columnName, float value) {
        this(columnName, value, false);
    }

    /**
     * Constructor.
     * 
     * @param columnName
     *            Numerical value.
     * @param value
     *            Value.
     * @param forced
     *            Forced flag.
     */
    public FFMPTableCellData(FIELDS columnName, float value, boolean forced) {
        if (columnName == FIELDS.RATIO) {
            displayAsInt = true;
        }

        this.columnName = columnName;
        this.value = value;
        this.guidForcedFlag = forced;

        if (displayAsInt == true) {
            colorValue = Math.rint(value);
        } else {
            if (!this.value.isNaN()) {
                colorValue = ((Math.round(value * 100.0)) / 100.0);
            } else {
                colorValue = Float.NaN;
            }
        }

        this.generateCellColor();
    }

    /**
     * Constructor.
     * 
     * @param columnName
     *            Column name.
     * @param cellText
     *            Test to be displayed in the table cell.
     * @param hoverText
     *            Hover text.
     */
    public FFMPTableCellData(FIELDS columnName, String cellText,
            String hoverText) {
        if (columnName == FIELDS.VIRTUAL) {
            this.columnName = FIELDS.NAME;
            backgroundColor = ffmpCfg.getCellColor(TableCellColor.VGB);
            vgbFlag = true;
        } else {
            this.columnName = columnName;
            backgroundColor = ffmpCfg.getCellColor(TableCellColor.Default);
            vgbFlag = false;
        }
        this.cellText = cellText;
        this.hoverText = hoverText;
    }

    /**
     * Get value as an float.
     * 
     * @return Value as a float.
     */
    public float getValueAsFloat() {
        return value;
    }

    /**
     * Flag indicating if this is a VGB
     * 
     * @return True if this is a VGB, false otherwise.
     */
    public boolean isVGB() {
        return vgbFlag;
    }

    /**
     * Get the cell text.
     * 
     * @return The cell text.
     */
    public String getCellText() {
        return cellText;
    }

    /**
     * Set the RGB which is the cell background color.
     */
    public void generateCellColor() {
        if ((columnName == FIELDS.GUIDANCE) && this.guidForcedFlag) {
            if (this.value.isNaN()) {
                backgroundColor = ffmpCfg.getCellColor(TableCellColor.Default);
            } else {
                backgroundColor = ffmpCfg
                        .getCellColor(TableCellColor.ForcedFFG);
            }
        } else if (columnName == FIELDS.GUIDANCE) {
            backgroundColor = ffmpCfg.getThresholdColor(
                    ThreshColNames.GUID.name(), colorValue);
        } else {
            backgroundColor = ffmpCfg.getThresholdColor(columnName.name(),
                    colorValue);
        }
    }

    /**
     * Get the hover text (mouse hover).
     * 
     * @return The hover text.
     */
    public String getHoverText() {
        return this.hoverText;
    }

    /**
     * Sort by method.
     * 
     * @param direction
     *            Sort direction.
     * @return Object that is a string or number.
     */
    public Object sortByObject(int direction) {
        if (cellText != null) {
            return String.format("%-20S", cellText);
        } else if (value.isNaN() == false) {
            if (displayAsInt == true) {
                return new Float(Math.round(value));
            }

            return new Float(value);
        } else if (value.isNaN() == true) {
            if (direction == SWT.DOWN) {
                return Float.MAX_VALUE * -1.0f;
            }
            return Float.MAX_VALUE;
        }

        return "Unknown";
    }

    public String displayString() {
        if (this.displayStr == null) {
            /*
             * Format the data for the display.
             */
            if (cellText != null) {
                displayStr = cellText;
            } else if (value.isNaN() == false) {
                if (displayAsInt == true) {
                    displayStr = String.valueOf(Math.round(value));
                } else {
                    displayStr = String.format("%1.2f", value);
                }

            } else if (value.isNaN() == true) {
                displayStr = "M";
            } else {
                displayStr = "Unknown";
            }
        }
        return displayStr;
    }

    /**
     * Get the cell background RGB.
     * 
     * @return The cell background RGB.
     */
    public Color getBackgroungColor() {
        return backgroundColor;
    }
}
