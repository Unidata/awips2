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

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;

/**
 * Table cell data class to define how the data is to be displayed in a table.
 * At this time cell data only supports black text (default) but can be modified
 * to handle foreground (text) color.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TableCellData {
    /**
     * Date and/or Time.
     */
    private Date date = null;

    /**
     * Date formatter.
     */
    private SimpleDateFormat dateFmt = null;

    /**
     * Default date format.
     */
    private final String dateFmtStr = "HH:mm MMM dd";

    /**
     * Cell text.
     */
    private String cellText = null;

    /**
     * Cell background color.
     */
    private RGB rgb = null;

    /**
     * Text to be displayed when the mouse hovers over the table cell.
     */
    private String hoverText = null;

    /**
     * Numerical value.
     */
    private Double value = Double.NaN;

    /**
     * Type of table cell defined by CellType in CommonTableConfig. Helps define
     * the display of the data in the cell.
     */
    private CellType cellType = CellType.NotMonitored;

    /**
     * Flag indicating the value is of type integer.
     */
    private boolean valueIsInt = false;

    /**
     * Prefixed the CellType so it is displayed in the data cell along with the
     * text/value.
     */
    private boolean displayCellTypePrefix = true;

    /**
     * Decimal factor for rounding a double value.
     */
    private int decimalFactor = 10000;

    /**
     * history table column [some columns in history table need special text
     * format] [June 4, 2010, zhao]
     */
    private CommonTableConfig.obsHistCols histCol;

    /**
     * Constructor.
     */
    public TableCellData() {
    }

    /**
     * Constructor.
     * 
     * @param value
     *            Numerical value.
     * @param celltype
     *            Cell type.
     * @param displayPrefix
     *            Display cell type flag.
     */
    public TableCellData(double value, CellType celltype, boolean displayPrefix) {
        this.value = value;
        this.cellType = celltype;
        this.displayCellTypePrefix = displayPrefix;
    }

    public TableCellData(double value, CellType celltype,
            boolean displayPrefix, int displayDec, RGB cellColor) {
        this.value = value;
        this.cellType = celltype;
        this.displayCellTypePrefix = displayPrefix;

        this.setDisplayDecimals(displayDec);
        this.setRgb(cellColor);
    }

    /**
     * Constructor.
     * 
     * @param value
     *            Numerical value.
     * @param celltype
     *            Cell type.
     * @param displayPrefix
     *            Display cell type flag.
     */
    public TableCellData(float value, CellType celltype, boolean displayPrefix) {
        this.value = new Double(value);
        this.cellType = celltype;
        this.displayCellTypePrefix = displayPrefix;
    }

    /**
     * Constructor [mainly for History Table, in which some columns need special
     * cell text format June 4, 2010, zhao]
     * 
     * @param value
     *            : numerical value
     * @param cellType
     *            : cell type
     * @param histCol
     *            : enumerated History Column
     */
    public TableCellData(float value, CellType cellType,
            CommonTableConfig.obsHistCols histCol) {
        this.value = new Double(value);
        this.cellType = cellType;
        this.displayCellTypePrefix = false;
        this.histCol = histCol;
    }

    /**
     * Constructor.
     * 
     * @param value
     *            Numerical value.
     * @param celltype
     *            Cell type.
     * @param displayPrefix
     *            Display cell type flag.
     */
    public TableCellData(int value, CellType celltype, boolean displayPrefix) {
        valueIsInt = true;
        this.value = (double) value;
        this.cellType = celltype;
        this.displayCellTypePrefix = displayPrefix;
    }

    /**
     * Constructor.
     * 
     * @param cellText
     *            Text to be displayed in the table cell.
     * @param hoverText
     *            Text to be displayed when the mouse hovers over the table
     *            cell.
     * @param celltype
     *            Cell type.
     * @param displayPrefix
     *            Display cell type flag.
     */
    public TableCellData(String cellText, String hoverText, CellType celltype,
            boolean displayPrefix) {
        this.cellText = cellText;
        this.hoverText = hoverText;
        this.cellType = celltype;
        this.displayCellTypePrefix = displayPrefix;
    }

    public TableCellData(String cellText, String hoverText, CellType celltype,
            boolean displayPrefix, RGB cellColor) {
        this.cellText = cellText;
        this.hoverText = hoverText;
        this.cellType = celltype;
        this.displayCellTypePrefix = displayPrefix;
        this.setRgb(cellColor);
    }

    /**
     * Constructor.
     * 
     * @param cellText
     *            Text to be displayed in the table cell.
     * @param celltype
     *            Cell type.
     * @param displayPrefix
     *            Display cell type flag.
     */
    public TableCellData(String cellText, CellType celltype,
            boolean displayPrefix) {
        this.cellText = cellText;
        this.hoverText = null;
        this.cellType = celltype;
        this.displayCellTypePrefix = displayPrefix;
    }

    /**
     * Constructor.
     * 
     * @param date
     *            Date/Time.
     * @param celltype
     *            Cell type.
     */
    public TableCellData(Date date, CellType celltype) {
        this.date = date;
        dateFmt = new SimpleDateFormat(dateFmtStr);
        this.cellType = celltype;
    }

    /**
     * Constructor.
     * 
     * @param date
     *            Date/Time.
     * @param dateFormat
     *            Date/Time format.
     * @param celltype
     *            Cell type.
     */
    public TableCellData(Date date, String dateFormat, CellType celltype) {
        this.date = date;
        dateFmt = new SimpleDateFormat(dateFormat);
        this.cellType = celltype;
    }

    /**
     * Set the value.
     * 
     * @param val
     *            Value to set.
     */
    public void setValue(int val) {
        valueIsInt = true;
        value = (double) val;
        this.cellText = null;
    }

    /**
     * Set the value.
     * 
     * @param val
     *            Value to set.
     */
    public void setValue(double val) {
        valueIsInt = false;
        value = val;
        //this.cellText = null;
    }

    /**
     * Set the value.
     * 
     * @param val
     *            Value to set.
     */
    public void setValue(float val) {
        valueIsInt = false;
        value = new Double(val);
        //this.cellText = null;
    }

    /**
     * Get value as an integer.
     * 
     * @return Value as an integer.
     */
    public int getValueAsInt() {
        return value.intValue();
    }

    /**
     * Get value as an double.
     * 
     * @return Value as a double.
     */
    public double getValueAsDouble() {
        return value;
    }

    /**
     * Get value as an float.
     * 
     * @return Value as a float.
     */
    public float getValueAsFloat() {
        return value.floatValue();
    }

    /**
     * Set the cell text.
     * 
     * @param cellText
     *            Text to set.
     */
    public void setCellText(String cellText) {
        this.cellText = cellText;
        this.value = Double.NaN;
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
     * 
     * @param rgb
     */
    public void setRgb(RGB rgb) {
        this.rgb = rgb;
    }

    /**
     * Set the hover text (mouse hover).
     * 
     * @param hoverText
     *            Hover text.
     */
    public void setHoverText(String hoverText) {
        this.hoverText = hoverText;
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
     * Set the cell text.
     * 
     * @param celltype
     *            Cell type.
     */
    public void setCelltype(CellType celltype) {
        this.cellType = celltype;
    }

    /**
     * Set the number of decimals to be displayed for a decimal value.
     * 
     * @param dec
     *            Display decimals.
     */
    public void setDisplayDecimals(int dec) {
        decimalFactor = (int) Math.pow(10.0, dec);
    }

    /**
     * Return the display prefix flag.
     * 
     * @return True if the prefix will be displayed, false if not.
     */
    public boolean getDisplayCellTypePrefix() {
        return displayCellTypePrefix;
    }

    /**
     * Set the cell type display prefix.
     * 
     * @param displayCellTypePrefix
     *            True displays the cell type in the table cell, false does not.
     */
    public void setDisplayCellTypePrefix(boolean displayCellTypePrefix) {
        this.displayCellTypePrefix = displayCellTypePrefix;
    }

    /**
     * Get the sort by string that will be used to sort the data in the
     * column/table.
     * 
     * @param direction
     *            Sort direction.
     * @return The sort string.
     */
    public String sortByString(int direction) {
        CommonTableConfig ztg = CommonTableConfig.getInstance();
        
        StringBuilder sb = new StringBuilder(ztg.getStringOffset(direction, cellType));
        
        if ( cellText != null && value.isNaN() == false ) {
        	// An example of this case is the visibility TableCellData;
        	// use value for sorting
        	sb.append(String.format("%010.4f", value));
        } 
        else if (cellText != null)
        {
            sb.append(cellText);
        } else if (value.isNaN() == false) {
            sb.append(String.format("%010.4f", value));
        } else if (date != null) {
            sb.append(date.getTime());
        }

        return sb.toString();
    }

    /**
     * Get the string to be displayed in the table cell.
     * 
     * @return
     */
    public String displayString() {
        /**
         * Check if it is CellType.ObsHist and the value is ObConst.MISSING or
         * needs special decimal format
         */
        if (cellType == CellType.ObsHist) {

            if (value.floatValue() == ObConst.MISSING) {
                return CellType.NotAvailable.getDisplayString();
            }

            if (histCol == CommonTableConfig.obsHistCols.P) {
                return new DecimalFormat("0.00").format(value);
            }

            if (histCol == CommonTableConfig.obsHistCols.PTend) {
                return new DecimalFormat("'+'0.00;'-'0.00").format(value);
            }
        }

        /*
         * If the cell type is not available, not monitored, or not determined
         * then return the display string for that type.
         */
        if (cellType == CellType.NotAvailable
                || cellType == CellType.NotMonitored
                || cellType == CellType.NotDetermined) {
            return cellType.getDisplayString();
        }

        StringBuilder sb = new StringBuilder();

        /*
         * Check if a Type prefix needs to be displayed.
         */
        if (displayCellTypePrefix == true) {
            if (cellType == CellType.R || cellType == CellType.Y
                    || cellType == CellType.G) {
                sb.append(cellType.getDisplayString()).append(" ");
            } else if (cellType == CellType.NotAvailable
                    || cellType == CellType.NotMonitored
                    || cellType == CellType.NotDetermined) {
                return cellType.getDisplayString();
            }
        }

        /*
         * Format the data for the display.
         */
        if (cellText != null) {
            sb.append(cellText);
        } else if (value.isNaN() == false) {
            if (valueIsInt == true) {
                /**
                 * check if it is Clear_Sky, i.e., Ceiling="CLR" or "SKC"
                 */
                if (value.intValue() == ObConst.CLR_SKY_INTEGER) {
                    sb.append(ObConst.CLR_SKY_STRING);
                } else if (value.intValue() == ObConst.SKC_SKY_INTEGER) {
                    sb.append(ObConst.SKC_SKY_STRING);
                } else {
                    sb.append(value.intValue());
                }
            } else {
                double d = value * decimalFactor;
                d = Math.round(d) / (double) decimalFactor;
                sb.append(d);
            }
        } else if (date != null) {
            sb.append(dateFmt.format(date));
        }

        return sb.toString();
    }

    /**
     * Get the cell background RGB.
     * 
     * @return The cell background RGB.
     */
    public RGB getBackgroungRGB() {
        if (rgb != null) {
            return rgb;
        }

        CommonTableConfig ztg = CommonTableConfig.getInstance();

        return ztg.getCellColor(cellType);
    }
}
