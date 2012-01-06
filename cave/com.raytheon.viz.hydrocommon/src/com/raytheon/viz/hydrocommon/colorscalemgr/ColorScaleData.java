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

package com.raytheon.viz.hydrocommon.colorscalemgr;

import org.eclipse.swt.graphics.RGB;

/**
 * Color data type sets for the color scale manager.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ColorScaleData {
    /**
     * RGB color value.
     */
    public RGB color;

    /**
     * Scale value.
     */
    public String value;

    /**
     * "Missing" string value.
     */
    public static final String MISSING = "MSG";

    /**
     * "Less than" string value.
     */
    public static final String LESS_THAN_MIN = "< Min";

    /**
     * Constructor.
     */
    public ColorScaleData() {
    }

    /**
     * Constructor.
     * 
     * @param color
     *            RGB color.
     * @param value
     *            Scale value.
     */
    public ColorScaleData(RGB color, double value) {
        this.color = color;
        setValueLbl(value);
    }

    /**
     * Set the missing color and scale value to "MSG".
     * 
     * @param rgb
     */
    public void missingScaleData(RGB rgb) {
        color = rgb;
        value = MISSING;
    }

    /**
     * Set the missing color and scale value to "> Min".
     * 
     * @param rgb
     *            RGB color.
     */
    public void lessThanMinScaleData(RGB rgb) {
        color = rgb;
        value = LESS_THAN_MIN;
    }

    /**
     * Set the color.
     * 
     * @param rgb
     *            RGB color.
     */
    public void setColor(RGB rgb) {
        color = rgb;
    }

    // /**
    // * Set the scale value.
    // * @param value Value in string format.
    // */
    // public void setValue(String value)
    // {
    // this.value = value;
    // }

    /**
     * Set the scale value.
     * 
     * @param dblValue
     *            The scale value.
     */
    public void setValueLbl(double dblValue) {
        dblValue = dblValue * 100.0;

        dblValue = Math.round(dblValue) / 100.0;

        value = String.format("%3.2f", dblValue);
    }

    /**
     * Check if the string value passed in is equal to the existing scale value.
     * 
     * @param strVal
     *            Scale value in string format.
     * @return True if the values are the same, false otherwise.
     */
    public boolean equalValue(String strVal) {
        if (value.compareTo(strVal) == 0) {
            return true;
        }

        return false;
    }

    /**
     * Check if the double value passed in is equal to the existing scale value.
     * 
     * @param dblVal
     *            Scale value.
     * @return True if the values are the same, false otherwise.
     */
    public boolean equalValue(double dblVal) {
        Double rvDbl = getDoubleVal();

        if (rvDbl.isNaN() == true) {
            return false;
        } else if (dblVal == rvDbl.doubleValue()) {
            return true;
        }

        return false;
    }

    /**
     * Get the value as a Double object.
     * 
     * @return Scale value as a Double.
     */
    public Double getDoubleVal() {
        Double rvDbl;

        try {
            if (value.equals(LESS_THAN_MIN)) {
                rvDbl = new Double(-8888);
            } else if (value.equals(MISSING)) {
                rvDbl = new Double(-9999);
            } else {
                rvDbl = Double.parseDouble(value);
            }
        } catch (Exception ex) {
            rvDbl = Double.NaN;
        }

        return rvDbl;
    }
}
