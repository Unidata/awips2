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
package com.raytheon.viz.hydrocommon.whfslib.colorthreshold;

/**
 * Contains a value, color pair for color mapping data.
 * 
 * "value" is the threshold for the location to be allowed to be colored
 * according to "color"
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class ColorThreshold {
    /**
     * The value for the color.
     */
    private double value;

    /**
     * The color name.
     */
    private String colorName;

    /**
     * constructor
     * 
     * @param value
     * @param colorName
     */
    public ColorThreshold(double value, String colorName) {
        this.value = value;
        this.colorName = colorName;
    }

    /**
     * @return the value
     */
    public double getValue() {
        return value;
    }

    /**
     * @return the colorName
     */
    public String getColorName() {
        return colorName;
    }

    /**
     * @param value the value to set
     */
    public void setValue(double value) {
        this.value = value;
    }

    /**
     * @param colorName the colorName to set
     */
    public void setColorName(String colorName) {
        this.colorName = colorName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "{" + value + ", " + colorName + "}";
    }

}