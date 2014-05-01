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

import java.io.File;
import java.util.Arrays;

import org.eclipse.swt.widgets.Widget;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Contains the set of colors for displaying a particular data type
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
public class ColorThresholdArray {
    private String missingColorName;

    private String defaultColorName;

    private ColorThreshold[] colorThresholds;
    
    private double classId = Math.random();
    
    public ColorThresholdArray() {
        
    }

    /**
     * constructor
     * 
     * @param missing_color_name
     *            color to be used for missing data
     * @param default_color_name
     *            default color to be used
     * @param threshold_values
     *            array of threshold values
     * @param color_names
     *            array of color names corresponding to each threshold value
     */
    public ColorThresholdArray(final String missing_color_name,
            final String default_color_name, final double threshold_values[],
            final String color_names[]) {
        int i;

        colorThresholds = new ColorThreshold[color_names.length];

        for (i = 0; i < colorThresholds.length; ++i) {
            colorThresholds[i] = new ColorThreshold(threshold_values[i],
                    color_names[i]);
        }

        missingColorName = missing_color_name;
        defaultColorName = default_color_name;

    }

    int loadColorThresholdArrayFromFile(ColorThresholdArray ctArray, File fp,
            Widget widget) {
        // TODO implement this
        throw new NotImplementedException();
    }

    String determineColorByThreshold(double value, double missingValue,
            final ColorThresholdArray colorArray) {
        /*
         * Finds the appropriate color that matches the entered value
         * in the array of points.
         */
        String color = null;
        int i = 0;

        if (value == HydroConstants.MISSING_VALUE) {
            color = colorArray.getMissingColorName();
        } else { /* not a missing value */
            color = colorArray.getDefaultColorName();
            ColorThreshold[] thresholdArray = colorArray.getThresholds();
            
            // Loop from the top to the bottom
            for (i = thresholdArray.length - 1 ; i >= 0; i--) {   
                if (value >= thresholdArray[i].getValue()) {
                    color = thresholdArray[i].getColorName();
                    break;
                }
            }
        }
        return color;   
    }

    void copyColorThresholdArray(ColorThresholdArray dest,
            final ColorThresholdArray source) {
        // TODO implement this
        throw new NotImplementedException();
    }

    void freeColorThresholdArray(ColorThresholdArray ctArray) {
        // TODO implement this
        throw new NotImplementedException();
    }

    void printColorThresholdArray(final ColorThresholdArray colorArray) {
        // TODO implement this
        throw new NotImplementedException();
    }

    void loadDefaultColorThresholdArray(ColorThresholdArray ctArray,
            Widget widget) {
        // TODO implement this
        throw new NotImplementedException();
    }

    /**
     * @return the missingColorName
     */
    public String getMissingColorName() {
        return missingColorName;
    }
    
    /**
     * Set the missing color name.
     * @param missingColorName
     *      The missing color name
     */
    public void setMissingColorName(String missingColorName) {
        this.missingColorName = missingColorName;
    }

    /**
     * @return the defaultColorName
     */
    public String getDefaultColorName() {
        return defaultColorName;
    }
    
    /**
     * Set the default color name.
     * @param defaultColorName
     *      The default color name
     */
    public void setDefaultColorName(String defaultColorName) {
        this.defaultColorName = defaultColorName;
    }

    /**
     * @return the thresholds
     */
    public ColorThreshold[] getThresholds() {
        return colorThresholds;
    }
    
    /**
     * Return a new ColorThreshold array instantiated to the size
     * @param size
     *      The size of the new array
     * @return
     *      A new array of "size" dimensions 
     */
    public ColorThreshold[] getNewThresholds(int size) {
        return new ColorThreshold[size];
    }

    /**
     * Initialize the ColorThreshold array to size dimensions.
     * 
     * @param size
     *      The new dimension for the ColorThreshold array
     */
    public void initializeColorThresholdArray(int size) {
        colorThresholds = new ColorThreshold[size];
    }
    
    /**
     * @return the classId
     */
    public double getClassId() {
        return classId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[" + classId + "]");
        sb.append("{ missing=");
        sb.append(missingColorName);
        sb.append(", default=");
        sb.append(defaultColorName);
        sb.append(Arrays.toString(colorThresholds));
        return sb.toString();
    }
}