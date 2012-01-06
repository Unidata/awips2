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
package com.raytheon.viz.hydro.riversummary;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * River Summary Max/Min data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2010 4382       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class MaxMin {
    /**
     * Missing Value.
     */
    private static final double MISSING_VALUE = HydroConstants.MISSING_VALUE;

    /**
     * Minimum value.
     */
    private double minValue = 9999;

    /**
     * Maximum value.
     */
    private double maxValue = MISSING_VALUE;
    
    /**
     * Constructor
     */
    public MaxMin() {
        
    }

    /**
     * Constructor
     */
    public MaxMin(double minValue, double maxValue) {
        this.minValue = minValue;
        this.maxValue = maxValue;
    }

    /**
     * Check this value for min/max status.
     * @param value
     */
    public void checkValue(double value) {
        if (value != MISSING_VALUE) {
            if (value < minValue) {
                minValue = value;
            }

            if (value > maxValue) {
                maxValue = value;
            }
        }

        return;
    }

    public double getMaxValue() {
        return maxValue;
    }

    public double getMinValue() {
        return minValue;
    }

    @Override
    public String toString() {
        return String.format("Max = %f  Min = %f\n", maxValue, minValue);
    }
}
