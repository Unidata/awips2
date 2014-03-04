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
package com.raytheon.uf.common.colormap;

/**
 * Converter that can do simple log scaling given a start/end range on either
 * side of zero.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 07, 2013  2492     mschenke    Initial creation
 * Feb 28, 2013  2791     bsteffen    Make EFFECTIVE_ZERO consistent with gl version.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LogConverter {

    private static double EFFECTIVE_ZERO = 0.0000001;

    public static double valueToIndex(double value, double rangeMin,
            double rangeMax) {
        // Account for 0 min index
        if (rangeMin == 0) {
            rangeMin = EFFECTIVE_ZERO;
            if (rangeMax < 0) {
                rangeMin = -rangeMin;
            }
        }

        boolean reverse = false;
        if ((value < rangeMin && rangeMin > 0)
                || (value > rangeMin && rangeMin < 0)) {
            reverse = true;
        }

        value = Math.abs(value);
        rangeMin = Math.abs(rangeMin);
        rangeMax = Math.abs(rangeMax);

        // Check uncomputable index value, everything between this range is 0,
        // rangeMin->rangeMax 0 -> 1, -rangeMin->-rangeMax 0 -> -1
        if (value <= rangeMin && value >= -rangeMin) {
            return 0;
        }

        double index = (Math.log(value) - Math.log(rangeMin))
                / (Math.log(rangeMax) - Math.log(rangeMin));
        if (reverse) {
            index = -index;
        }

        return index;
    }

    public static double indexToValue(double index, double rangeMin,
            double rangeMax) {
        // Account for 0 min index
        if (rangeMin == 0) {
            rangeMin = EFFECTIVE_ZERO;
            if (rangeMax < 0) {
                rangeMin = -rangeMin;
            }
        }

        boolean reverse = index < 0;

        index = Math.abs(index);
        rangeMin = Math.abs(rangeMin);
        rangeMax = Math.abs(rangeMax);

        double value = Math.exp(Math.log(rangeMin)
                + (index * (Math.log(rangeMax) - Math.log(rangeMin))));
        if (reverse) {
            value = -value;
        }
        return value;
    }

}
