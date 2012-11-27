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
package com.raytheon.uf.common.geospatial.interpolation;

import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;

/**
 * Bilinear interpolation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class BilinearInterpolation implements Interpolation {

    private float missingThreshold = 0.25f;

    public BilinearInterpolation() {
    }

    @Override
    public double getInterpolatedValue(DataSource source, double x, double y) {
        double value = 0.0f;
        double missing = 1.0f;

        int xi = (int) Math.floor(x);
        int yi = (int) Math.floor(y);
        // Upper left corner
        double xWeight = 1 - x + xi;
        double yWeight = 1 - y + yi;
        double weight = xWeight * yWeight;
        double val = source.getDataValue(xi, yi);
        if (Double.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }
        // upper right corner
        xi = xi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - y + yi;
        weight = xWeight * yWeight;
        val = source.getDataValue(xi, yi);
        if (Double.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }
        // lower right corner
        yi = yi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        val = source.getDataValue(xi, yi);
        if (Double.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }
        // lower left corner
        xi = xi - 1;
        xWeight = 1 - x + xi;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        val = source.getDataValue(xi, yi);
        if (Double.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }

        if (missing >= 1.0 || missing > missingThreshold) {
            // compensate for missing data
            return value / missing;
        } else {
            // If we are too far from the data skip it
            return Double.NaN;
        }
    }

    /**
     * The missing threshold is used along grid boundaries and in "holes" in the
     * grid(regions with NaN or values outside the valid range). This value
     * should be set to a value between 0 and 1. Bilinear interpolation uses a
     * weighted average and this value represents the weight of data that must
     * be valid to interpolate. 1 means all data must be present, 0 means it
     * will interpolate even when the value is almost entirely NaN and 0.5 means
     * that up to half of the input for an interpolation can be NaN.
     * 
     * @param missingThreshold
     */
    public void setMissingThreshold(float missingThreshold) {
        if (missingThreshold > 1.0f) {
            this.missingThreshold = 1.0f;
        } else if (missingThreshold < 0.0f) {
            missingThreshold = 0.0f;
        } else {
            this.missingThreshold = missingThreshold;
        }
    }

}
