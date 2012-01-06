package com.raytheon.uf.common.geospatial.interpolation;

import org.geotools.coverage.grid.GeneralGridGeometry;

/**
 * 
 * Bilinear interpolation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class BilinearInterpolation extends AbstractInterpolation {

    private float missingThreshold = 0.25f;

    public BilinearInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        super(sourceGeometry, targetGeometry, minValid, maxValid, fillValue);
    }

    public BilinearInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(sourceGeometry, targetGeometry);
    }

    public BilinearInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        super(data, sourceGeometry, targetGeometry, minValid, maxValid,
                fillValue);
    }

    public BilinearInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(data, sourceGeometry, targetGeometry);
    }

    @Override
    protected float getInterpolatedValue(double xd, double yd) {
        float value = 0.0f;
        float missing = 1.0f;

        float x = (float) xd;
        float y = (float) yd;

        int xi = (int) x;
        int yi = (int) y;
        // Upper left corner
        float xWeight = 1 - x + xi;
        float yWeight = 1 - y + yi;
        float weight = xWeight * yWeight;
        float val = getRawDataValue(xi, yi);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }
        // upper right corner
        xi = xi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - y + yi;
        weight = xWeight * yWeight;
        val = getRawDataValue(xi, yi);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }
        // lower right corner
        yi = yi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        val = getRawDataValue(xi, yi);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }
        // lower left corner
        xi = xi - 1;
        xWeight = 1 - x + xi;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        val = getRawDataValue(xi, yi);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }

        if (missing >= 1.0 || missing > missingThreshold) {
            // compensate for missing data
            return value / missing;
        } else {
            // If we are too far from the data skip it
            return fillValue;
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
