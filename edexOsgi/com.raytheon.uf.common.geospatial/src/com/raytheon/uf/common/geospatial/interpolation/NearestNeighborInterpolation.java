package com.raytheon.uf.common.geospatial.interpolation;

import org.geotools.coverage.grid.GeneralGridGeometry;

/**
 * 
 * Nearest Neighbor interpolation
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
public class NearestNeighborInterpolation extends AbstractInterpolation {

    public NearestNeighborInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        super(sourceGeometry, targetGeometry, minValid, maxValid, fillValue);
    }

    public NearestNeighborInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(sourceGeometry, targetGeometry);
    }

    public NearestNeighborInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        super(data, sourceGeometry, targetGeometry, minValid, maxValid,
                fillValue);
    }

    public NearestNeighborInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(data, sourceGeometry, targetGeometry);
    }

    @Override
    protected float getInterpolatedValue(double x, double y) {
        float val = getRawDataValue((int) Math.round(x), (int) Math.round(y));
        if (Float.isNaN(val)) {
            return fillValue;
        } else {
            return val;
        }
    }

}
