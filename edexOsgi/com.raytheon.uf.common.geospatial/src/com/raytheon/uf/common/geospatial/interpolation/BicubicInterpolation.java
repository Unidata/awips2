package com.raytheon.uf.common.geospatial.interpolation;

import org.geotools.coverage.grid.GeneralGridGeometry;

/**
 * 
 * bicubic convolusion.
 * 
 * <pre>
 * http://en.wikipedia.org/wiki/Bicubic_interpolation#Bicubic_convolution_algorithm
 * http://docs.oracle.com/cd/E17802_01/products/products/java-media/jai/forDevelopers/jai-apidocs/javax/media/jai/InterpolationBicubic.html
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class BicubicInterpolation extends AbstractInterpolation {

    private double a = -0.5;

    public BicubicInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        super(sourceGeometry, targetGeometry, minValid, maxValid, fillValue);
    }

    public BicubicInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(sourceGeometry, targetGeometry);
    }

    public BicubicInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        super(data, sourceGeometry, targetGeometry, minValid, maxValid,
                fillValue);
    }

    public BicubicInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(data, sourceGeometry, targetGeometry);
    }

    /**
     * Should be -0.5, -0.75, or -1.0, or maybe something inbetween?
     * 
     * @param a
     */
    public void setA(double a) {
        this.a = a;
    }

    @Override
    protected float getInterpolatedValue(double x, double y) {
        double value = 0.0f;
        double weight = 0.0f;

        double xd = ((int) x) - x;
        double yd = ((int) y) - y;

        for (int xi = -1; xi <= 2; xi += 1) {
            for (int yi = -1; yi <= 2; yi += 1) {
                double xWeight = weight(xd + xi);
                double yWeight = weight(yd + yi);
                double w = xWeight * yWeight;
                double val = getRawDataValue((int) x + xi, (int) y + yi);
                value += w * val;
                weight += w;
            }
        }
        return (float) (value / weight);
    }

    private double weight(double dist) {
        if (dist < 0) {
            dist = -dist;
        }
        if (dist <= 1) {
            return (a + 2) * dist * dist * dist - (a + 3) * dist * dist + 1.0;
        } else if (dist > 1 && dist <= 2) {
            return a * dist * dist * dist - 5 * a * dist * dist + 8 * a * dist
                    - 4 * a;
        }
        return 0.0f;

    }

}
