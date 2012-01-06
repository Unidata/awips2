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
package com.raytheon.uf.viz.npp.viirs.rsc;

import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.Matrix;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataMathTransform implements MathTransform {

    private float[] latitudes;

    private float[] longitudes;

    private int width, height;

    public VIIRSDataMathTransform(float[][] projectionData, int width,
            int height) {
        this.longitudes = projectionData[0];
        this.latitudes = projectionData[1];
        this.height = height;
        this.width = width;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.opengis.referencing.operation.MathTransform#derivative(org.opengis
     * .geometry.DirectPosition)
     */
    @Override
    public Matrix derivative(DirectPosition arg0)
            throws MismatchedDimensionException, TransformException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.opengis.referencing.operation.MathTransform#getSourceDimensions()
     */
    @Override
    public int getSourceDimensions() {
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.opengis.referencing.operation.MathTransform#getTargetDimensions()
     */
    @Override
    public int getTargetDimensions() {
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.opengis.referencing.operation.MathTransform#inverse()
     */
    @Override
    public MathTransform inverse() throws NoninvertibleTransformException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.opengis.referencing.operation.MathTransform#isIdentity()
     */
    @Override
    public boolean isIdentity() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.opengis.referencing.operation.MathTransform#toWKT()
     */
    @Override
    public String toWKT() throws UnsupportedOperationException {
        System.out.println("toWKT?");
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.opengis.referencing.operation.MathTransform#transform(org.opengis
     * .geometry.DirectPosition, org.opengis.geometry.DirectPosition)
     */
    @Override
    public DirectPosition transform(DirectPosition arg0, DirectPosition arg1)
            throws MismatchedDimensionException, TransformException {
        System.out.println("transform a bunch of DirectPositions?");
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.opengis.referencing.operation.MathTransform#transform(double[],
     * int, double[], int, int)
     */
    @Override
    public void transform(double[] in, int inOffset, double[] out,
            int outOffset, int numPoints) throws TransformException {
        for (int i = 0; i < numPoints; ++i) {
            int xIdx = (i * 2);
            int yIdx = (i * 2) + 1;

            double xLoc = in[xIdx] - 0.5;
            // TODO: Why is data flipped along "y" axis?
            double yLoc = height - in[yIdx] - 0.5;
            out[xIdx] = getInterpolatedValue(xLoc, yLoc, 0);
            out[yIdx] = getInterpolatedValue(xLoc, yLoc, 1);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.opengis.referencing.operation.MathTransform#transform(float[],
     * int, float[], int, int)
     */
    @Override
    public void transform(float[] arg0, int arg1, float[] arg2, int arg3,
            int arg4) throws TransformException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.opengis.referencing.operation.MathTransform#transform(float[],
     * int, double[], int, int)
     */
    @Override
    public void transform(float[] arg0, int arg1, double[] arg2, int arg3,
            int arg4) throws TransformException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.opengis.referencing.operation.MathTransform#transform(double[],
     * int, float[], int, int)
     */
    @Override
    public void transform(double[] arg0, int arg1, float[] arg2, int arg3,
            int arg4) throws TransformException {
    }

    protected float getInterpolatedValue(double xd, double yd, int dim) {
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
        float val = getRawDataValue(xi, yi, dim);
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
        val = getRawDataValue(xi, yi, dim);
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
        val = getRawDataValue(xi, yi, dim);
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
        val = getRawDataValue(xi, yi, dim);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
        }

        return value / missing;
    }

    /**
     * @param xi
     * @param yi
     * @param dim
     * @return
     */
    private float getRawDataValue(int xi, int yi, int dim) {
        Coordinate c = null;
        if (xi >= 0 && xi < width && yi >= 0 && yi < height) {
            c = index(xi, yi);
        } else {
            int xInc = 0;
            int closestX = xi;
            if (closestX < 0) {
                closestX = 0;
                xInc = 1;
            } else if (closestX >= width) {
                closestX = width - 1;
                xInc = -1;
            }

            int yInc = 0;
            int closestY = yi;
            if (closestY < 0) {
                closestY = 0;
                yInc = 1;
            } else if (closestY >= height) {
                closestY = height - 1;
                yInc = -1;
            }

            Coordinate a = index(closestX, closestY);
            Coordinate b = index(closestX + xInc, closestY + yInc);
            LineSegment ls = new LineSegment(a, b);
            int xDiff = closestX - xi;
            int yDiff = closestY - yi;
            c = ls.pointAlong(-Math.sqrt(xDiff * xDiff + yDiff * yDiff));
        }
        return (float) (dim == 0 ? c.x : c.y);
    }

    private Coordinate index(int xi, int yi) {
        return new Coordinate(longitudes[yi * width + xi], latitudes[yi * width
                + xi]);
    }
}