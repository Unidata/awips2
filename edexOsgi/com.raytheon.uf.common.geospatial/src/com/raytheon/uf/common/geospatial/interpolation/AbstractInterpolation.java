package com.raytheon.uf.common.geospatial.interpolation;

import java.awt.geom.Point2D;
import java.awt.image.Raster;

import org.apache.commons.lang.Validate;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

/**
 * 
 * Abstract class for mapping data from one grid geometry to another
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
public abstract class AbstractInterpolation {

    protected float[] data;

    protected GeneralGridGeometry sourceGeometry;

    protected GeneralGridGeometry targetGeometry;

    protected float minValid = Float.NEGATIVE_INFINITY;

    protected float maxValid = Float.POSITIVE_INFINITY;

    protected float fillValue = Float.NaN;

    // The following were extracted out of the geometries and stored for easy
    // use

    protected CoordinateReferenceSystem targetCRS;

    protected CoordinateReferenceSystem sourceCRS;

    protected int targetNx;

    protected int targetNy;

    protected int sourceNx;

    protected int sourceNy;

    protected MathTransform transform;

    protected float[] transformTable = null;

    protected AbstractInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        this.sourceGeometry = sourceGeometry;
        this.targetGeometry = targetGeometry;
        // Extract some local variables
        targetCRS = targetGeometry.getCoordinateReferenceSystem();
        sourceCRS = sourceGeometry.getCoordinateReferenceSystem();
        targetNx = targetGeometry.getGridRange().getSpan(0);
        targetNy = targetGeometry.getGridRange().getSpan(1);
        sourceNx = sourceGeometry.getGridRange().getSpan(0);
        sourceNy = sourceGeometry.getGridRange().getSpan(1);
    }

    protected AbstractInterpolation(GridCoverage2D gridCoverage,
            GeneralGridGeometry targetGeometry) {
        this(gridCoverage.getGridGeometry(), targetGeometry);
        Raster data = gridCoverage.getRenderedImage().getData();
        this.data = data.getPixels(data.getMinX(), data.getMinY(),
                data.getWidth(), data.getHeight(), this.data);
    }

    protected AbstractInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        this(sourceGeometry, targetGeometry);
        this.data = data;
    }

    protected AbstractInterpolation(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        this(sourceGeometry, targetGeometry);
        this.minValid = minValid;
        this.maxValid = maxValid;
        this.fillValue = fillValue;
    }

    /**
     * Remap data from sourceGeometry to targetGeometry.
     * 
     * @param data
     *            the raw data
     * @param sourceGeometry
     *            a geometry describing rawData
     * @param targetGeometry
     *            a geometry describing the result of this function
     * @param minValid
     *            the minimum valid value for the data, anything less will be
     *            treated as NaN, for all real numbers use
     *            Float.NEGATIVE_INFINITY
     * @param maxValid
     *            the maximum valid value for the data, anything greater will be
     *            treated as NaN, for all real numbers use
     *            Float.POSITIVE_INFINITY
     * @param fillValue
     *            the value to fill in for any data out of range, Float.NaN is
     *            the recommended fill value
     */
    protected AbstractInterpolation(float[] data,
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry, float minValid, float maxValid,
            float fillValue) {
        this(data, sourceGeometry, targetGeometry);
        this.minValid = minValid;
        this.maxValid = maxValid;
        this.fillValue = fillValue;
    }

    protected void initTransforms() throws FactoryException, TransformException {
        if (transform == null) {
            MathTransform grid2crs = targetGeometry
                    .getGridToCRS(PixelInCell.CELL_CENTER);
            MathTransform crs2crs = CRS.findMathTransform(targetCRS, sourceCRS);
            MathTransform crs2grid = sourceGeometry.getGridToCRS(
                    PixelInCell.CELL_CENTER).inverse();
            DefaultMathTransformFactory mtf = new DefaultMathTransformFactory();
            transform = mtf.createConcatenatedTransform(grid2crs,
                    mtf.createConcatenatedTransform(crs2crs, crs2grid));
        }
    }

    public float[] getReprojectedGrid() throws FactoryException,
            TransformException {
        Validate.notNull(data);
        float[] newData = new float[targetNx * targetNy];
        if (transformTable == null) {
            for (int j = 0; j < targetNy; j++) {
                for (int i = 0; i < targetNx; i++) {
                    newData[j * targetNx + i] = getReprojectedGridCell(i, j);
                }
            }
        } else {
            // This version is equivalent but faster since it can skip range
            // checks
            int tIndex = 0;
            for (int i = 0; i < newData.length; i++) {
                float x = transformTable[tIndex++];
                float y = transformTable[tIndex++];
                newData[i] = getInterpolatedValue(x, y);
            }
        }
        return newData;
    }

    /**
     * 
     * @param x
     * @param y
     * @return
     * @throws FactoryException
     * @throws TransformException
     */
    public float getReprojectedGridCell(int x, int y) throws FactoryException,
            TransformException {
        Validate.notNull(data);

        Point2D.Double dp = getReprojectDataPoint(x, y);
        return getInterpolatedValue(dp.x, dp.y);
    }

    /**
     * Given an x and y in source grid space return the interpolated value or
     * fillValue if interpolation fails.
     * 
     * @param x
     * @param y
     * @return
     */
    protected abstract float getInterpolatedValue(double x, double y);

    /**
     * does range x,y, and data range checking for x and y in source grid space,
     * returns a real data value or NaN if out of any bounds.
     * 
     * @param x
     * @param y
     * @return
     */
    protected float getRawDataValue(int x, int y) {
        if (y < 0 || y > sourceNy - 1) {
            // outside y range
            return Float.NaN;
        } else if (x < 0 || x > sourceNx - 1) {
            // outside xRange
            return Float.NaN;
        } else {
            float val = data[(int) (y * sourceNx + x)];
            if (val < minValid || val > maxValid) {
                // skip outside valid range
                val = Float.NaN;
            }
            return val;
        }
    }

    protected Point2D.Double getReprojectDataPoint(int x, int y)
            throws TransformException, FactoryException {
        initTransforms();
        if (transformTable != null && x >= 0 && x < targetNx && y >= 0
                && y < targetNy) {
            int index = (y * targetNx + x) * 2;
            float xVal = transformTable[index];
            float yVal = transformTable[index + 1];
            return new Point2D.Double(xVal, yVal);
        } else {
            DirectPosition2D dp = new DirectPosition2D(x, y);
            transform.transform(dp, dp);
            return dp;
        }
    }

    /**
     * @return the sourceGeometry
     */
    public GeneralGridGeometry getSourceGeometry() {
        return sourceGeometry;
    }

    /**
     * @return the targetGeometry
     */
    public GeneralGridGeometry getTargetGeometry() {
        return targetGeometry;
    }

    /**
     * Can be used to run interpolation for the same geometries for multiple
     * data sets.
     * 
     * @param data
     *            the data to set
     */
    public void setData(float[] data) {
        this.data = data;
    }

    /**
     * This function precomputes the math transform for all grid cells in the
     * target grid range. This method is recommended when you are reprojecting
     * multiple datasets using the same interpolation. Precalculating this table
     * takes time and uses more memory but cuts the time to perform
     * interpolation significantly.
     * 
     * 
     * @return the size in bytes of the extra memory used by the transform
     *         table.
     * @throws FactoryException
     * @throws TransformException
     */
    public int computeTransformTable() throws FactoryException,
            TransformException {
        initTransforms();
        float[] transformTable = new float[targetNy * targetNx * 2];
        int index = 0;
        for (int j = 0; j < targetNy; j++) {
            for (int i = 0; i < targetNx; i++) {
                transformTable[index++] = i;
                transformTable[index++] = j;
            }
        }
        transform.transform(transformTable, 0, transformTable, 0, targetNy
                * targetNx);
        this.transformTable = transformTable;
        return transformTable.length * 4;
    }

    /**
     * delete the transform table, freeing up memory but slowing down any future
     * reprojections
     */
    public void clearTransformTable() {
        transformTable = null;
    }
}
