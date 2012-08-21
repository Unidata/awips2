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

import java.awt.geom.Point2D;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.interpolation.data.DataDestination;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;

/**
 * Class for mapping data from one grid geometry to another. Uses interfaces for
 * source and destination so that reprojection can be done regardless of the
 * original format of the raw data or the desired format of new data.
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

public class GridReprojection {

    protected GeneralGridGeometry sourceGeometry;

    protected GeneralGridGeometry targetGeometry;

    protected int targetNx;

    protected int targetNy;

    protected MathTransform transform;

    protected float[] transformTable = null;

    public GridReprojection(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        this.sourceGeometry = sourceGeometry;
        this.targetGeometry = targetGeometry;
        this.targetNx = targetGeometry.getGridRange().getSpan(0);
        this.targetNy = targetGeometry.getGridRange().getSpan(1);
    }

    protected void initTransforms() throws FactoryException, TransformException {
        if (transform == null) {
            CoordinateReferenceSystem targetCRS = targetGeometry
                    .getCoordinateReferenceSystem();
            CoordinateReferenceSystem sourceCRS = sourceGeometry
                    .getCoordinateReferenceSystem();
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

    public <T extends DataDestination> T reprojectedGrid(
            Interpolation interpolation, DataSource source, T dest)
            throws FactoryException, TransformException {
        return reprojectedGrid(new GridSampler(source, interpolation), dest);
    }

    public <T extends DataDestination> T reprojectedGrid(GridSampler sampler,
            T dest) throws FactoryException, TransformException {
        for (int j = 0; j < targetNy; j++) {
            for (int i = 0; i < targetNx; i++) {
                dest.setDataValue(reprojectedGridCell(sampler, i, j), i, j);
            }
        }
        return dest;
    }

    public double reprojectedGridCell(GridSampler sampler, int x, int y)
            throws FactoryException, TransformException {
        Point2D.Double dp = null;
        try {
            dp = getReprojectDataPoint(x, y);
        } catch (ProjectionException e) {
            // ProjectionException is thrown when a point is outside
            // the valid range of the source data, so we will treat
            // it like other out of range values and set it to fill
            // value.
            return Double.NaN;
        }

        return sampler.sample(dp.x, dp.y);
    }

    protected Point2D.Double getReprojectDataPoint(int x, int y)
            throws TransformException, FactoryException {
        initTransforms();
        if (transformTable != null && x >= 0 && x < targetNx && y >= 0
                && y < targetNy) {
            int index = (y * targetNx + x) * 2;
            float xVal = transformTable[index];
            float yVal = transformTable[index + 1];
            if (!Float.isNaN(xVal) && !Float.isNaN(yVal)) {
                return new Point2D.Double(xVal, yVal);
            }
        }
        DirectPosition2D dp = new DirectPosition2D(x, y);
        transform.transform(dp, dp);
        return dp;
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
        try {
            transform.transform(transformTable, 0, transformTable, 0, targetNy
                    * targetNx);
        } catch (ProjectionException e) {
            ;// Ignore, the points in the transformTable that are invalid are
             // set to NaN, no other action is necessary.
        }
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

    public GeneralGridGeometry getSourceGeometry() {
        return sourceGeometry;
    }

    public GeneralGridGeometry getTargetGeometry() {
        return targetGeometry;
    }

}
