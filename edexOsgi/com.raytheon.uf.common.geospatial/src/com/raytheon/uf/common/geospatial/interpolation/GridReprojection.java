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

import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;

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
 * Jun 18, 2012            bsteffen    Initial creation
 * Jul 17, 2013 2185       bsteffen    Cache computed grid reprojections.
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
        DirectPosition2D dp = new DirectPosition2D(x, y);
        transform.transform(dp, dp);
        return dp;
    }

    public GeneralGridGeometry getSourceGeometry() {
        return sourceGeometry;
    }

    public GeneralGridGeometry getTargetGeometry() {
        return targetGeometry;
    }

}
