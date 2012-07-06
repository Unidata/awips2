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

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import javax.measure.unit.SI;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.cs.CoordinateSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.interpolation.data.DataDestination;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;

/**
 * Class used to create downscaled versions of geospatial data. Downscales to an
 * optimal size
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GridDownscaler {

    /** All sides of grid range must be at least this many pixels */
    private static final int MIN_GRID_SIZE = 32;

    /** Will continue to downscale until grid spacing is larger than this */
    private static final double MAX_GRID_SPACING_METERS = 20 * 1000;

    /**
     * Get the {@link GeneralGridGeometry} [] of downscale levels that the
     * GridDownscaler will use for downscaling it's source geometry. This array
     * will NOT include the original source geometry in it but instead will
     * contain only the downscaled geomerties. An empty array will be returned
     * if no downscaling is recommended
     * 
     * @param gridGeometry
     * @return
     */
    public static Rectangle[] getDownscaleSizes(GeneralGridGeometry gridGeometry) {
        CoordinateSystem cs = gridGeometry.getCoordinateReferenceSystem()
                .getCoordinateSystem();
        double minGridSize = MIN_GRID_SIZE;
        boolean checkSpacing = true;
        if (SI.METER.equals(cs.getAxis(0).getUnit()) == false
                || SI.METER.equals(cs.getAxis(1).getUnit()) == false) {
            // One of the axes is not in meter spacing, default to 512
            // minGridSize and ignore spacing like pre-spatial-aware downscaling
            // code
            minGridSize = 512;
            checkSpacing = false;
        }

        List<Rectangle> downscaleSizes = new ArrayList<Rectangle>();
        GridEnvelope ge = gridGeometry.getGridRange();
        Envelope e = gridGeometry.getEnvelope();
        Rectangle currSize = new Rectangle(ge.getSpan(0), ge.getSpan(1));
        while (currSize.width > minGridSize
                && currSize.height > minGridSize
                && (!checkSpacing || ((e.getSpan(0) / currSize.width) < MAX_GRID_SPACING_METERS && (e
                        .getSpan(1) / currSize.height) < MAX_GRID_SPACING_METERS))) {
            currSize = new Rectangle(currSize.width / 2, currSize.height / 2);
            downscaleSizes.add(currSize);
        }
        return downscaleSizes.toArray(new Rectangle[downscaleSizes.size()]);
    }

    private DataSource dataSource;

    private GeneralGridGeometry sourceGeometry;

    private Rectangle[] downscaleGeometries;

    private Interpolation interpolation;

    /**
     * Constructs a GridDownscaler for the given source geometry and data source
     * using the default interpolation method
     * 
     * @param sourceGeometry
     * @param dataSource
     */
    public GridDownscaler(GeneralGridGeometry sourceGeometry,
            DataSource dataSource) {
        this(sourceGeometry, dataSource, new NearestNeighborInterpolation());
    }

    /**
     * Constructs a GridDownscaler for the given source
     * {@link GeneralGridGeometry} and {@link DataSource} using the specified
     * {@link Interpolation}
     * 
     * @param sourceGeometry
     * @param dataSource
     * @param interpolation
     */
    public GridDownscaler(GeneralGridGeometry sourceGeometry,
            DataSource dataSource, Interpolation interpolation) {
        this.sourceGeometry = sourceGeometry;
        this.dataSource = dataSource;
        this.downscaleGeometries = getDownscaleSizes(sourceGeometry);
        this.interpolation = interpolation;
    }

    public int getNumberOfDownscaleLevels() {
        return downscaleGeometries.length;
    }

    public Rectangle getDownscaleSize(int downscaleLevel) {
        return downscaleGeometries[downscaleLevel];
    }

    /**
     * Downscales the specified level into the destination object
     * 
     * @param downscaleLevel
     * @param destination
     * @throws TransformException
     */
    public void downscale(int downscaleLevel, DataDestination destination)
            throws TransformException {
        Rectangle destSize = getDownscaleSize(downscaleLevel);
        GeneralGridGeometry destGeometry = new GeneralGridGeometry(
                new GridEnvelope2D(destSize), sourceGeometry.getEnvelope());
        GridReprojection reprojection = new GridReprojection(sourceGeometry,
                destGeometry);
        try {
            reprojection
                    .reprojectedGrid(interpolation, dataSource, destination);
        } catch (FactoryException e) {
            throw new TransformException(
                    "Error creating transforms required for downscaling", e);
        } catch (TransformException e) {
            throw e;
        }
    }
}
