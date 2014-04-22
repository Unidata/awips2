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

import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;

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
     * GridDownscaler will use for downscaling it's source geometry. The
     * returned array will be at least of size one with the first item in the
     * array being the rectangle for the source gridGeometry
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
        downscaleSizes.add(currSize);

        while (currSize.width > minGridSize
                && currSize.height > minGridSize
                && (!checkSpacing || ((e.getSpan(0) / currSize.width) < MAX_GRID_SPACING_METERS && (e
                        .getSpan(1) / currSize.height) < MAX_GRID_SPACING_METERS))) {
            currSize = new Rectangle(currSize.width / 2, currSize.height / 2);
            downscaleSizes.add(currSize);
        }
        return downscaleSizes.toArray(new Rectangle[downscaleSizes.size()]);
    }

    private Envelope sourceEnvelope;

    private Rectangle[] downscaleGeometries;

    private Interpolation interpolation;

    /**
     * Constructs a GridDownscaler for the given source geometry using the
     * default interpolation method
     * 
     * @param sourceGeometry
     * @param dataSource
     */
    public GridDownscaler(GeneralGridGeometry sourceGeometry) {
        this(sourceGeometry, new NearestNeighborInterpolation());
    }

    /**
     * Constructs a GridDownscaler for the given source
     * {@link GeneralGridGeometry} using the specified {@link Interpolation}
     * 
     * @param sourceGeometry
     * @param interpolation
     */
    public GridDownscaler(GeneralGridGeometry sourceGeometry,
            Interpolation interpolation) {
        this.sourceEnvelope = sourceGeometry.getEnvelope();
        this.downscaleGeometries = getDownscaleSizes(sourceGeometry);
        this.interpolation = interpolation;
    }

    /**
     * Sets the {@link Interpolation} algorithm to be used when downscaling
     * 
     * @param interpolation
     */
    public void setInterpolation(Interpolation interpolation) {
        this.interpolation = interpolation;
    }

    /**
     * Returns the total number of levels, this includes the full resolution
     * grid
     * 
     * @return
     */
    public int getNumberOfDownscaleLevels() {
        return downscaleGeometries.length;
    }

    /**
     * Returns the size of the downscale level passed in.
     * 
     * @param downscaleLevel
     * @return
     */
    public Rectangle getDownscaleSize(int downscaleLevel) {
        return new Rectangle(downscaleGeometries[downscaleLevel]);
    }

    /**
     * Downscales the specified level into the destination object.
     * 
     * @param downscaleLevel
     * @param destination
     * @throws TransformException
     */
    public void downscale(int fromLevel, int toLevel, DataSource source,
            DataDestination destination) throws TransformException {
        Rectangle sourceSize = getDownscaleSize(fromLevel);
        GeneralGridGeometry sourceGeometry = new GeneralGridGeometry(
                new GridEnvelope2D(sourceSize), sourceEnvelope);
        Rectangle destSize = getDownscaleSize(toLevel);
        GeneralGridGeometry destGeometry = new GeneralGridGeometry(
                new GridEnvelope2D(destSize), sourceEnvelope);
        GridReprojection reprojection = new GridReprojection(sourceGeometry,
                destGeometry);
        try {
            reprojection.reprojectedGrid(interpolation, source, destination);
        } catch (FactoryException e) {
            throw new TransformException(
                    "Error creating transforms required for downscaling", e);
        } catch (TransformException e) {
            throw e;
        }
    }
}
