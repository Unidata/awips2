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
package com.raytheon.uf.common.geospatial.util;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.coverage.grid.GridGeometry;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Calculates a portion of a grid geometry that overlaps an envelope in a
 * different projection. Provides convienence methods for accessing versions of
 * the GridGeometry and range that are easier to use.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 04, 2014  2672     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SubGridGeometryCalculator {

    private final Envelope envelope;

    private final GridGeometry gridGeometry;

    private final GridGeometry subGridGeometry;

    /**
     * Create a new SubGridGeometryCalculator.
     * 
     * @param envelope
     *            The area which should be included in the subgrid, all grid
     *            points within this area will be included. Grid points outside
     *            the envelope will be included only if they are needed to keep
     *            the grid evenly spaced(for example when the difference in CRS
     *            has significant warping.)
     * @param gridGeometry
     *            The full grid geometry for which a subgrid is needed. The
     *            subgrid will be defined in the same grid space as this
     *            geometry.
     * @throws TransformException
     *             If something goes wrong mapping the envelope crs onto the
     *             grid geometry crs.
     */
    public SubGridGeometryCalculator(Envelope envelope, GridGeometry gridGeometry)
            throws TransformException {
        this.envelope = envelope;
        this.gridGeometry = gridGeometry;
        this.subGridGeometry = calculate(envelope, gridGeometry);
    }

    protected static GridGeometry calculate(Envelope envelope,
            GridGeometry gridGeometry) throws TransformException {
        GridGeometry2D gg2D = GridGeometry2D.wrap(gridGeometry);
        CoordinateReferenceSystem gridCRS = gg2D.getCoordinateReferenceSystem();
        GridEnvelope2D gridRange = gg2D.getGridRange2D();
        Envelope2D gridEnv = gg2D.getEnvelope2D();
        int gridWidth = gridRange.width;
        int gridHeight = gridRange.height;
        /*
         * Use grid spacing to determine a threshold for EnvelopeIntersection.
         * This guarantees the result is within one grid cell.
         */
        double dx = gridEnv.width / gridWidth;
        double dy = gridEnv.height / gridHeight;
        double threshold = Math.max(dx, dy);
        Geometry geom = null;
        try {
            geom = EnvelopeIntersection.createEnvelopeIntersection(envelope,
                    gridEnv, threshold, gridWidth, gridHeight);
        } catch (FactoryException e) {
            throw new TransformException("Error initializing transforms.", e);
        }
        /* Convert from jts envelope to geotools envelope. */
        com.vividsolutions.jts.geom.Envelope env = geom.getEnvelopeInternal();
        Envelope2D subEnv = new Envelope2D(gridCRS, env.getMinX(),
                env.getMinY(), env.getWidth(), env.getHeight());
        GridEnvelope2D subRange = gg2D.worldToGrid(subEnv);
        /* Add a 1 pixel border so interpolation near the edges is nice */
        subRange.grow(1, 1);
        /* Make sure not to grow bigger than original grid. */
        subRange = new GridEnvelope2D(subRange.intersection(gridRange));
        if (subRange.equals(gridRange)) {
            return gridGeometry;
        }
        return new GridGeometry2D(subRange, gridGeometry.getGridToCRS(),
                gridCRS);

    }

    /**
     * @return The original envelope used to create this calculator.
     */
    public Envelope getEnvelope() {
        return envelope;
    }

    /**
     * @return The original grid geometry used to create this calculator.
     */
    public GridGeometry getGridGeometry() {
        return gridGeometry;
    }

    /**
     * @return The grid geometry describing the overlapping portion. The range
     *         of the geometry will have values that line up with the source
     *         geometry.
     */
    public GridGeometry getSubGridGeometry() {
        return subGridGeometry;
    }

    /**
     * Converts/Casts the sub grid geometry to a {@link GridGeometry2D}
     * 
     * @see #getSubGridGeometry()
     */
    public GridGeometry2D getSubGridGeometry2D() {
        return GridGeometry2D.wrap(subGridGeometry);
    }

    /**
     * Creates a new grid geometry describing the subgrid area with the minimum
     * values of the range set to 0. This type of geometry does not have
     * information about the range but is often more compatible with certain
     * code that assumes 0 minimum ranges.
     * 
     * @return
     */
    public GridGeometry2D getZeroedSubGridGeometry() {
        GridGeometry2D subGridGeometry = getSubGridGeometry2D();
        GridEnvelope2D subGridRange = subGridGeometry.getGridRange2D();
        subGridRange.x = 0;
        subGridRange.y = 0;
        return new GridGeometry2D(subGridRange, subGridGeometry.getEnvelope());
    }

    public int[] getGridRangeLow(boolean inclusive) {
        int[] low = subGridGeometry.getGridRange().getLow()
                .getCoordinateValues();
        if (!inclusive) {
            for (int i = 0; i < low.length; i += 1) {
                low[i] -= 1;
            }
        }
        return low;
    }

    public int[] getGridRangeHigh(boolean inclusive) {
        int[] high = subGridGeometry.getGridRange().getHigh()
                .getCoordinateValues();
        if (!inclusive) {
            for (int i = 0; i < high.length; i += 1) {
                high[i] += 1;
            }
        }
        return high;
    }

    /**
     * @return true if the envelope does not intersect the grid geometry.
     */
    public boolean isEmpty() {
        GridEnvelope range = subGridGeometry.getGridRange();
        for (int i = 0; i < range.getDimension(); i += 1) {
            if (range.getSpan(i) <= 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return true if the envelope intersects the entire grid geometry.
     */
    public boolean isFull() {
        return subGridGeometry == gridGeometry;
    }

}
