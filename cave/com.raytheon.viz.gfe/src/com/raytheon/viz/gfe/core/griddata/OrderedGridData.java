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
package com.raytheon.viz.gfe.core.griddata;

import java.awt.Point;
import java.util.Date;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Ordered grid data is an abstract class that has common functionality for both
 * scalar and vector grid data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial Class Skeleton.	
 * 04/07/2008   879        rbell       calculatePencilInfluence()
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public abstract class OrderedGridData extends AbstractGridData {
    static enum DirectionType {
        UP, UP_RIGHT, RIGHT, DOWN_RIGHT, DOWN, DOWN_LEFT, LEFT, UP_LEFT
    };

    // keep these in the same order the values in UP...UP_LEFT (0-7)
    static final int dirX[] = { 0, 1, 1, 1, 0, -1, -1, -1 };

    static final int dirY[] = { 1, 1, 0, -1, -1, -1, 0, 1 };

    static final double distXY[] = { 1, 1.414, 1, 1.414, 1, 1.414, 1, 1.414 };

    protected OrderedGridData(Parm parm, IGridSlice slice) {
        super(parm, slice);
    }

    /**
     * This function interpolates grid points based on the surrounding grid
     * points that are valid. All coordinates set to true in gridCells will be
     * interpolated and all points set to false will not be modified. This
     * function sets the new values in dataGridOut directly.
     * 
     * That way we only loop of the extrema of bits that are not set. Next loop
     * through the extrema and for each bit not set in gridCells, get the edge
     * distance and value for each of the four directions. If a good value is
     * returned, maintain a running sum of (value/distance) and (1/distance).
     * After all directions are done calculate the new value and stuff it in
     * dataGrid.
     * 
     * @param gridCells
     * @param dataGridIn
     * @param minValue
     * @param maxValue
     * @return
     */
    public void interpSpatialGap(final Grid2DBit gridCells,
            final Grid2DFloat dataGridIn, float minValue, float maxValue,
            Grid2DFloat dataGridOut) {

        // Get the extrema of the set bits
        Point lowerLeft = new Point();
        Point upperRight = new Point();
        if (!gridCells.extremaOfSetBits(lowerLeft, upperRight)) {
            return; // nothing to do
        }

        int i, j;
        float numeratorSum, denominatorSum;
        Point coord = new Point();
        DistAndValue dv;

        for (i = lowerLeft.x; i <= upperRight.x; i++) {
            for (j = lowerLeft.y; j <= upperRight.y; j++) {
                coord.x = i;
                coord.y = j;
                if (gridCells.getAsBoolean(i, j)) // interpolate this one.
                {
                    numeratorSum = denominatorSum = 0.0f;
                    for (DirectionType dir : DirectionType.values()) {
                        dv = getEdgeDistAndValue(gridCells, dataGridIn, coord,
                                dir);
                        if (dv != null) {
                            numeratorSum += dv.value / dv.distance;
                            denominatorSum += 1 / dv.distance;
                        }
                    }
                    // Make sure we don't divide by zero
                    if (denominatorSum > 0.0) {
                        float gridValue = numeratorSum / denominatorSum;
                        if (gridValue < minValue) {
                            dataGridOut.set(i, j, minValue);
                        } else if (gridValue > maxValue) {
                            dataGridOut.set(i, j, maxValue);
                        } else {
                            dataGridOut.set(i, j, gridValue);
                        }
                    }
                    // otherwise we just leave it alone
                }
            }
        }
    }

    protected class DistAndValue {
        double distance;

        float value;
    }

    /**
     * This function uses the gridCells bit array, the gridData, the coord and
     * the direction and returns the value and the distance (in grid points) of
     * the first valid grid point it finds in the specified direction. Valid
     * points are those coordinates whose value is false in gridCells. This
     * function starts at the specified coord and looks in the specified
     * direction until it finds a point that is valid. The value at this point
     * and the number of grid points searched (distance) are returned. If the
     * function bumps into the edge of the array before finding a valid point,
     * this function returns false. If a valid point is found, it returns true.
     * 
     * First define a coordinate that we will add as we search in the specified
     * direction. Then for each coordinate in the direction of the search, if
     * it's outside the boundary of the array, return false. If the the grid
     * point is set, increment the distance and the coord. If the grid point is
     * not set get the value from the float array and return.
     * 
     * @param gridCells
     * @param gridData
     * @param startCoord
     * @param direction
     * @return
     */
    protected DistAndValue getEdgeDistAndValue(final Grid2DBit gridCells,
            final Grid2DFloat gridData, final Point startCoord,
            DirectionType dir) {

        int dx = dirX[dir.ordinal()];
        int dy = dirY[dir.ordinal()];
        double incDist = distXY[dir.ordinal()];

        // Now search for the first set bit and get its value
        Point coord = new Point(startCoord);
        coord.translate(dx, dy);
        DistAndValue dv = new DistAndValue();
        dv.distance = incDist;
        while (true) {
            // Check for bounds
            if (coord.x < 0 || coord.x >= gridData.getXdim() || coord.y < 0
                    || coord.y >= gridData.getYdim()) {
                return null;
            }
            // we found one
            if (!gridCells.getAsBoolean(coord.x, coord.y)) {
                dv.value = gridData.get(coord.x, coord.y);
                return dv;
            }
            // check on either side if we're going in the diagonal direction
            if (((dx + dy) % 2) == 0) {
                if (dx == -1 && coord.x > 0) {
                    if (!gridCells.getAsBoolean(coord.x - 1, coord.y)) {
                        dv.value = gridData.get(coord.x - 1, coord.y);
                        dv.distance += incDist;
                        return dv;
                    }
                }
                if (dy == -1 && coord.y > 0) {
                    if (!gridCells.getAsBoolean(coord.x, coord.y - 1)) {
                        dv.value = gridData.get(coord.x, coord.y - 1);
                        dv.distance += incDist;
                        return dv;
                    }
                }
                if (dx == 1 && coord.x < gridCells.getXdim() - 1) {
                    if (!gridCells.getAsBoolean(coord.x + 1, coord.y)) {
                        dv.value = gridData.get(coord.x + 1, coord.y);
                        dv.distance += incDist;
                        return dv;
                    }
                }
                if (dy == 1 && coord.y < gridCells.getYdim() - 1) {
                    if (!gridCells.getAsBoolean(coord.x, coord.y + 1)) {
                        dv.value = gridData.get(coord.x, coord.y + 1);
                        dv.distance += incDist;
                        return dv;
                    }
                }
            }
            // if we made if this far increment and keep going
            dv.distance += incDist;
            coord.translate(dx, dy);
            continue;
        }
    }

    @Override
    public IGridSlice gridMin(IGridSlice gridSlice) {
        populate();
        return doGridMin(gridSlice);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#gridMax(com.raytheon.edex
     * .plugin.gfe.slice.IGridSlice)
     */
    @Override
    public IGridSlice gridMax(IGridSlice gridSlice) {
        populate();
        return doGridMax(gridSlice);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#gridSum(com.raytheon.edex
     * .plugin.gfe.slice.IGridSlice)
     */
    @Override
    public IGridSlice gridSum(IGridSlice gridSlice) {
        populate();
        return doGridSum(gridSlice);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#gridMultiply(float)
     */
    @Override
    public IGridSlice gridMultiply(float factor) {
        populate();
        return doGridMultiply(factor);
    }

    protected abstract IGridSlice doGridMin(IGridSlice aGridSlice);

    protected abstract IGridSlice doGridMax(IGridSlice aGridSlice);

    protected abstract IGridSlice doGridMultiply(float aFactor);

    protected abstract IGridSlice doGridSum(IGridSlice aGridSlice);

    /**
     * Sets the bits on all sides of the specified coordinate. The number of
     * gridCells from the coordinate is indicated by size.
     * 
     * Loop and set the gridCells checking for boundaries.
     * 
     * @param coord
     * @param size
     * @param gridCells
     */
    protected void setInfluence(Point coord, int size, Grid2DBit gridCells) {
        int i, j;
        for (i = coord.x - size / 2; i <= coord.x + size / 2; i++) {
            for (j = coord.y - size / 2; j <= coord.y + size / 2; j++) {
                if (gridCells.isValid(i, j)) {
                    gridCells.set(i, j);
                }
            }
        }
    }

    @Override
    protected abstract Grid2DBit doPencilStretch(Date time, WxValue value,
            Coordinate path[], Grid2DBit editArea);

    /**
     * Calculates the set of grid cells to be interpolated after a pencil tool
     * operation. This function is shared by scalar and vector GridData classes.
     * Returns a Grid2DBit that indicates those grid cells that are to be
     * interpolated (1 = interpolate 0 = do not interpolate).
     * 
     * First get the grid domain min and max of points and the min and max value
     * for the set of points. Then set all of the points within the domain to be
     * interpolated if their values underneath fall within the min and max rage
     * calculated earlier. Finally clear all of the grid cells defined by points
     * so they are not interpolated so those points will be used when the
     * spatial gap is filled.
     * 
     * @param points
     * @param grid
     * @return
     */
    public Grid2DBit calculatePencilInfluence(final Point[] points,
            final Grid2DFloat grid) {

        // Get min and max extent of the points and the data that lies beneath.
        Point lowerLeft = this.getParm().getGridInfo().getGridLoc().gridSize();
        Point upperRight = new Point(0, 0);
        float minValue = this.getMaxValue();
        float maxValue = this.getMinValue();
        int i, j;
        float thisVal;

        for (i = 0; i < points.length; i++) {
            // Check and set the domain extrema
            if (points[i].x < lowerLeft.x) {
                lowerLeft.x = points[i].x;
            }
            if (points[i].x > upperRight.x) {
                upperRight.x = points[i].x;
            }
            if (points[i].y < lowerLeft.y) {
                lowerLeft.y = points[i].y;
            }
            if (points[i].y > upperRight.y) {
                upperRight.y = points[i].y;
            }

            // Check and set the value extrema
            thisVal = grid.get(points[i].x, points[i].y);
            if (thisVal > maxValue) {
                maxValue = thisVal;
            }
            if (thisVal < minValue) {
                minValue = thisVal;
            }
        }

        // Make the Grid2DBit and set the bits appropriately
        Grid2DBit gridCells = new Grid2DBit(this.getParm().getGridInfo()
                .getGridLoc().gridSize().x, this.getParm().getGridInfo()
                .getGridLoc().gridSize().y);

        // Get the influence from the parmState
        int influence = this.parm.getParmState().getPencilWidth();

        // Set all points within the domain whose values are b/w min & max
        Point coord = new Point();
        for (i = lowerLeft.x; i <= upperRight.x; i++) {
            for (j = lowerLeft.y; j <= upperRight.y; j++) {
                if ((grid.get(i, j) >= minValue)
                        && (grid.get(i, j) <= maxValue)) {
                    coord.x = i;
                    coord.y = j;
                    setInfluence(coord, influence, gridCells);
                }
            }
        }

        // Finally make sure path points won't be interpolated
        for (i = 0; i < points.length; i++) {
            gridCells.clear(points[i].x, points[i].y);
        }

        return gridCells;
    }

    protected float getMinValue() {
        return this.getParm().getGridInfo().getMinValue();
    }

    protected float getMaxValue() {
        return this.getParm().getGridInfo().getMaxValue();
    }

}
