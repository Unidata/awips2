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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IContinuousSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IISCDataAccess;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState.VectorMode;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Defines a set of common methods for all GridData implementations
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial Class Skeleton.
 * 03/13/2008   879        rbell       Legacy conversion.
 * 06/10/2009   2159       rjpeter     Updated isValid to call gridSlice.isValid
 * 02/19/2013   1637       randerso    Added throws declarations to translateDataFrom
 * 04/15/2013   1892       randerso    Adding logging to help determine what is different in the gridInfos
 *                                     Changed how gridInfo is retrieved which seems to have fixed the problem
 * 04/23/2013   1949       rjpeter     Removed validation on copy, source is verified on store.

 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public abstract class AbstractGridData implements IGridData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGridData.class);

    protected Parm parm;

    protected IGridSlice gridSlice;

    protected boolean iscCapable = true;

    protected long lastAccessTime;

    protected Grid2DBit changedPoints;

    protected AbstractGridData(Parm aParm, IGridSlice aSlice) {
        this.lastAccessTime = System.currentTimeMillis();
        this.parm = aParm;
        this.gridSlice = aSlice;
        // this.gridSlice.setUseCache(true);
        this.changedPoints = new Grid2DBit();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#changeParmAssociation(com
     * .raytheon.viz.gfe.core.parm.Parm)
     */
    @Override
    public boolean changeParmAssociation(Parm newParm) {
        if (newParm != null) {
            parm = newParm;
            gridSlice.getGridInfo().resetParmID(parm.getGridInfo().getParmID());
        }
        return false;
    }

    /**
     * Make grid data with the corresponding gridSlice and parm
     * 
     * @param parm
     * @param gridSlice
     * @return
     */
    public static IGridData makeGridData(Parm parm, IGridSlice slice) {
        if (slice == null) {
            throw new IllegalArgumentException("Null GridSlice not permitted");
        }

        switch (slice.getGridInfo().getGridType()) {
        case SCALAR:
            return new ScalarGridData(parm, slice);
        case VECTOR:
            return new VectorGridData(parm, slice);
        case WEATHER:
            return new WeatherGridData(parm, slice);
        case DISCRETE:
            return new DiscreteGridData(parm, slice);
        default:
            throw new IllegalArgumentException("Unsupported gridSlice type: "
                    + slice.getGridInfo().getGridType());
        }
    }

    @Override
    public long getLastAccessTime() {
        return this.lastAccessTime;
    }

    @Override
    public Parm getParm() {
        return this.parm;
    }

    @Override
    public IGridSlice getGridSlice() {
        populate();
        return this.gridSlice;
    }

    @Override
    public boolean isUserModified() {
        GridDataHistory[] history = this.getHistory();
        if (history.length == 0) {
            return false;
        }
        for (GridDataHistory hist : history) {
            if (hist.getTimeModified() != null) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean isValid() {
        populate();
        return doValid();
    }

    protected abstract boolean doValid();

    @Override
    public void populate() {
        if (!this.isPopulated()) {
            try {
                this.parm.populateGrid(this);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return;
            }
        }

        this.lastAccessTime = System.currentTimeMillis();
    }

    @Override
    public boolean isOkToEdit() {
        return this.parm.isOkToEdit(getGridTime());
    }

    @Override
    public boolean isSupportedEditOp(EditOp editOp) {
        return true;
    }

    /**
     * Checks if the GridData is set up properly for editing.
     * 
     * Uses okayToEdit() and checks the size of the changed points Grid2DBit.
     * This routine will return false if the proper protocol has not been
     * followed, i.e., startParmEdit() before this operation.
     * 
     */
    protected void checkOkayForEdit() {
        if (!isOkToEdit()) {
            throw new IllegalStateException(
                    "Attempt to set() when not okay to edit");
        }
        if (!this.changedPoints.isValid()) {
            throw new IllegalStateException(
                    "Attempt to set() without startGridEdit()");
        }
        return;
    }

    @Override
    public Grid2DBit endGridEdit() {
        Grid2DBit rVal = this.changedPoints;
        this.changedPoints = new Grid2DBit();
        return rVal;
    }

    @Override
    public void startGridEdit() {
        this.populate();
        Point size = new Point(this.gridSlice.getGridInfo().getGridLoc()
                .getNx().intValue(), this.gridSlice.getGridInfo().getGridLoc()
                .getNy().intValue());
        this.changedPoints = new Grid2DBit(size.x, size.y);
    }

    /**
     * Resets the save and publish times in the history. Returns true for
     * success.
     * 
     * implementation:
     * 
     * Resets the fields, then notifies the parm that history has updated.
     * 
     */
    @Override
    public void resetSavePublishHistory() {
        for (GridDataHistory history : this.gridSlice.getHistory()) {
            history.setUpdateTime(null);
            history.setPublishTime(null);
            history.setLastSentTime(null);
        }
        this.parm.gridHistoryChanged(this);
    }

    /**
     * Sets the save times in the history. Returns true for success.
     * 
     * @return true if the method succeeded
     */
    @Override
    public boolean setSaveHistory() {
        Date current = SimulatedTime.getSystemTime().getTime();
        for (int i = 0; i < this.getHistory().length; i++) {
            this.getHistory()[i].setUpdateTime((Date) current.clone());
        }

        this.getParm().gridHistoryChanged(this);

        return true;
    }

    /**
     * Updates the history to indicate grid has been modified.
     * 
     * @param modifier
     *            the modifier
     * 
     */
    @Override
    public void updateHistoryToModified(WsId modifier) {
        Date now = SimulatedTime.getSystemTime().getTime();
        for (int i = 0; i < getHistory().length; i++) {
            getHistory()[i].setModified(modifier);
            getHistory()[i].setTimeModified((Date) now.clone());
        }
        this.parm.gridHistoryChanged(this);
    }

    /**
     * Copies the given grid's data values into this grid. Any projection or
     * translation necessary are performed (due to different grid sizes or world
     * coordinate domains). The source grid must be of type and share the same
     * units. If the source grid is not compatible or the data values cannot be
     * copied, then false is returned. Note that this grid's time range is not
     * changed. Data values are adjusted to fit within the destination grid's
     * limits. Units are changed if necessary.
     * 
     * @param sourceGrid
     *            the original grid
     */
    @Override
    public boolean copyGridValues(final IGridData sourceGrid) {
        populate();

        // validate data type
        if (sourceGrid.getParm().getGridInfo().getGridType() != getParm()
                .getGridInfo().getGridType()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Attempt to copyGridValues of different parm types on "
                            + getParm().getParmID() + " and "
                            + sourceGrid.getParm().getParmID());
            return false;
        }

        // validate units same or can be converted
        if (!getParm()
                .getGridInfo()
                .getUnitObject()
                .isCompatible(
                        sourceGrid.getParm().getGridInfo().getUnitObject())) {
            statusHandler.handle(Priority.PROBLEM,
                    "Attempt to copyGridValues of different units: "
                            + getParm().getGridInfo().getUnitObject()
                            + " vs. "
                            + sourceGrid.getParm().getGridInfo()
                                    .getUnitObject());
            return false;
        }

        // perform translation
        try {
            return translateDataFrom(sourceGrid);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error translating data", e);
            return false;
        }

    }

    /**
     * Clone the object, returning a copy.
     * 
     * @return the copy
     * @throws CloneNotSupportedException
     */
    @Override
    public abstract IGridData clone() throws CloneNotSupportedException;

    protected abstract boolean translateDataFrom(final IGridData source)
            throws FactoryException, TransformException;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#setValue(com.raytheon.viz
     * .gfe.core.wxvalue.WxValue, com.raytheon.edex.grid.Grid2DBit)
     */
    @Override
    public boolean setValue(WxValue aValue, Grid2DBit editArea) {
        populate();
        checkOkayForEdit();

        // Make the change
        return setChangedPoints(doSet(aValue, editArea));

    }

    protected abstract Grid2DBit doDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange);

    protected abstract Grid2DBit doSet(WxValue aValue, Grid2DBit editArea);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#updateHistory(com.raytheon
     * .edex.plugin.gfe.GridDataHistory)
     */
    @Override
    public boolean updateHistory(GridDataHistory history) {
        return updateHistory(new GridDataHistory[] { history });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#updateHistory(com.raytheon
     * .edex.plugin.gfe.GridDataHistory[])
     */
    @Override
    public boolean updateHistory(GridDataHistory[] history) {
        if (history.length == 0) {
            return false;
        }
        if (this.gridSlice.getHistory() != history) {
            ArrayList<GridDataHistory> thisGDHA = new ArrayList<GridDataHistory>();

            // add one by one to eliminate any duplicates
            for (GridDataHistory element : history) {
                boolean found = false;
                for (GridDataHistory thisGDH : thisGDHA) {
                    if (element.equals(thisGDH)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    thisGDHA.add(element);
                }
            }
            this.gridSlice.setHistory(thisGDHA
                    .toArray(new GridDataHistory[thisGDHA.size()]));
            this.parm.gridHistoryChanged(this);
        }
        return true;
    }

    // -- protected
    // --------------------------------------------------------------
    // GridData::taperGrid()
    // Tapers the grid based on the specified editArea Grid2DBit.
    // -- implementation
    // ---------------------------------------------------------
    // Get cells along the fringe using the fringe function. If the fringe is
    // nearly all of the cells in editArea, return, since we don't want to taper
    // everything. If the ratio of the number of edge cells to number of
    // editArea cells is less than half, get the edge of what's left to taper
    // two cells deep. Call doSmooth with the edge cells and return.
    // ---------------------------------------------------------------------------
    protected void taperGrid(final Date time, final Grid2DBit editArea) {
        // Determine the cells that will be tapered.
        Grid2DBit edge = fringe(editArea);
        int editAreaCount = editArea.numberOfBitsSet();
        int edgeCount = edge.numberOfBitsSet();

        if (editAreaCount == 0) {
            return;
        }

        if (edgeCount / editAreaCount > 0.9) {
            return;
        } else if (edgeCount / editAreaCount < 0.5) {
            edge = edge.or(fringe(editArea.xor(edge)));
        }

        smooth(time, edge);

        return;
    }

    @Override
    public boolean applyDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange) {
        if (delta == 0.0) {
            return true; // nothing to change
        }
        populate();
        checkOkayForEdit();

        // Make the change and remember the changes.
        return setChangedPoints(doDelta(time, delta, taper, pointsToChange));
    }

    // -- protected
    // --------------------------------------------------------------
    // GridData::computeTaperGrid()
    // Returns a function in the form of a Grid2D<float> based on the size of
    // the grid and the specified location.
    //
    // For each coord, calculate the distance from the center and distance to
    // the edge. The taper value at each point is defined as the ratio of the
    // distance to the center to the distance to the edge.
    // ---------------------------------------------------------------------------
    protected Grid2DFloat computeTaperGrid(final Point center,
            final Grid2DBit area) {
        Grid2DFloat taper = new Grid2DFloat(area.getXdim(), area.getYdim());
        Point location = new Point();
        Point edge;
        Point ll = new Point();
        Point ur = new Point();

        if (!area.extremaOfSetBits(ll, ur)) {
            return taper;
        }

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                location.x = i;
                location.y = j;
                edge = getEdge(center, location, area);

                // Get the distance from center to edge
                Point delta = new Point();
                delta.x = edge.x - center.x;
                delta.y = edge.y - center.y;
                float totalDist = (float) Math.sqrt(delta.x * delta.x + delta.y
                        * delta.y);

                // Get the distance from the edge to current location
                delta.x = i - edge.x;
                delta.y = j - edge.y;
                float dist = (float) Math.sqrt(delta.x * delta.x + delta.y
                        * delta.y);

                // Taper value is the ratio of dist to total
                if (totalDist == 0.0) {
                    taper.set(i, j, 1.0f);
                } else {
                    taper.set(i, j, dist / totalDist);
                }
            }
        }

        return taper;
    }

    // -- protected
    // --------------------------------------------------------------
    // GridData::getEdge()
    // This function starts at the location and moves in a direction as defined
    // by
    // the center to the location and returns the gridCoordinate of the edge of
    // the specified area.
    // -- implementation
    // ---------------------------------------------------------
    // Get the absolute value of the largest component of the vector from the
    // center to location. Then calculate an incremental delta. Start at
    // location and move away from the center until we're outside of area.
    // Return the coordinate.
    // ---------------------------------------------------------------------------
    Point getEdge(final Point center, final Point location, final Grid2DBit area) {
        int maxDelta = Math.max(Math.abs(location.x - center.x),
                Math.abs(location.y - center.y));

        // If we're already at the center return (0, 0)
        if (maxDelta == 0) {
            return new Point(0, 0);
        }

        Point delta = new Point();
        delta.x = (location.x - center.x) / maxDelta;
        delta.y = (location.y - center.y) / maxDelta;

        Point edge = new Point();
        boolean moving = true;
        Point pos = new Point(location.x, location.y);
        while (moving) {
            pos.x = pos.x + delta.x;
            pos.y = pos.y + delta.y;
            // If we've left the area, we're all done
            int x = (int) (pos.x + 0.5); // round off
            int y = (int) (pos.y + 0.5); // round off
            if ((x >= area.getXdim()) || (y >= area.getYdim())
                    || (area.get(x, y) != 1)) {
                // We're either off the grid or out of the area
                edge.x = x;
                edge.y = y;
                moving = false;
            }
        }
        return edge;
    }

    /**
     * Adds the set of changed points to the already changed points. Returns
     * true if ok (valid changedPoints).
     * 
     * @param changedPoints
     */
    protected boolean setChangedPoints(final Grid2DBit aChangedPoints) {
        // Performs a logical "OR" to the two Grid2DBits.
        if (aChangedPoints.isValid()) {
            this.changedPoints.orEquals(aChangedPoints);
            return true;
        }
        return false;
    }

    protected boolean setChangedPoints(Point loc) {
        if (this.changedPoints.isValid(loc.x, loc.y)) {
            this.changedPoints.set(loc.x, loc.y);
            return true;
        }

        return false;
    }

    @Override
    public boolean smooth(final Date time, Grid2DBit pointsToSmooth) {
        populate();

        Grid2DBit points = pointsToSmooth;
        if (iscMode()) {
            Grid2DBit iP = iscPoints(time, true);
            points = points.and(iP); // limit valid data areas
        }

        return setChangedPoints(doSmooth(time, points));
    }

    /**
     * "Smooth" the grid values at the points in the input selected group of
     * points, by averaging with neighbor points' values.
     * 
     * @param time
     * @param pointsToSmooth
     * @return
     */
    protected abstract Grid2DBit doSmooth(final Date time,
            final Grid2DBit pointsToSmooth);

    // -- protected
    // --------------------------------------------------------------
    // GridData::fringe()
    // Returns a Grid2DBit that identifies the out edge or fringe of the
    // specified
    // Grid2DBit.
    // -- implementation
    // ---------------------------------------------------------
    // For each set cell in points, look in each direction to see if the any
    // points are no set. If not, set the cell in edge.
    // ---------------------------------------------------------------------------
    protected Grid2DBit fringe(final Grid2DBit points) {
        // Make a new Grid2DBit and clear it
        int xMax = points.getXdim() - 1;
        int yMax = points.getYdim() - 1;

        Grid2DBit edge = points.clone();
        edge.clear();

        Point ll = new Point();
        Point ur = new Point();
        if (!points.extremaOfSetBits(ll, ur)) {
            return edge;
        }

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (points.get(i, j) > 0) {
                    if ((i == 0) || (i == xMax) || (j == 0) || (j == yMax)) {
                        edge.set(i, j);
                    } else {
                        for (int k = i - 1; k <= i + 1; k++) {
                            for (int m = j - 1; m <= j + 1; m++) {
                                if (points.get(k, m) == 0) {
                                    edge.set(i, j);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        return edge;
    }

    @Override
    public void changeValidTime(TimeRange timeRange, boolean considerRate) {
        if (!timeRange.equals(this.gridSlice.getValidTime())) {
            populate();

            // rate-dependent parm?
            if (getParm().getGridInfo().isRateParm() && considerRate
                    && this.gridSlice.getValidTime().isValid()) {
                float factor = timeRange.getDuration()
                        / (float) this.gridSlice.getValidTime().getDuration();
                ((IContinuousSlice) this.gridSlice).operateEquals(Op.MULTIPLY,
                        factor);
            }
            this.gridSlice.setValidTime(timeRange);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof AbstractGridData)) {
            return false;
        }

        AbstractGridData rhs = (AbstractGridData) o;

        if (!this.parm.equals(rhs.parm)) {
            return false;
        }

        if (!this.gridSlice.equals(rhs.gridSlice)) {
            return false;
        }

        return true;
    }

    /**
     * Converts the specified float (lat/lon) coordinates into grid coordinates.
     * 
     * Use the gridLocation function to convert each coord.
     * 
     * @param floatCoords
     */
    protected Point[] convertToGridCoords(Coordinate[] floatCoords) {
        GridLocation gridLoc = this.getParm().getGridInfo().getGridLoc();

        Point gridSize = gridLoc.gridSize();

        ArrayList<Point> gridCoords = new ArrayList<Point>();
        for (Coordinate coord : floatCoords) {
            Coordinate thisCoord = MapUtil.latLonToGridCoordinate(coord,
                    PixelOrientation.CENTER, gridLoc);

            Point p = new Point((int) thisCoord.x, (int) thisCoord.y);

            // if point is in the grid
            if ((p.x >= 0) && (p.x < gridSize.x) && (p.y >= 0)
                    && (p.y < gridSize.y)) {
                gridCoords.add(p);
            }
        }

        return gridCoords.toArray(new Point[gridCoords.size()]);
    }

    // -- public
    // -----------------------------------------------------------------
    // GridData::moveCopyArea()
    // Copies the identified points and shifts them by delta. Return true for
    // success.
    // -- implementation
    // ---------------------------------------------------------
    // Checks ok to edit. If okay, then calls virtual doMoveCopy.
    // ---------------------------------------------------------------------------
    @Override
    public boolean moveCopyArea(final Date time,
            final Grid2DBit pointsToMoveCopy, final Point delta, boolean copyOp) {
        populate();
        if (!this.isOkToEdit()) {
            return false;
        }

        // ISC mode requires truncating the pointsToMoveCopy to just those
        // valid data points
        Grid2DBit points = pointsToMoveCopy;
        if (iscMode()) {
            Grid2DBit iP = iscPoints(time, true);
            points = iP.and(points); // limit to valid data areas
        }

        // Make the change
        Grid2DBit changes = doCopy(time, points, delta);

        if (!copyOp) // Then it must be a move operation
        {
            // Subtract off the intersection of points to copy it's translation
            Grid2DBit fillGrid = points
                    .xor(points.and(points.translate(delta)));
            if (this.getParm().getGridInfo().getGridType() == GridType.VECTOR) {

                VectorMode vm = this.parm.getParmState().getVectorMode();
                this.parm.getParmState().setVectorMode(VectorMode.BOTH);
                doFillIn(time, fillGrid);
                this.parm.getParmState().setVectorMode(vm);
            } else {
                doFillIn(time, fillGrid);
            }
            changes = changes.or(fillGrid);
        }
        // track the changes
        return setChangedPoints(changes);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#getContiguousArea(java.util
     * .Date, java.awt.Point)
     */
    @Override
    public Grid2DBit getContiguousArea(Date time, Point location) {
        populate();
        return doContiguous(time, location);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#pencilStretch(java.util.
     * Date, com.raytheon.viz.gfe.core.wxvalue.WxValue,
     * com.vividsolutions.jts.geom.Coordinate[])
     */
    @Override
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate path[]) {
        return pencilStretch(time, value, path, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#pencilStretch(java.util.
     * Date, com.raytheon.viz.gfe.core.wxvalue.WxValue,
     * com.vividsolutions.jts.geom.Coordinate[], boolean)
     */
    @Override
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate[] path,
            boolean limitToEditArea) {
        populate();
        if (!this.isOkToEdit()) {
            return null;
        }

        // Get the active edit area, or the complete area
        Grid2DBit editArea = limitToEditArea ? this.parm.getDataManager()
                .getRefManager().getActiveRefSet().getGrid() : this.parm
                .getDataManager().getRefManager().fullRefSet().getGrid();

        // Make the change
        Grid2DBit newChangedPoints = doPencilStretch(time, value, path,
                editArea);
        if (newChangedPoints.isValid()) {
            setChangedPoints(newChangedPoints);
        }

        return newChangedPoints;
    }

    protected abstract Grid2DBit doContiguous(Date time, Point location);

    protected abstract Grid2DBit doPencilStretch(Date time, WxValue value,
            Coordinate[] path, Grid2DBit editArea);

    protected abstract Grid2DBit doCopy(final Date time,
            final Grid2DBit pointsToCopy, final Point delta);

    protected abstract Grid2DBit doFillIn(final Date time,
            final Grid2DBit pointsToFillIn);

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(IGridData o) {
        return this.getGridTime().compareTo(o.getGridTime());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#depopulate()
     */
    @Override
    public void depopulate() {
        synchronized (this) {
            if (!this.isPopulated()) {
                return;
            }
            // String msg = "Depopulating " + getParm().getParmID() + " tr="
            // + getGridTime();
            // statusHandler.handle(Priority.DEBUG, msg, new Exception("Debug: "
            // + msg));

            this.lastAccessTime = 0;
            setGridSliceDataToNull();
        }
    }

    protected abstract void setGridSliceDataToNull();

    @Override
    public List<String> getHistorySites() {
        GridDataHistory[] h = this.getHistory();
        List<String> sites = new ArrayList<String>();
        for (GridDataHistory element : h) {
            String site = element.getOriginParm().getDbId().getSiteId();
            if (!sites.contains(site)) {
                sites.add(site);
            }
        }
        return sites;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.parm.getParmID().toString() + " "
                + this.getGridTime().toString();
    }

    @Override
    public Grid2DBit getISCGrid(Date t, ScalarGridSlice slice) {
        if (iscMode()) {
            IISCDataAccess iscDA = this.getParm().getDataManager()
                    .getIscDataAccess();
            return iscDA
                    .getCompositeGrid(new GridID(getParm(), t), true, slice);
        } else {
            return new Grid2DBit(1, 1);
        }
    }

    @Override
    public Grid2DBit getISCGrid(Date t, VectorGridSlice slice) {
        if (iscMode()) {
            IISCDataAccess iscDA = getParm().getDataManager()
                    .getIscDataAccess();
            return iscDA
                    .getCompositeGrid(new GridID(getParm(), t), true, slice);
        } else {
            return new Grid2DBit(1, 1);
        }
    }

    @Override
    public Grid2DBit getISCGrid(Date t, DiscreteGridSlice slice) {
        if (iscMode()) {
            IISCDataAccess iscDA = getParm().getDataManager()
                    .getIscDataAccess();
            return iscDA
                    .getCompositeGrid(new GridID(getParm(), t), true, slice);
        } else {
            return new Grid2DBit(1, 1);
        }
    }

    @Override
    public Grid2DBit getISCGrid(Date t, WeatherGridSlice slice) {
        if (iscMode()) {
            IISCDataAccess iscDA = getParm().getDataManager()
                    .getIscDataAccess();
            return iscDA
                    .getCompositeGrid(new GridID(getParm(), t), true, slice);
        } else {
            return new Grid2DBit(1, 1);
        }
    }

    @Override
    public boolean iscMode() {
        if (iscCapable) {
            return DataManager.getCurrentInstance().getParmManager().iscMode();
        } else {
            return false;
        }
    }

    @Override
    public Grid2DBit iscPoints(Date t, boolean includeOwnSite) {

        int nx = this.getParm().getGridInfo().getGridLoc().getNx();
        int ny = this.getParm().getGridInfo().getGridLoc().getNy();

        Grid2DBit points = new Grid2DBit(nx, ny);

        // get my site points
        if (includeOwnSite) {
            points = getParm().getDataManager().getRefManager()
                    .mySiteGridpoints();
        }

        // isc mode?
        if (iscMode()) {
            IISCDataAccess iscDA = getParm().getDataManager()
                    .getIscDataAccess();
            GridID iscGID = iscDA.getISCGridID(new GridID(getParm(), t), true);
            IGridData iscGrid = null;
            if (iscGID != null) {
                iscGrid = iscGID.grid();
            }
            if (iscGrid != null) {
                Grid2DBit iscSitePoints = getParm().getDataManager()
                        .getRefManager()
                        .siteGridpoints(iscGrid.getHistorySites(), false);
                points = points.or(iscSitePoints);
            }
        }
        return points;
    }

    @Override
    public Grid2DBit mySitePoints() {
        return getParm().getDataManager().getRefManager().mySiteGridpoints();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#lockGrid()
     */
    @Override
    public boolean lockGrid() {
        return parm.forceLockTR(getGridTime());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#replace(com.raytheon.viz
     * .gfe.core.griddata.IGridData)
     */
    @Override
    public boolean replace(IGridData source) {
        if (source == null) {
            return false;
        }

        if (!source.getParm().getGridInfo()
                .equals(this.getParm().getGridInfo())) {
            statusHandler.handle(Priority.PROBLEM,
                    "Differing gridInfos for source/dest for replace()."
                            + "\nSource: " + source.getParm().getGridInfo()
                            + "\nDest: " + this.getParm().getGridInfo());
            return false;
        }

        if (!source.getGridTime().equals(getGridTime())) {
            statusHandler.handle(Priority.PROBLEM,
                    "Differing gridTimes for source/dest for replace()"
                            + "\nSource: " + source.getGridTime() + "\nDest: "
                            + this.getGridTime());
            return false;
        }

        try {
            // make identical copy
            gridSlice = ((AbstractGridData) source).gridSlice.clone();

            // send out notifications that items have changed.
            this.parm.gridHistoryChanged(this);
        } catch (CloneNotSupportedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#getGridTime()
     */
    @Override
    public TimeRange getGridTime() {
        return this.gridSlice.getValidTime();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#getHistory()
     */
    @Override
    public GridDataHistory[] getHistory() {
        return this.gridSlice.getHistory();
    }

    protected void substitudeDS(IGridSlice ds) {
        TimeRange origTR = getGridTime(); // save time range of this grid
        GridDataHistory[] gdh = gridSlice.getHistory();
        GridParmInfo gpi = gridSlice.getGridInfo();
        try {
            gridSlice = ds.clone();// make identical copy
            gridSlice.setHistory(gdh);
            gridSlice.setGridInfo(gpi);
            gridSlice.setValidTime(origTR);
        } catch (CloneNotSupportedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }
    }
}
