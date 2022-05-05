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
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.Validate;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.gfe.core.IISCDataAccess;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState.VectorMode;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import org.locationtech.jts.geom.Coordinate;

/**
 * GridData is the abstract base class of the GridData hierarchy. It contains
 * the metadata and a lazily populated dataObject and defines an interface for
 * editing and accessing data.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 29, 2008           chammack  Initial Class Skeleton.
 * Mar 13, 2008  879      rbell     Legacy conversion.
 * Jun 10, 2009  2159     rjpeter   Updated isValid to call gridSlice.isValid
 * Feb 19, 2013  1637     randerso  Added throws declarations to
 *                                  translateDataFrom
 * Apr 15, 2013  1892     randerso  Adding logging to help determine what is
 *                                  different in the gridInfos, changed how
 *                                  gridInfo is retrieved which seems to have
 *                                  fixed the problem
 * Apr 23, 2013  1949     rjpeter   Removed validation on copy, source is
 *                                  verified on store.
 * Aug 02, 2016  5744     mapeters  Cleanup
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Changes to support IDataObject. Code cleanup
 * Jan 02, 2019  7705     randerso  Fix deadlock issue. Remove unnecessary
 *                                  populate calls.
 * Aug 19, 2019  7906     randerso  Remove soft/hard reference logging
 *
 * </pre>
 *
 * @author chammack
 */
public abstract class AbstractGridData implements IGridData {
    /*
     * RODO DR #7178 design concept
     *
     * IGridSlice is a construct carried over from A1 that is used on both
     * client and server side. It contains both metaData and optionally data
     * (i.e. the actual grids/keys).
     *
     * IGridData was originally a wrapper around the IGridSlice on the client
     * side that kept track of changed points and last access.
     *
     * As part of this change I separated the metaData and data (metaData moved
     * into IGridData, data into IDataObject) so we can use soft references for
     * the data part when it's unmodified so the GC can throw it out if we need
     * the memory rather than evicting the data solely based on time.
     *
     * The old parm evictor code would replace the IGridSlice in IGridData with
     * one with the data part unpopulated. I couldn't use soft references to the
     * entire IGridSlice or we'd lose the metadata. Since IGridSlice is a server
     * side object I couldn't use the SoftReference for the data inside it.
     *
     * If I were designing GFE from scratch today I would have reworked the
     * server interface to separate the data and the metadata better.
     */

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGridData.class);

    protected Parm parm;

    protected TimeRange validTime;

    protected GridParmInfo gridParmInfo;

    protected List<GridDataHistory> gridDataHistory;

    protected long lastAccessTime;

    protected Grid2DBit changedPoints;

    /*
     * These dataRef members should only be accessed by the setDataObject,
     * getDataObject, and isPopulated methods
     *
     */

    /**
     * The hardDataRef is only set when the IDataObject is modified/unsaved and
     * must not be discarded. The hard reference will prevent the Garbage
     * Collector from freeing the IDataObject.
     */
    private IDataObject hardDataRef;

    /**
     * The softDataRef stores a reference to the IDataObject allowing the
     * Garbage Collector to free the IDataObject if needed to avoid an
     * OutOfMemory condition. If the IDataObject is freed it will be reloaded
     * from the server side database when the data is needed.
     */
    private SoftReference<IDataObject> softDataRef;

    /**
     * Constructor
     *
     * @param parm
     * @param aSlice
     * @param unsaved
     *            true if data is unsaved and must not be depopulated
     */
    protected AbstractGridData(Parm parm, IGridSlice aSlice, boolean unsaved) {
        this.lastAccessTime = System.currentTimeMillis();
        this.parm = parm;
        this.validTime = aSlice.getValidTime();
        this.gridParmInfo = aSlice.getGridInfo();
        this.gridDataHistory = aSlice.getGridDataHistory();
        this.changedPoints = new Grid2DBit();

        // populate dataObject from slice
        IDataObject dataObject = null;
        switch (this.parm.getGridInfo().getGridType()) {
        case SCALAR: {
            ScalarGridSlice slice = (ScalarGridSlice) aSlice;
            Grid2DFloat grid = slice.getScalarGrid();
            if (grid != null) {
                dataObject = new ScalarDataObject(grid);
            }
            break;
        }

        case VECTOR: {
            VectorGridSlice slice = (VectorGridSlice) aSlice;
            Grid2DFloat magGrid = slice.getMagGrid();
            Grid2DFloat dirGrid = slice.getDirGrid();
            if ((magGrid != null) && (dirGrid != null)) {
                dataObject = new VectorDataObject(magGrid, dirGrid);
            }
            break;
        }

        case WEATHER: {
            WeatherGridSlice slice = (WeatherGridSlice) aSlice;
            Grid2DByte grid = slice.getWeatherGrid();
            WeatherKey[] keys = slice.getKeys();
            if ((grid != null) && (keys != null)) {
                dataObject = new WeatherDataObject(grid, keys);
            }
            break;
        }

        case DISCRETE: {
            DiscreteGridSlice slice = (DiscreteGridSlice) aSlice;
            Grid2DByte grid = slice.getDiscreteGrid();
            DiscreteKey[] keys = slice.getKeys();
            if ((grid != null) && (keys != null)) {
                dataObject = new DiscreteDataObject(grid, keys);
            }
            break;
        }

        default:
            throw new IllegalArgumentException("Unknown GridType received: "
                    + this.gridParmInfo.getGridType());
        }

        softDataRef = new SoftReference<>(dataObject);
        if (dataObject != null) {
            if (unsaved) {
                hardDataRef = dataObject;
            }
        }
    }

    /**
     * Constructor, not for general use
     *
     * @param parm
     * @param dataObject
     */
    protected AbstractGridData(Parm parm, IDataObject dataObject) {
        this.parm = parm;
        this.gridParmInfo = parm.getGridInfo();
        setDataObject(dataObject, "Creating");
    }

    /**
     * Copy constructor
     *
     * @param other
     */
    protected AbstractGridData(AbstractGridData other) {
        this.lastAccessTime = System.currentTimeMillis();
        this.parm = other.parm;
        this.validTime = other.getGridTime().clone();
        this.gridParmInfo = other.getGridInfo().copy();
        List<GridDataHistory> newHistory = new ArrayList<>(
                other.getGridDataHistory().size());
        for (GridDataHistory history : other.getGridDataHistory()) {
            newHistory.add(history.copy());
        }
        this.gridDataHistory = newHistory;
        this.changedPoints = new Grid2DBit();

        IDataObject newDataObject;
        boolean hard = false;
        newDataObject = other.getDataObjectIfPopulated();
        hard = other.hardDataRef != null;

        if (newDataObject != null) {
            newDataObject = newDataObject.copy();
        }

        this.softDataRef = new SoftReference<>(newDataObject);
        if (hard) {
            this.hardDataRef = newDataObject;
        }
    }

    @Override
    public boolean changeParmAssociation(Parm newParm) {
        if (newParm != null) {
            parm = newParm;
            gridParmInfo.resetParmID(parm.getGridInfo().getParmID());
        }
        return false;
    }

    /**
     * Make grid data with the corresponding gridSlice and parm
     *
     * @param parm
     * @param slice
     * @param unsaved
     *            true if data is unsaved and must not be depopulated
     * @return the grid data
     */
    public static IGridData makeGridData(Parm parm, IGridSlice slice,
            boolean unsaved) {
        if (slice == null) {
            throw new IllegalArgumentException("slice must not be null");
        }

        switch (parm.getGridInfo().getGridType()) {
        case SCALAR:
            return new ScalarGridData(parm, slice, unsaved);
        case VECTOR:
            return new VectorGridData(parm, slice, unsaved);
        case WEATHER:
            return new WeatherGridData(parm, slice, unsaved);
        case DISCRETE:
            return new DiscreteGridData(parm, slice, unsaved);
        default:
            throw new IllegalArgumentException("Unsupported GridType: "
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
    @Deprecated
    public IGridSlice getGridSlice() {
        return createSlice();
    }

    protected abstract IGridSlice createSlice();

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
        return doValid();
    }

    protected abstract boolean doValid();

    @Override
    public String validateData() {
        String retVal = null;
        IDataObject dataObject;

        if (this.gridParmInfo.getGridType() == GridType.NONE) {
            retVal = "AbstractGridSlice type is NONE";

        } else if (gridDataHistory.isEmpty()) {
            retVal = "Grid history length 0";

        } else if (!validTime.isValid()) {
            retVal = "AbstractGridSlice time range is not valid";

        } else if ((dataObject = getDataObjectIfPopulated()) == null) {
            retVal = "Grid data not populated";
        } else {
            retVal = doValidateData(dataObject);
        }

        return retVal;
    }

    protected abstract String doValidateData(IDataObject dataObject);

    /**
     * @return the populated data grid
     */
    @Override
    public IDataObject getDataObject() {
        populate();
        return softDataRef.get();
    }

    /**
     * Get a reference to the DataObject if it is populated
     *
     * Used internally by this class. The standard getDataObject() method will
     * force the DataObject to be populated.
     *
     * @return the DataObject or null if not populated
     */
    private IDataObject getDataObjectIfPopulated() {
        return softDataRef.get();
    }

    @Override
    public void setDataObject(IDataObject dataObject) {
        setDataObject(dataObject, "Setting");
    }

    private void setDataObject(IDataObject dataObject, String action) {
        Validate.notNull(dataObject, "dataObject must not be null");

        // if already a hard reference, keep it hard
        if (this.hardDataRef != null) {
            this.hardDataRef = dataObject;
        }

        // set the soft reference
        this.softDataRef = new SoftReference<>(dataObject);
    }

    @Override
    public boolean isPopulated() {
        return getDataObjectIfPopulated() != null;
    }

    private void populate() {
        if (!this.isPopulated()) {
            try {
                statusHandler.debug("Populating " + this.toString());
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
        Point size = new Point(
                this.gridParmInfo.getGridLoc().getNx().intValue(),
                this.gridParmInfo.getGridLoc().getNy().intValue());
        this.changedPoints = new Grid2DBit(size.x, size.y);

        // switch to hard reference
        if (this.hardDataRef == null) {
            this.hardDataRef = getDataObject();
        }
    }

    @Override
    public void successfullySaved() {
        // switch back to soft reference
        if (this.hardDataRef != null) {
            this.hardDataRef = null;
        }
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
        for (GridDataHistory history : this.getHistory()) {
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
        if (!getParm().getGridInfo().getUnitObject().isCompatible(
                sourceGrid.getParm().getGridInfo().getUnitObject())) {
            statusHandler.handle(Priority.PROBLEM,
                    "Attempt to copyGridValues of different units: "
                            + getParm().getGridInfo().getUnitObject() + " vs. "
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

    protected abstract boolean translateDataFrom(final IGridData source)
            throws FactoryException, TransformException;

    @Override
    public boolean setValue(WxValue aValue, Grid2DBit editArea) {
        checkOkayForEdit();

        // Make the change
        return setChangedPoints(doSet(aValue, editArea));

    }

    protected abstract Grid2DBit doDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange);

    protected abstract Grid2DBit doSet(WxValue aValue, Grid2DBit editArea);

    @Override
    public boolean updateHistory(GridDataHistory history) {
        return updateHistory(new GridDataHistory[] { history });
    }

    @Override
    public boolean updateHistory(GridDataHistory[] history) {
        if (history.length == 0) {
            return false;
        }
        if (this.getHistory() != history) {
            List<GridDataHistory> thisGDHA = new ArrayList<>();

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
            this.gridDataHistory = thisGDHA;
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

        if ((edgeCount / editAreaCount) > 0.9) {
            return;
        } else if ((edgeCount / editAreaCount) < 0.5) {
            edge = edge.or(fringe(editArea.xor(edge)));
        }

        smooth(time, edge);

        return;
    }

    @Override
    public boolean applyDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange) {
        if (delta == 0.0) {
            // nothing to change
            return true;
        }
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
                float totalDist = (float) Math
                        .sqrt((delta.x * delta.x) + (delta.y * delta.y));

                // Get the distance from the edge to current location
                delta.x = i - edge.x;
                delta.y = j - edge.y;
                float dist = (float) Math
                        .sqrt((delta.x * delta.x) + (delta.y * delta.y));

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
    protected Point getEdge(final Point center, final Point location,
            final Grid2DBit area) {
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
            int x = (int) (pos.x + 0.5);
            int y = (int) (pos.y + 0.5);
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
     * @return true if successful
     */
    protected boolean setChangedPoints(final Grid2DBit changedPoints) {
        // Performs a logical "OR" to the two Grid2DBits.
        if (changedPoints.isValid()) {
            this.changedPoints.orEquals(changedPoints);
            return true;
        }
        return false;
    }

    /**
     * Adds a changed point
     *
     * @param loc
     * @return true if successful
     */
    protected boolean setChangedPoints(Point loc) {
        if (this.changedPoints.isValid(loc.x, loc.y)) {
            this.changedPoints.set(loc.x, loc.y);
            return true;
        }

        return false;
    }

    @Override
    public boolean smooth(final Date time, Grid2DBit pointsToSmooth) {
        Grid2DBit points = pointsToSmooth;
        if (iscMode()) {
            Grid2DBit iP = iscPoints(time, true);

            // limit valid data areas
            points = points.and(iP);
        }

        return setChangedPoints(doSmooth(time, points));
    }

    /**
     * "Smooth" the grid values at the points in the input selected group of
     * points, by averaging with neighbor points' values.
     *
     * @param time
     * @param pointsToSmooth
     * @return mask of the changed points
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

        Grid2DBit edge = points.copy();
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
                        for (int k = i - 1; k <= (i + 1); k++) {
                            for (int m = j - 1; m <= (j + 1); m++) {
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
        if (!timeRange.equals(this.validTime)) {
            // preserve original validTime for rate dependent parm adjustment
            TimeRange origTime = this.validTime;

            // set hard reference
            this.hardDataRef = getDataObject();
            this.validTime = timeRange;

            // rate-dependent parm?
            if (getParm().getGridInfo().isRateParm() && considerRate
                    && origTime.isValid()) {
                float factor = timeRange.getDuration()
                        / (float) origTime.getDuration();
                ((IContinuousDataObject) getDataObject())
                        .operateEquals(Op.MULTIPLY, factor);
            }
        }
    }

    /**
     * Converts the specified float (lat/lon) coordinates into grid coordinates.
     *
     * Use the gridLocation function to convert each coord.
     *
     * @param floatCoords
     * @return the grid coordinates
     */
    protected Point[] convertToGridCoords(Coordinate[] floatCoords) {
        GridLocation gridLoc = this.getParm().getGridInfo().getGridLoc();

        Point gridSize = gridLoc.gridSize();

        ArrayList<Point> gridCoords = new ArrayList<>();
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((gridDataHistory == null) ? 0 : gridDataHistory.hashCode());
        result = (prime * result)
                + ((gridParmInfo == null) ? 0 : gridParmInfo.hashCode());
        IDataObject dataObject = getDataObjectIfPopulated();
        result = (prime * result)
                + ((dataObject == null) ? 0 : dataObject.hashCode());
        result = (prime * result) + ((parm == null) ? 0 : parm.hashCode());
        result = (prime * result)
                + ((validTime == null) ? 0 : validTime.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AbstractGridData other = (AbstractGridData) obj;
        if (gridDataHistory == null) {
            if (other.gridDataHistory != null) {
                return false;
            }
        } else if (!gridDataHistory.equals(other.gridDataHistory)) {
            return false;
        }
        if (gridParmInfo == null) {
            if (other.gridParmInfo != null) {
                return false;
            }
        } else if (!gridParmInfo.equals(other.gridParmInfo)) {
            return false;
        }
        IDataObject dataObject = getDataObjectIfPopulated();
        IDataObject otherDataObject = other.getDataObjectIfPopulated();
        if (dataObject == null) {
            if (otherDataObject != null) {
                return false;
            }
        } else if (!dataObject.equals(otherDataObject)) {
            return false;
        }
        if (parm == null) {
            if (other.parm != null) {
                return false;
            }
        } else if (!parm.equals(other.parm)) {
            return false;
        }
        if (validTime == null) {
            if (other.validTime != null) {
                return false;
            }
        } else if (!validTime.equals(other.validTime)) {
            return false;
        }
        return true;
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
            final Grid2DBit pointsToMoveCopy, final Point delta,
            boolean copyOp) {
        if (!this.isOkToEdit()) {
            return false;
        }

        // ISC mode requires truncating the pointsToMoveCopy to just those
        // valid data points
        Grid2DBit points = pointsToMoveCopy;
        if (iscMode()) {
            Grid2DBit iP = iscPoints(time, true);

            // limit to valid data areas
            points = iP.and(points);
        }

        // Make the change
        Grid2DBit changes = doCopy(time, points, delta);

        // if not copy then it must be a move operation
        if (!copyOp) {
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

    @Override
    public Grid2DBit getContiguousArea(Date time, Point location) {
        return doContiguous(time, location);
    }

    @Override
    public Grid2DBit pencilStretch(Date time, WxValue value,
            Coordinate path[]) {
        return pencilStretch(time, value, path, true);
    }

    @Override
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate[] path,
            boolean limitToEditArea) {
        if (!this.isOkToEdit()) {
            return null;
        }

        // Get the active edit area, or the complete area
        Grid2DBit editArea = limitToEditArea
                ? this.parm.getDataManager().getRefManager().getActiveRefSet()
                        .getGrid()
                : this.parm.getDataManager().getRefManager().fullRefSet()
                        .getGrid();

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

    @Override
    public int compareTo(IGridData o) {
        return this.getGridTime().compareTo(o.getGridTime());
    }

    @Override
    public List<String> getHistorySites() {
        GridDataHistory[] h = this.getHistory();
        List<String> sites = new ArrayList<>();
        for (GridDataHistory element : h) {
            String site = element.getOriginParm().getDbId().getSiteId();
            if (!sites.contains(site)) {
                sites.add(site);
            }
        }
        return sites;
    }

    @Override
    public String toString() {
        String s = this.parm.getParmID().toString();
        TimeRange tr = this.getGridTime();
        if (tr != null) {
            s += " " + tr.toString();
        }
        return s;
    }

    @Override
    public Pair<Grid2DBit, IGridData> getISCGrid(Date t) {
        if (iscMode()) {
            IISCDataAccess iscDA = this.getParm().getDataManager()
                    .getIscDataAccess();
            return iscDA.getCompositeGrid(new GridID(getParm(), t), true);
        } else {
            return new Pair<>(new Grid2DBit(), null);
        }
    }

    @Override
    public boolean iscMode() {
        return getParm().getDataManager().getParmManager().iscMode();
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

    @Override
    public boolean lockGrid() {
        return parm.forceLockTR(getGridTime());
    }

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

        // make identical copy
        TimeRange newValidTime = source.getGridTime().clone();
        GridParmInfo newGridInfo = source.getGridInfo().copy();
        List<GridDataHistory> newHistory = new ArrayList<>(
                source.getGridDataHistory().size());
        for (GridDataHistory history : source.getGridDataHistory()) {
            newHistory.add(history.copy());
        }
        IDataObject newDataObject = ((AbstractGridData) source)
                .getDataObjectIfPopulated();
        if (newDataObject != null) {
            newDataObject.copy();
        }

        this.validTime = newValidTime;
        this.gridParmInfo = newGridInfo;
        this.gridDataHistory = newHistory;
        setDataObject(newDataObject);

        // send out notifications that items have changed.
        this.parm.gridHistoryChanged(this);
        return true;
    }

    @Override
    public TimeRange getGridTime() {
        return this.validTime;
    }

    @Override
    public GridParmInfo getGridInfo() {
        return this.gridParmInfo;
    }

    @Override
    public List<GridDataHistory> getGridDataHistory() {
        return Collections.unmodifiableList(this.gridDataHistory);
    }

    @Override
    public GridDataHistory[] getHistory() {
        return this.gridDataHistory
                .toArray(new GridDataHistory[this.gridDataHistory.size()]);
    }

    protected void substituteDataObject(IGridData source) {
        // replace our dataObject with a copy of the source dataObject
        IDataObject newDataObject = ((AbstractGridData) source)
                .getDataObjectIfPopulated();
        if (newDataObject != null) {
            newDataObject = newDataObject.copy();
        }
        setDataObject(newDataObject);
    }
}
