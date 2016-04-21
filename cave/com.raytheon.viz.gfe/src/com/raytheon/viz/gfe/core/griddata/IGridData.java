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
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GridData is the ABC of the GridData hierarchy. It encapsulates the concept of
 * a data gridSlice and defines an interface for editing and accessing data.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial Class Skeleton.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IGridData extends Comparable<IGridData> {
    public enum EditOp {
        DELTA, SET, MOVE_COPY, SMOOTH, FILLINHOLE, CONTOUR, PENCILSTRETCH
    };

    /**
     * Applies the given delta value to the identified points. If taper is true,
     * then a taper function is applied.
     * 
     * @param time
     *            the time to apply the change
     * @param delta
     *            the change
     * @param taper
     *            the taper flag
     * @param pointsToChange
     *            the points to apply to
     * @return
     */
    public boolean applyDelta(final Date time, float delta, boolean taper,
            Grid2DBit pointsToChange);

    /**
     * Changes the parm association for this GridData. This should only be
     * called by the Parm class.
     * 
     * @param newParm
     *            The new Parm this GridData will be associated with.
     * @return True, if the swap was successful, false if not.
     */
    public boolean changeParmAssociation(final Parm newParm);

    /**
     * Modifies the timeRange found in _dataSlice. The user is responsible for
     * ensuring that this grid does not overlap any other grids in parm. Note
     * that gridDataChanged notification is NOT sent to the parm. The parm is
     * not notified of the valid time be changed which will cause visualizations
     * of the inventory to be incorrect. This routine is intended to be used
     * only by the data subsystem. Note that if this is a rate-dependent parm,
     * then the data values may be adjusted based on the new duration vs. the
     * old duration. The "considerRate" indicates to consider the duration
     * change in the data.
     * 
     * @param timeRange
     * @param considerRate
     */
    public void changeValidTime(final TimeRange timeRange, boolean considerRate);

    /**
     * Return a copy of the grid data object
     * 
     * @return
     * @throws CloneNotSupportedException
     */
    public IGridData clone() throws CloneNotSupportedException;

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
     * @return true if successful
     */
    public boolean copyGridValues(final IGridData sourceGrid);

    /**
     * Depopulates the grid. Effectively removes the data associated with the
     * grid
     */
    public void depopulate();

    /**
     * Command from parm to indicate that a grid edit has ended. The points that
     * have changed are returned as a Grid2DBit.
     * 
     * @return the points that were changed
     */
    public Grid2DBit endGridEdit();

    /**
     * Returns a Grid2Bit whose bits match the grid points that have the same
     * value and are all touching.
     * 
     * @param time
     * @param location
     * @return
     */
    public Grid2DBit getContiguousArea(final Date time, final Point location);

    /**
     * Return the underlying grid gridSlice. This must not be null.
     * 
     * @return
     */
    public IGridSlice getGridSlice();

    /**
     * Return the time the grid was last accessed
     * 
     * @return time the grid was last accessed
     */
    public long getLastAccessTime();

    /**
     * Returns the time range associated with this grid.
     * 
     * @return
     */
    public TimeRange getGridTime();

    /**
     * Returns the history of this data slice.
     * 
     * @return
     */
    public GridDataHistory[] getHistory();

    /**
     * Return the parm that this grid data is associated with
     * 
     * @return
     */
    public Parm getParm();

    /**
     * Return the value at a point
     * 
     * @param x
     *            the x coordinate
     * @param y
     *            the y coordinate
     * @return the value
     */
    public WxValue getWxValue(int x, int y);

    /**
     * Returns max of this grid and supplied grid.
     * 
     * @param gridSlice
     * @return
     */
    public IGridSlice gridMax(IGridSlice gridSlice);

    /**
     * Returns min of this grid and supplied grid.
     * 
     * @param gridSlice
     * @return
     */
    public IGridSlice gridMin(IGridSlice gridSlice);

    /**
     * Returns new grid that is multiplied by factor.
     * 
     * @param factor
     * @return
     */
    public IGridSlice gridMultiply(float factor);

    /**
     * Returns sum of supplied grid and this grid.
     * 
     * @param gridSlice
     * @return
     */
    public IGridSlice gridSum(IGridSlice gridSlice);

    /**
     * Return true if the grid is safe for editing
     * 
     * @return
     */
    public boolean isOkToEdit();

    public boolean isSupportedEditOp(EditOp editOp);

    /**
     * Returns true if the grid is valid
     * 
     * @return true if the grid is valid
     * 
     */
    public boolean isPopulated();

    /**
     * Return true if the grid has been modified by a user
     * 
     * @return
     */
    public boolean isUserModified();

    /**
     * Return true if the grid is valid
     * 
     * @return
     */
    public boolean isValid();

    /**
     * Copies the identified points and shifts them by delta. Return true for
     * success.
     * 
     * @param time
     * @param pointsToMoveCopy
     * @param delta
     * @param copyOp
     * @return
     */
    public boolean moveCopyArea(final Date time,
            final Grid2DBit pointsToMoveCopy, final Point delta, boolean copyOp);

    /**
     * Smooth the grid
     * 
     * @param time
     * @param pointsToSmooth
     * @return true if successful
     */
    public boolean smooth(Date time, Grid2DBit pointsToSmooth);

    /**
     * Calculate portions of the grid based on a pencil stretch operation.
     * 
     * @param time
     * @param value
     * @param path
     * @return
     */
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate[] path);

    /**
     * Calculate portions of the grid based on a pencil stretch operation.
     * LimittoEditArea defines whether the changes are limited to the active
     * edit area or not.
     * 
     * @param time
     * @param value
     * @param path
     * @param limitToEditArea
     * @return
     */
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate[] path,
            boolean limitToEditArea);

    /**
     * Populate the grid with values (if it is not already done)
     */
    public void populate();

    /**
     * Resets the save and publish times in the history.
     */
    public void resetSavePublishHistory();

    /**
     * Set a value at a specific grid coordinate. The appropriate WxValue class
     * must be used.
     * 
     * @param gridLoc
     *            the location in the grid
     * @param wxValue
     *            the value to set to
     */
    public void set(Point gridLoc, WxValue wxValue);

    /**
     * Set the underlying grid gridSlice
     * 
     * @param gridSlice
     */
    public void setGridSlice(IGridSlice gridSlice);

    /**
     * Sets the save times in the history.
     * 
     * @return
     */
    public boolean setSaveHistory();

    /**
     * Set a value to all points in the passed in grid where a bit is set.
     * 
     * @param aValue
     *            the value to set to
     * 
     * @param editArea
     *            the points that the value should be set
     * @return
     */
    public boolean setValue(WxValue aValue, Grid2DBit editArea);

    /**
     * Command from parm to indicate that a grid edit is beginning.
     */
    public void startGridEdit();

    /**
     * Updates the history
     * 
     * @param history
     * @return
     */
    public boolean updateHistory(GridDataHistory history);

    /**
     * Updates the history
     * 
     * @param history
     * @return
     */
    public boolean updateHistory(GridDataHistory[] history);

    /**
     * Updates the history to indicate grid has been modified. Returns true for
     * success.
     * 
     * @param modifier
     */
    public void updateHistoryToModified(WsId modifier);

    /**
     * Returns true if we are in ISC mode. If the iscCapable flag is set to
     * false, then always returns false
     * 
     * @return True if in ISC mode and iscCapable flag is set, else false
     */
    public boolean iscMode();

    /**
     * For scalar, returns the ISC grid merged with this grid. Returned
     * Grid2DBit reflects the valid points of the grid. If not in ISCmode,
     * returns an empty grid
     * 
     * @param t
     *            The time range to get
     * @param gridSlice
     *            The original grid gridSlice
     * @return the ISC grid merged with this grid
     */
    public Grid2DBit getISCGrid(Date t, ScalarGridSlice slice);

    /**
     * For Vector, returns the ISC grid merged with this grid. Returned
     * Grid2DBit reflects the valid points of the grid. If not in ISC mode,
     * returns empty grid.
     * 
     * @param t
     *            The time range to get
     * @param gridSlice
     *            The original grid gridSlice
     * @return The ISC grid merged with this grid
     */
    public Grid2DBit getISCGrid(Date t, VectorGridSlice slice);

    /**
     * For Discrete, returns the ISC grid merged with this grid. Returned
     * Grid2DBit reflects the valid points of the grid. If not in ISC mode,
     * returns empty grid.
     * 
     * @param t
     *            The time range to get
     * @param slice
     *            The original grid gridSlice
     * @return The ISC grid merged with this grid
     */
    public Grid2DBit getISCGrid(Date t, DiscreteGridSlice slice);

    /**
     * For Weather, returns the ISC grid merged with this grid. Returned
     * Grid2DBit reflects the valid points of the grid. If not in ISC mode,
     * returns empty grid.
     * 
     * @param t
     *            The time range to get
     * @param slice
     *            The original grid gridSlice
     * @return The ISC grid merged with this grid
     */
    public Grid2DBit getISCGrid(Date t, WeatherGridSlice slice);

    /**
     * Returns a Grid2DBit representing the valid points on the associated isc
     * grid, without this site's id. If not associated isc grid, then the
     * returned Grid2DBit is correctly sized. IncludeOwnSite true always sets
     * the bits that represent this site, regardless of whether there is an ISC
     * grid and what is in the ISC grid.
     * 
     * @param t
     *            The time range to get
     * @param includedOwnSite
     *            Whether to include own site
     * @return Grid2DBit representing the valid points on the associated isc
     *         grid.
     */
    public Grid2DBit iscPoints(Date t, boolean includedOwnSite);

    /**
     * Returns a Grid2DBit representing the my site's points for this grid.
     * 
     * @return A Grid2DBit representing the my site's points for this grid.
     */
    public Grid2DBit mySitePoints();

    /**
     * Returns a list of site ids that this grid contains
     * 
     * @return The list of site ids that this grid contains
     */
    public List<String> getHistorySites();

    /**
     * Forces a lock on this grid
     * 
     * @return true if successful, false if lock was not granted.
     */
    public boolean lockGrid();

    /**
     * Substitutes the given grid slice for "this" grid slice.
     * 
     * @param source
     * @return true for success.
     */
    public boolean replace(IGridData source);
}
