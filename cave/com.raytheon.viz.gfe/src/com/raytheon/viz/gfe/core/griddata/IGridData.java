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
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import org.locationtech.jts.geom.Coordinate;

/**
 * IGridData defines the common interface for the GridData hierarchy.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 29, 2008           chammack  Initial Class Skeleton.
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Changes to support IDataObject. Code cleanup
 * Jan 02, 2018  7705     randerso  Remove unneeded public method.
 *
 * </pre>
 *
 * @author chammack
 */
public interface IGridData extends Comparable<IGridData> {
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
     * @return true if successful
     */
    public boolean applyDelta(final Date time, float delta, boolean taper,
            Grid2DBit pointsToChange);

    /**
     * Changes the parm association for this GridData. This should only be
     * called by the Parm class.
     *
     * @param newParm
     *            The new Parm this GridData will be associated with.
     * @return true, if the swap was successful, false if not.
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
    public void changeValidTime(final TimeRange timeRange,
            boolean considerRate);

    /**
     * @return a copy of this object
     */
    public IGridData copy();

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
     * @return the contiguous area
     */
    public Grid2DBit getContiguousArea(final Date time, final Point location);

    /**
     * @return the underlying gridSlice. This must not be null.
     */
    @Deprecated
    public IGridSlice getGridSlice();

    /**
     * @return the underlying data object
     */
    public IDataObject getDataObject();

    /**
     * @return time the grid was last accessed
     */
    public long getLastAccessTime();

    /**
     * @return the grid information
     */
    public GridParmInfo getGridInfo();

    /**
     * @return the valid time of this grid
     */
    public TimeRange getGridTime();

    /**
     * @return list of grid histories
     */
    public List<GridDataHistory> getGridDataHistory();

    /**
     * @return array of grid histories
     */
    public GridDataHistory[] getHistory();

    /**
     * @return the parm that this grid data is associated with
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
     * @return the max grid
     */
    public IDataObject gridMax(IDataObject gridSlice);

    /**
     * Returns min of this grid and supplied grid.
     *
     * @param dataObject
     * @return the min grid
     */
    public IDataObject gridMin(IDataObject dataObject);

    /**
     * Returns new grid that is multiplied by factor.
     *
     * @param factor
     * @return the multiplied grid
     */
    public IDataObject gridMultiply(float factor);

    /**
     * Returns sum of supplied grid and this grid.
     *
     * @param dataObject
     * @return the grid sum
     */
    public IDataObject gridSum(IDataObject dataObject);

    /**
     * @return true if the grid is safe for editing
     */
    public boolean isOkToEdit();

    /**
     * @param editOp
     * @return true if editOp is supported by this GridData
     */
    public boolean isSupportedEditOp(EditOp editOp);

    /**
     * Returns true if the grid is valid
     *
     * @return true if the grid is valid
     *
     */
    public boolean isPopulated();

    /**
     * @return true if the grid has been modified by a user
     */
    public boolean isUserModified();

    /**
     * @return true if the grid is valid
     */
    public boolean isValid();

    /**
     * Returns null if the slice is valid, or a String if the slice is invalid.
     * The slice is invalid if the GridType is NONE, the GridDataHistory length
     * is 0, the time range is invalid, or the grid data is invalid.
     *
     * @return a String containing the invalid reason, or null if valid
     */
    public String validateData();

    /**
     * Copies the identified points and shifts them by delta. Return true for
     * success.
     *
     * @param time
     * @param pointsToMoveCopy
     * @param delta
     * @param copyOp
     * @return true if successful
     */
    public boolean moveCopyArea(final Date time,
            final Grid2DBit pointsToMoveCopy, final Point delta,
            boolean copyOp);

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
     * @return mask of changed grid cells
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
     * @return mask of changed grid cells
     */
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate[] path,
            boolean limitToEditArea);

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
     * Set the underlying dataObject
     *
     * @param dataObject
     */
    public void setDataObject(IDataObject dataObject);

    /**
     * Sets the save times in the history.
     *
     * @return true if successful
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
     * @return true if successful
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
     * @return true if successful
     */
    public boolean updateHistory(GridDataHistory history);

    /**
     * Updates the history
     *
     * @param history
     * @return true if successful
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
     * Returns the ISC grid merged with this grid. Returned Grid2DBit reflects
     * the valid points of the grid. If not in ISCmode, returns an empty grid
     *
     * @param t
     *            The time range to get
     * @return the ISC grid merged with this grid
     */
    public Pair<Grid2DBit, IGridData> getISCGrid(Date t);

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

    /**
     * Notify grid that is has been successfully saved
     */
    public void successfullySaved();

}
