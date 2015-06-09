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

package com.raytheon.uf.common.dataplugin.gfe.slice;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.time.TimeRange;

/**
 * IGridSlice defines a common interface to all types of GridSlices
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial creation.
 * 01/31/2008   879        rbell       Legacy conversion
 * Apr 23, 2015 4259       njensen     Added getNDArray()
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IGridSlice extends Comparable<IGridSlice> {

    /**
     * Assigns a gridSlice
     * 
     * @param gs
     *            the grid slice containing values to assign
     */
    public void assign(IGridSlice gs);

    /**
     * Returns the ValidTime of the slice
     * 
     * @return the range over which the data is valid
     */
    public TimeRange getValidTime();

    /**
     * Sets the history for the GridSlice
     * 
     * @param timeRange
     *            the timerange of the data
     */
    public void setValidTime(TimeRange timeRange);

    /**
     * Returns the history for the GridSlice
     * 
     * @return the history of the data
     */
    public GridDataHistory[] getHistory();

    /**
     * Sets the history for the GridSlice
     * 
     * @param history
     *            the history of the data
     */
    public void setHistory(GridDataHistory[] history);

    /**
     * Returns null if the slice is valid, or a String if the slice is invalid.
     * The slice is invalid if the GridType is NONE, the GridDataHistory length
     * is 0, the time range is invalid, or the grid data is invalid.
     * 
     * @return a String containg the invalid reason, or null if valid
     */
    public String isValid();

    /**
     * Return the grid information
     * 
     * @return the grid information
     */
    public GridParmInfo getGridInfo();

    /**
     * Sets the grid info for the GridSlice
     * 
     * @param gridParmInfo
     *            the grid info
     */
    public void setGridInfo(GridParmInfo gridParmInfo);

    /**
     * Clone the grid slice, cloning will not use a cache on cloned slice
     * 
     * @return a clone of the IGridSlice
     */
    public IGridSlice clone() throws CloneNotSupportedException;

    /**
     * Collapses the weather/discrete key/grid to not contain extra key
     * definitions. This is a no-op on non-weather/non-discrete data.
     */
    public void collapse();

    /**
     * Returns whether the slice is using a cache for the underlying data.
     * 
     * @return if the cache is currently enabled
     */
    public boolean getUseCache();

    /**
     * Update the slice to not keep any hard references to the underlying data
     * and use a cache mechanism to store the data.
     */
    public void setUseCache(boolean useCache);

    /**
     * Gets a representation of the underlying grid slice as an NDArray.
     * 
     * FIXME The returned NDArray object will have the x and y dimensions
     * reversed. That's what AWIPS 1 did and that makes the pre-existing python
     * code compatible. Java ordering is x,y while python is ordering is y,x.
     * It's confusing and questionable at best so someday someone should correct
     * all that. Good luck.
     * 
     * @return an NDArray or NDArray[]
     */
    public Object getNDArray();
}
