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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jep.INumpyable;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * 
 * BEGIN LEGACY DOCUMENTATION
 * 
 * A GridSlice is a part of a DataSlice. A GridSlice contains either scalar,
 * vector, or weather data, its attributes, its history, and its valid time
 * range.
 * 
 * END LEGACY DOCUMENTATION
 * 
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial Creation.
 * 01/31/2008   879        rbell       Legacy conversion
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public abstract class AbstractGridSlice implements IGridSlice,
        Comparable<IGridSlice>, INumpyable, ISerializableObject {

    @DynamicSerializeElement
    protected TimeRange validTime;

    @DynamicSerializeElement
    protected GridParmInfo gridParmInfo;

    @DynamicSerializeElement
    protected List<GridDataHistory> gridDataHistory;

    protected boolean useCache = false;

    /**
     * Constructor for serialization only.
     */
    protected AbstractGridSlice() {

    }

    /**
     * Constructor that uses a Timerange and GFERecord. The data grid is still
     * null.
     * 
     * @param validTime
     *            TimeRange to use
     * @param gfeRecord
     *            GFERecord to use
     */
    protected AbstractGridSlice(TimeRange validTime, GFERecord gfeRecord) {
        this.validTime = validTime;
        List<GridDataHistory> hist = new ArrayList<GridDataHistory>();
        hist.addAll(gfeRecord.getGridHistory());
        this.gridDataHistory = hist;
        this.gridParmInfo = gfeRecord.getGridInfo();
    }

    /**
     * Constructor that uses a TimeRange, GridParmInfo and GridDataHistory
     * 
     * @param validTime
     *            TimeRange to use
     * @param gpi
     *            grid parm info
     * @param gdh
     *            grid history
     */
    protected AbstractGridSlice(TimeRange validTime, GridParmInfo gpi,
            GridDataHistory[] gdh) {
        this.validTime = validTime;
        this.gridParmInfo = gpi;
        this.gridDataHistory = new ArrayList<GridDataHistory>(
                Arrays.asList(gdh));

    }

    /**
     * Copy constructor
     * 
     * @param rhs
     *            AbstractGridSlice to be constructed from
     */
    protected AbstractGridSlice(AbstractGridSlice rhs) {
        this.validTime = rhs.validTime.clone();
        this.gridDataHistory = rhs.gridDataHistory;
        this.gridParmInfo = rhs.gridParmInfo;
        this.useCache = rhs.useCache;
    }

    @Override
    public GridDataHistory[] getHistory() {
        return this.gridDataHistory
                .toArray(new GridDataHistory[this.gridDataHistory.size()]);
    }

    @Override
    public TimeRange getValidTime() {
        return this.validTime;
    }

    @Override
    public GridParmInfo getGridInfo() {
        return this.gridParmInfo;
    }

    @Override
    public void assign(IGridSlice rhs) {
        if (!(rhs instanceof AbstractGridSlice)) {
            throw new IllegalArgumentException(
                    "Attemped to assign AbstractGridSlice to non-AbstractGridSlice object");
        }

        AbstractGridSlice rhsAbstractGridSlice = (AbstractGridSlice) rhs;

        this.validTime = rhsAbstractGridSlice.validTime.clone();
        if (rhsAbstractGridSlice.gridParmInfo != null) {
            this.gridParmInfo = rhsAbstractGridSlice.gridParmInfo.clone();
        } else {
            this.gridParmInfo = null;
        }
        this.gridDataHistory.clear();
        for (GridDataHistory thisGDH : rhs.getHistory()) {
            if (thisGDH != null) {
                try {
                    this.gridDataHistory.add(thisGDH.clone());
                } catch (CloneNotSupportedException e) {
                    this.gridDataHistory.add(null);
                }
            } else {
                this.gridDataHistory.add(null);
            }
        }
    }

    @Override
    public int compareTo(IGridSlice o) {
        if (!(o instanceof AbstractGridSlice)) {
            throw new IllegalArgumentException(
                    "Attempt to compare to non-AbstractGridSlice");
        }

        AbstractGridSlice rhs = (AbstractGridSlice) o;
        return validTime.compareTo(rhs.validTime);
    }

    @Override
    public boolean equals(Object rhs) {
        if (!(rhs instanceof AbstractGridSlice)) {
            return false;
        }

        AbstractGridSlice rhsAbstractGridSlice = (AbstractGridSlice) rhs;

        if (!validTime.equals(rhsAbstractGridSlice.validTime)) {
            return false;
        }

        if (!gridParmInfo.equals(rhsAbstractGridSlice.gridParmInfo)) {
            return false;
        }

        for (GridDataHistory thisGDH : this.gridDataHistory) {
            if (!rhsAbstractGridSlice.gridDataHistory.contains(thisGDH)) {
                return false;
            }
        }

        for (GridDataHistory thisGDH : rhsAbstractGridSlice.gridDataHistory) {
            if (!this.gridDataHistory.contains(thisGDH)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public String isValid() {
        if (this.gridParmInfo.getGridType() == GridType.NONE) {
            return "AbstractGridSlice type is NONE";
        }

        if (gridDataHistory.size() == 0) {
            return "Grid history length 0";
        }

        if (!validTime.isValid()) {
            return "AbstractGridSlice time range is not valid";
        }

        // if (!gridParmInfo.isValid()) {
        // return "AbstractGridSlice time range is not valid";
        // }

        // must be good if we got here!
        return null;
    }

    @Override
    public void setHistory(GridDataHistory[] history) {
        this.gridDataHistory = new ArrayList<GridDataHistory>(
                Arrays.asList(history));
    }

    @Override
    public void setGridInfo(GridParmInfo gridParmInfo) {
        this.gridParmInfo = gridParmInfo;
    }

    @Override
    public void setValidTime(TimeRange timeRange) {
        this.validTime = timeRange;
    }

    @Override
    public String toString() {
        StringBuilder rVal = new StringBuilder();
        rVal.append("GRIDSLICE GridType: ").append(gridParmInfo.getGridType())
                .append(" ");
        rVal.append("GridInfo: ").append(gridParmInfo);
        rVal.append(" ValidTime: ").append(validTime).append(" ");
        rVal.append("GridHistory: ").append(gridDataHistory).append(" ");

        return rVal.toString();
    }

    @Override
    public AbstractGridSlice clone() throws CloneNotSupportedException {
        throw new UnsupportedOperationException(
                "Cannot clone abstract grid slice");
    }

    /**
     * @return the gridParmInfo
     */
    public GridParmInfo getGridParmInfo() {
        return gridParmInfo;
    }

    /**
     * @param gridParmInfo
     *            the gridParmInfo to set
     */
    public void setGridParmInfo(GridParmInfo gridParmInfo) {
        this.gridParmInfo = gridParmInfo;
    }

    /**
     * @return the gridDataHistory
     */
    public List<GridDataHistory> getGridDataHistory() {
        return gridDataHistory;
    }

    /**
     * @param gridDataHistory
     *            the gridDataHistory to set
     */
    public void setGridDataHistory(List<GridDataHistory> gridDataHistory) {
        this.gridDataHistory = gridDataHistory;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice#collapse()
     */
    @Override
    public void collapse() {
        // do nothing by default
    }

    public boolean getUseCache() {
        return this.useCache;
    }

    public void setUseCache(boolean useCache) {
        if (this.useCache != useCache) {
            this.useCache = useCache;
            if (useCache) {
                moveDataToLocalCache();
            } else {
                moveDataToMem();
            }
        }
    }

    /**
     * Called if data is currently in memory and should be moved to use cache.
     */
    abstract protected void moveDataToLocalCache();

    /**
     * Called if data is currently in cache and should be moved to in memory.
     */
    abstract protected void moveDataToMem();
}
