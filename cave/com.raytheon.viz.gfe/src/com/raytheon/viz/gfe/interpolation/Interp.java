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
package com.raytheon.viz.gfe.interpolation;

import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;

/**
 * Abstract base class for objects which are used to contain math algorithms to
 * make one interpolated grid of data. This manages the data common to its
 * derived classes. Interp derived classes have the following responsibilities:
 * examine the sequence of grid slices and determine which require
 * interpolation; interpolate one grid at a time upon command from the
 * Interpolator.
 * 
 * Interpolation is based on a set of grid slices and time ranges. Some come
 * with defined data, called the "base" grid slices. This data is used to
 * control interpolation. Others have no grid data in them yet but they do have
 * start and end times defined. The missing grid data will be generated here by
 * interpolation and put into the missing grids. Only one grid is interpolated
 * at a time, on the call to the interpolate function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 30, 2008		#1161	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class Interp {
    protected final ParmID _parmid;

    protected final GridParmInfo gridParmInfo;

    protected final List<TimeRange> _gridTimes;

    private List<IGridSlice> _dataslices;

    private int[] baseDataIndices;

    private Date[] knownTimeValues;

    // private final String _parameterName;

    /**
     * Constructor for abstract base class Interp taking a parameter name, a
     * sequence of grid slices that contains base data and "NULL" grid slices to
     * be interpolated, and a sequence of indices that point to the base data in
     * "data".
     * 
     * @param dataslices
     * @param baseDataIndices
     * @param parmid
     * @param gridparminfo
     * @param gridTimes
     */
    public Interp(List<IGridSlice> dataslices, final int[] baseDataIndices,
            final ParmID parmid, final GridParmInfo gridparminfo,
            final List<TimeRange> gridTimes) {
        this._parmid = parmid;
        this.gridParmInfo = gridparminfo;
        this._gridTimes = gridTimes;
        this._dataslices = dataslices;
        this.baseDataIndices = baseDataIndices;

        initializeKnownTimeValues();
    }

    /**
     * Called to interpolate one data gridSlice.
     * 
     * @param index
     *            index of the data gridSlice to be interpolated
     * @return
     */
    public abstract IGridSlice interpolate(int index);

    /**
     * Called when Interp is no longer need to allow data structures to be freed
     * up
     * 
     */
    public void dispose() {
        // do nothing base method to be overidden as necessary in derived
        // classes
    }

    /**
     * Determine fractional time of interpolation grid from first known grid to
     * second known grid. Should be a number t, 0.0 < t < 1.0.
     * 
     * Uses nearest adjacent time of grids as measure of distance - the ending
     * time for the preceding grid, and the starting time for the following
     * grid. This prevents a grid gridSlice much longer in time than others from
     * skewing the interpolated result too far from its values, since otherwise
     * its midpoint time is too far from the interpolated grid gridSlice's time.
     * 
     * @param index
     * @return
     */
    protected float findTimeFraction(int index) {
        // get the grid gridSlice with the given "index",
        // the one needing interpolation.
        // GridSlice &ds = gridSlice(index); // an Interp class inline function

        // make its interp time (mid point time)
        Date interpTime = _gridTimes.get(index).getCenterTime();

        // Determine indices and times for neighboring base gridslices

        int before, after; // indices for known data before and after
        // interpTime

        int bdCount; // counter of base (known) grid slices

        // time Relative to first gridslice in the request, long integer type
        long ritime;

        // times of the known grid slices before and after interpolation time
        double beforeTime = 0.0, afterTime = 0.0;

        // knownTimeValues exists for all grid slices in the request;

        // Look through all the grid slices to find the two bracketing
        // interpTime.
        for (bdCount = 1; bdCount < getNumberOfBaseSlices(); bdCount++) {
            if ((getKnownTimeValues()[getBaseDataIndices()[bdCount - 1]]
                    .compareTo(interpTime) < 0)
                    && (getKnownTimeValues()[getBaseDataIndices()[bdCount]]
                            .compareTo(interpTime) >= 0)) {
                // Have found the pair of base or known grid slices bracketing
                // the
                // one to interpolate.

                // Keep indices to the base gridslices before and after interp
                // time,
                // in the array of times of all the grid slices,
                // and in the array of grid slices.
                before = getBaseDataIndices()[bdCount - 1];
                after = getBaseDataIndices()[bdCount];

                // determine times of before and after known data as "double"
                // (use time relative to first ds in the request, and integer
                // type)

                // use the ending time of the preceding known grid gridSlice
                Date knownDataTime = _gridTimes.get(before).getEnd();
                ritime = knownDataTime.getTime()
                        - getKnownTimeValues()[0].getTime();
                // convert integer to double type
                beforeTime = ritime;

                // use start time of the following known grid gridSlice
                knownDataTime = _gridTimes.get(after).getStart();
                ritime = knownDataTime.getTime()
                        - getKnownTimeValues()[0].getTime();
                afterTime = ritime;

                break;
            }
        }

        // what if afterTime = beforeTime ?
        // means grid gridSlice to interpolate is of zero length.
        // Should never have gotten this far!
        if (beforeTime == afterTime) {
            Activator
                    .getDefault()
                    .getLog()
                    .log(
                            new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                                    "Grid gridSlice to interpolate of 0 time length in the request"));
            return 0.0f;
        }

        // In the same way,
        // make a relative time of the interp time, of double type
        ritime = interpTime.getTime() - getKnownTimeValues()[0].getTime();
        double itime = ritime;

        // now we can find the time fraction
        float fraction = (float) ((itime - beforeTime) / (afterTime - beforeTime));

        // System.out.println(" findTimeFraction: beforeTime is " + beforeTime);
        // System.out.println(" findTimeFraction: afterTime is " + afterTime);
        // System.out.println(" findTimeFraction: interpTime is " + itime);
        // System.out.println(" findTimeFraction: time fraction is " +
        // fraction);

        return fraction;
    }

    public int[] getBaseDataIndices() {
        return baseDataIndices;
    }

    protected IGridSlice getGridSlice(int index) {
        return this._dataslices.get(index);
    }

    public Date[] getKnownTimeValues() {
        return knownTimeValues;
    }

    protected int getNumberOfBaseSlices() {
        return this.baseDataIndices.length;
    }

    /**
     * Routine that computes working (ABS) time values for ALL gridslices in the
     * request's SeqOf<GridSlices>. Called at construction of an Interp-derived
     * object.
     * 
     * Allocates the appropriate amount of memory for ALL grid slices in the
     * request. Then goes through _dataslices, and sets the mid-point of the
     * time range as the grid's time (for interpolation purposes), and stores it
     * into _knownTimeValues.
     * 
     * Special case: if the first and last (and therefore "base") grid slices
     * are of unequal length, interpolation will be skewed if the mid-point
     * times of these two slices are used. In that case use the innermost times
     * of these two slices (the end time for the first gridSlice, the start time for
     * the last gridSlice).
     * 
     */
    private void initializeKnownTimeValues() {
        // set length of _knownTimeValues
        knownTimeValues = new Date[_gridTimes.size()];

        int n = _gridTimes.size() - 1; // highest index allowed

        for (int i = 0; i <= n; i++) {
            if (i == 0 || i == n) {
                // if the time lengths of the first and last known grid slices
                // are not the same, then use inner-most times for these
                // two slices
                if (_gridTimes.get(0).getDuration() != _gridTimes.get(n)
                        .getDuration()) {
                    if (i == 0) {
                        knownTimeValues[0] = _gridTimes.get(0).getEnd();
                    }

                    if (i == n) {
                        knownTimeValues[n] = _gridTimes.get(n).getStart();
                    }

                    continue; // go to next iteration of loop
                }
            }

            // the mid-point time of the grid gridSlice is = the start time
            // plus half the duration
            knownTimeValues[i] = _gridTimes.get(i).getCenterTime();
        }

        return;
    }

    public GridParmInfo getGridParmInfo() {
        return gridParmInfo;
    }
}
