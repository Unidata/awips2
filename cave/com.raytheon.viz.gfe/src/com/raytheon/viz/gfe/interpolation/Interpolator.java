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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;

/**
 * <pre>
 *  USE OF THE INTERPOLATOR (in code)
 *  1. Create an Interpolator object.
 *  2. Call Interpolator::requestInterpolation(const ParmID&amp; parmid,
 *  const GridParmInfo&amp; gridparminfo,
 *  const SeqOf&lt;TimeRange&gt;&amp; interptimes, SeqOf&lt;GridSlice&gt;&amp; dataslices);
 *  to set up an interpolation request. The input values are based
 *  on a sequence of gridslices with this related data:
 *  parmid a ParmID for those grids
 *  gridparminfo  a GridParmInfo for the grids
 *  interptimes an array of timeranges, one for each grid in the
 *  request, including times of both base grids and grids to be made.
 *  dataslices an array of gridslices. They are of two kinds. 1.&quot;Base&quot;&quot; grids
 *  are grid slices fully defined (with a data grid, time range,
 *  gridtype, etc.) used to control interpolation, and 2. grids to be
 *  interpolated of gridtype &quot;NONE&quot; and no other attributes.
 *  There must be at least two base grid slices in the request, and
 *  there must be some non-base type grids to interpolate, between the
 *  base grids, for the request to do anything. Does not extrapolate
 *  beyond the end base gridslices, so there should be base gridslices
 *  at each end of the &quot;dataslices&quot; and &quot;interipmes&quot;. NONE type grid slices
 *  on the ends will be ignored.
 *  Note that &quot;interptimes&quot; and &quot;dataslices&quot; must be parallel arrays
 *  with the proper
 *  time ranges, in order, one for each grid gridSlice. This data structure
 *  is required since NONE type gridslices now cannot carry their own
 *  time range like they used to.
 * 
 *  3. Once the request has been submitted, use code like this to
 *  create the new interpolated grid slices:
 *     GridSlice newslice;
 *     while(I.interpolate(GridSlice&amp; newslice))
 *     {
 *        // the next newslice has been made if returns true;
 *     }
 * 
 *  I.interpolate(newslice) will return false and quit the while loop
 *  when all grids have been made. You don't have to keep count how many.
 * 
 *  That's all.
 * 
 *  -------------------------------------------------------------------------
 * 
 *  Formal description.
 *  An Interpolator object
 *  - receives requests to interpolate a seq of grid slices  
 *    (via input argument list)
 *  - rejects any overlapping time requests
 *  - maintains a queue of items to interpolate
 *  - adds requests to the queue - each single &quot;request&quot; can hold a long
 *    sequence of grid slices
 *  - when one calls Interpolator::interpolate(),
 *    for one data gridSlice to be interpolated, then
 *  - gets index of next request, and index of gridslice in that request
 *    to interpolate
 *  - determines proper type of Interp derived class object depending on
 *    the dataType, and instantiates it (with function createInterpClass)
 *  - calls interp-&gt;interpolate(index) in that interp object
 *  - handles interpolation statistics: can reset to zero, or output them.
 * 
 *  -- implementation ---------------------------------------------------------
 *  &quot;Base&quot; grids are grids with real values,
 *  known before interpolation, and used to control interpolation.
 *  They also have a valid time range.
 * </pre>
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

public class Interpolator {
    public static enum Algorithm {
        LINEAR_NOADVECT("Linear/NoAdvection"), //
        CUBIC_NOADVECT("Cubic/NoAdvection"), //
        LINEAR_ADVECT("Linear/Advection"), //
        CUBIC_ADVECT("Cubic/Advection"); //

        private String displayString;

        private Algorithm(String displayString) {
            this.displayString = displayString;
        }

        /**
         * Get the human readable string to be displayed in dialogs, etc.
         * 
         * @return display string
         */
        public String getDisplayString() {
            return displayString;
        }
    };

    // the queue of requests due for interpolation
    private List<InterpRequest> _interpQueue;

    // pointer to the current object to do interpolations
    private Interp _interp;

    // a timer for determining interpolation time per data gridSlice
    private long gridSliceTime;

    // a structure of statistics kept per parm name
    private class InterpStats {
        long gridSliceSentTotal; // count of ds in each request

        long gridSliceSentMax; // max ds in a request

        long gridSliceSentMin; // min ds in a request

        long gridSliceInterpTotal; // count of ds to be interp

        long gridSliceInterpMax; // max interp ds in a request

        long gridSliceInterpMin; // min interp ds in a request

        long numberOfRequests; // count of number of requests

        long numberOfGoodRequests; // count of good requests

        long numberOfBadRequests; // count of bad requests

        double interpReqTimeTotal; // time for all interpolation requests

        double interpReqTimeMax; // max time for an interpolation request

        double interpReqTimeMin; // min time for an interpolation request

        double gridSliceIntTimeTotal; // time for all data gridSlice interpol

        double gridSliceIntTimeMax; // max time for 1 ds interpolation

        double gridSliceIntTimeMin; // min time for 1 ds interpolation

        long gridSliceCount; // totalcounts for ds interpolated

        public InterpStats() {
            gridSliceSentTotal = 0;
            gridSliceSentMax = Long.MIN_VALUE;
            gridSliceSentMin = Long.MAX_VALUE;

            gridSliceInterpTotal = 0;
            gridSliceInterpMax = Long.MIN_VALUE;
            gridSliceInterpMin = Long.MAX_VALUE;

            numberOfRequests = 0;
            numberOfGoodRequests = 0;
            numberOfBadRequests = 0;

            interpReqTimeTotal = 0.0;
            interpReqTimeMax = Double.NEGATIVE_INFINITY;
            interpReqTimeMin = Double.POSITIVE_INFINITY;

            gridSliceIntTimeTotal = 0.0;
            gridSliceIntTimeMax = Double.NEGATIVE_INFINITY;
            gridSliceIntTimeMin = Double.POSITIVE_INFINITY;

            gridSliceCount = 0;
        }
    };

    private Map<String, InterpStats> statistics; // statistics per parm

    /**
     * Constructor for interpolator class
     * 
     * sets the pointer to the _interp object (not yet made) to NULL
     */
    public Interpolator() {
        _interp = null;
        statistics = new HashMap<String, InterpStats>();
        _interpQueue = new ArrayList<InterpRequest>();
    }

    @Override
    protected void finalize() throws Throwable {
        dispose();
        super.finalize();
    }

    /**
     * "destructor" for Interpolator class
     * 
     * Delete _interp the associated interpolation object.
     * 
     * Delete each request in the queue.
     * 
     * Delete stuff in statistics.
     */
    public void dispose() {
        _interp = null;

        if (_interpQueue != null) {
            _interpQueue.clear();
        }

        resetStatistics();
    }

    /**
     * Make interpolation queue entry(ies) from the input: ParmID parmid
     * GridParmInfo gridparminfo SeqOf<TimeRange>& interptimes - all times of
     * relevant grids, both base data grids and new grids to be made by interp.
     * SeqOf<GridSlice>& dataslices - an array of grids, some of base values,
     * and some of type "NONE" to be interpolated. Should be exactly parallel to
     * array of times "interptimes" which lists the times of all grids both base
     * and "NONE."
     * 
     * Note this code may make more than one "request" from one call here.
     * 
     * Returns true if the data set is valid and interpolation can proceed.
     * Returns false if the data set is invalid and interpolation cannot
     * proceed. -- implementation
     * ----------------------------------------------------- For purposes of
     * interpolation the time to use for base or known grids is the beginning or
     * start time of the associated time range since that is when the
     * model-based grids are actually valid in the present GFE design.
     * 
     * @param parmID
     * @param gridParmInfo
     * @param interpTimes
     * @param dataSlices
     * @param algorithm
     * @return
     */
    public boolean requestInterpolation(final ParmID parmID,
            final GridParmInfo gridParmInfo, final TimeRange[] interpTimes,
            IGridSlice[] dataSlices, Algorithm algorithm) {
        int i, j;
        boolean okay = true;

        // Set how many known or base data values to use in making
        // the spline curve, on each end of the time interval where
        // interpolation
        // is needed. 2 end points makes nice smooth curves;
        // 1 makes linear plots - generally not satisfactory. Looks
        // like sawtooth in time series plots, and weather never does that.
        // The trivial additional time required for the extra point
        // in going from 1 to 2 makes a great improvement in interpolated
        // values.
        // 3 is not required; only changes quality of smooth curve a little.
        int numbEndPts = 0;
        boolean advection = false;
        switch (algorithm) {
        case LINEAR_NOADVECT:
            numbEndPts = 1;
            break;
        case LINEAR_ADVECT:
            advection = true;
            numbEndPts = 1;
            break;
        case CUBIC_NOADVECT:
            numbEndPts = 2;
            break;
        case CUBIC_ADVECT:
            advection = true;
            numbEndPts = 2;
            break;
        default:
            numbEndPts = 2;
            break;
        }

        // logDebug << " " << std::endl;
        // logDebug << " Incoming interp request for " << std::endl;
        // logDebug << " parm name " << parmid.parmName() << std::endl;
        // logDebug << " total number data slices " <<
        // dataslices.length() << std::endl;

        if (dataSlices.length < 1) {
            Activator
                    .getDefault()
                    .getLog()
                    .log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "No data slices to interpolate in request.\n"
                                    + " dataslices.length() = "
                                    + dataSlices.length));
            return false;
        }

        // The following code divides entire list of incoming interpolation
        // requests into smaller ones, each centered on a group of 1 or more
        // contiguous empty GridSlices, bracketed between "numbEndPts" of
        // base (filled) data grids; one base data grid at least at each end.
        i = 0;
        while (i < dataSlices.length) {
            // if this gridSlice is an "empty data gridSlice", need to
            // interpolate here
            if (dataSlices[i] == null) {
                // Have found the first empty data gridSlice in this bunch.
                // Create a new working array of gridslices.
                LinkedList<IGridSlice> interpSlices = new LinkedList<IGridSlice>();
                // and create a parallel array of time ranges for each
                // grid, since "NONE" type grids can't carry their
                // own time range.
                LinkedList<TimeRange> gridTimes = new LinkedList<TimeRange>();
                // Search backwards starting at previous index (i-1)
                // and find the "numbEndPts" preceeding base or filled
                // gridslices (ignoring empties mixed in)
                // and prepend them to the array of grids for the request.
                int count = 0;
                for (j = i - 1; j >= 0 && count < numbEndPts; j--) {
                    if (dataSlices[j] != null) {
                        interpSlices.addFirst(dataSlices[j]); // prepend to
                        // list
                        gridTimes.addFirst(interpTimes[j]);
                        count++;
                        // logDebug << "Use base data at index " << j <<
                        // std::endl;
                    }
                }
                // now add in all empty data slices from this i to next base
                // grid
                for (; i < dataSlices.length; i++) {
                    if (dataSlices[i] == null) {
                        interpSlices.add(dataSlices[i]);
                        gridTimes.add(interpTimes[i]);
                        // logDebug << "Interpolate at index " << i <<
                        // std::endl;
                    } else {
                        break; // have hit the next base grid so go on to next
                        // step
                    }
                }

                // Now add in the next "numbEndPts" base grids
                // (or only one if there is only one base data gridSlice
                // remaining).
                // i is now pointing to the
                // first non-NONE-type or base data gridSlice after the set of
                // empties)
                count = 0;
                for (j = i; j < dataSlices.length && count < numbEndPts; j++) {
                    if (dataSlices[j] != null) {
                        interpSlices.add(dataSlices[j]);
                        gridTimes.add(interpTimes[j]);
                        count++;
                        // logDebug << "Use base data at index " << j <<
                        // std::endl;
                    }
                }

                // logDebug << " one request is complete; index is "<<i<<
                // std::endl;

                // i is the index of the first base gridslice beyond the
                // group of empties just added. That is, parked at a base grid.

                // Note the first and last gridslices in "interpSlices"
                // are copied from base gridslices with valid real data
                // and defined real time ranges. Not NONE gridSlices.

                // create the request packet object
                InterpRequest request = new InterpRequest(parmID, gridParmInfo,
                        interpSlices, gridTimes, advection);

                updateRequestStatCounts(request); // update statistics

                // Check if this interp request overlaps a previous request
                // to interpoalate the same parm.
                // ensure no overlaps for same parameter
                boolean thisOkay = true;

                TimeRange requestDSSpan = request.interpolateSliceSpan();

                for (InterpRequest req : _interpQueue) {
                    if (parmID == req.getParmID()
                            && requestDSSpan.overlaps(req
                                    .interpolateSliceSpan())) {
                        Activator
                                .getDefault()
                                .getLog()
                                .log(new Status(IStatus.ERROR,
                                        Activator.PLUGIN_ID,
                                        "Outstanding interpolation request for "
                                                + parmID + " " + requestDSSpan
                                                + ". Request denied."));
                        request = null; // free the data structure
                        okay = thisOkay = false;
                        break;
                    }
                }

                // add the interpolation request to the queue
                if (thisOkay) {
                    _interpQueue.add(request);
                    // logDebug<<" length of interpQueue is "<<
                    // _interpQueue.length()<<std::endl;
                    // logDebug << " submit request to interpolate with " <<
                    // interpSlices.length() << " grids, from " <<
                    // gridTimes[0].startTime() << " to " <<
                    // gridTimes[interpSlices.length()-1].endTime() <<
                    // std::endl;
                }
            } else // data gridSlice is not empty; try next one by increasing
                   // index
            {
                // logDebug << " gridslice at " << i << " is not
                // empty"<<std::endl;
                i++;
            }
        }

        return okay;
    }

    /**
     * Interpolate next data grid on the request queue "_interpQueue." Returns
     * true if a data gridSlice was made by interpolation, or Returns false if
     * there were no data slices to process. And Returns, via the argument list,
     * a reference to the new GridSlice made by interpolation.
     * 
     * Assumes that the requests on the queue are valid.
     * 
     * 
     * The data gridSlice to be interpolated is managed by the Interpolator, and
     * other child classes of Interp class.
     * 
     * Has a tricky stop where this function calls itself after a request is
     * complete, to handle the next request. And so on until finally we reach
     * bootm and leave the function.
     * 
     * @return
     */
    public IGridSlice interpolate() {
        // If there is nothing left in the interpolation queue, return null.
        // This is the final and proper return when all done with all requests.
        // (also would exit here if there was nothing in the request
        // when first called, but that is not the amin intent.)
        if (_interpQueue.size() == 0) {
            return null;
        }

        // get top entry on request queue
        InterpRequest request = _interpQueue.get(0);
        // and get the index to next gridslice to interpolate
        int nextIndex = request.getNextInterpolationIndex();

        // if all done with this request
        if (nextIndex == -1) {
            updateRequestStatTimes(request); // update the statistics
            request = null; // free the memory for the request
            _interpQueue.remove(0); // remove the top entry from the queue
            _interp.dispose();
            _interp = null; // delete the interpolation routines object

            // call itself to see if there is ANOTHER request in the
            // interpQueue.
            return interpolate();
        }

        // Is an interp-type object made? If not,
        // create an interp-type object for this request.
        // The construction here of an object (derived from Interp class)
        // does all the set-up needed to later call for interpolation
        // in this data set.
        if (_interp == null) {
            createInterpClass(request);
        }

        // start the timer
        long t0 = System.currentTimeMillis();

        // interpolate the next data gridSlice
        IGridSlice gridMade = _interp.interpolate(nextIndex);

        // stop the timer
        this.gridSliceTime = System.currentTimeMillis() - t0;

        updateInterpStatTimes(request); // update statistics

        return gridMade;
    }

    /**
     * Utility routine to create an Interp class derived object "_interp" based
     * on the data type in the interpolation request. Assumes that the
     * interpolation request is valid.
     * 
     * @param request
     */
    private void createInterpClass(InterpRequest request) {
        switch (request.getGridParmInfo().getGridType()) {
        case SCALAR:
            // cases of weather scalar data that moves around in bounded areas,
            // surrounded by "none present" values equal to 0.0,
            // and so should be advected. Rain, clouds, etc.
            if (request.isAdvected()) {
                _interp = new ScalarAdvectInterp(request.getDataSlices(),
                        request.baseDataIndices(), request.getParmID(),
                        request.getGridParmInfo(), request.getRequestTimes());
            } else {
                // for continous values that always cover all the grid
                // without gaps or bounded areas, where 0.0 has no special
                // meaning, such as temperature.
                _interp = new ScalarInterp(request.getDataSlices(),
                        request.baseDataIndices(), request.getParmID(),
                        request.getGridParmInfo(), request.getRequestTimes());
            }
            break;
        case VECTOR:
            // for wind
            _interp = new VectorInterp(request.getDataSlices(),
                    request.baseDataIndices(), request.getParmID(),
                    request.getGridParmInfo(), request.getRequestTimes());
            break;

        case WEATHER:
            // special GFE "weather" data type
            _interp = new WeatherInterp(request.getDataSlices(),
                    request.baseDataIndices(), request.getParmID(),
                    request.getGridParmInfo(), request.getRequestTimes());
            break;
        case DISCRETE:
            // special GFE "discrete" data type
            _interp = new DiscreteInterp(request.getDataSlices(),
                    request.baseDataIndices(), request.getParmID(),
                    request.getGridParmInfo(), request.getRequestTimes());
            break;

        default:
            // logBug
            // << "Invalid grid type found in the interp. request, type# "
            // << (unsigned long)request.gridType() << std::endl;
            break;
        }

        return;
    }

    /**
     * Removes all values in the statistics object. Then goes through the
     * outstanding interpolation requests and adds them back into the statistics
     * object.
     * 
     */
    public void resetStatistics() {
        this.statistics.clear();

        for (InterpRequest request : this._interpQueue) {
            updateRequestStatCounts(request);
        }
    }

    /**
     * <pre>
     * Outputs the statistics in the following form:
     * 
     * 0000000111111111122222222223333333333444444444455555555556666666666777777777
     * --------------------- Interpolation Request Statistics ----------------------
     * --ParameterName--  ---Data Slices Sent---   --Data Slices Interpolated--
     *                     Total   Max  Min  Ave       Total   Max  Min  Ave
     *                    1234567 1234 1234 123.4     1234567 1234 1234 123.4
     *                   
     * --ParameterName--  --Number of Requests--   ----Time Requests on Queue----
     *                     Total     Good    Bad    Total       Max     Min      Ave
     *                   1234567  1234567   12345   99999.999 9999.999 9999.999 9999.999
     *                 
     * --ParameterName--    Individual Data Slice Interpolation
     *                    TotalTime   MaxTime  MinTime  TotalCount
     *                    99999.999  9999.999 9999.999  123456789
     * </pre>
     * 
     * @return string containing statistics
     */
    public String outputStatistics() {
        double average;
        String emptyPName = "                    "; // 20 spaces

        StringBuilder s = new StringBuilder();
        s.append("-------------------- Interpolation Request Statistics --------------------\n");

        s.append("--ParameterName--   ---Data Slices Sent---   --Data Slices Interpolated--\n");

        s.append(emptyPName).append(
                " Total   Max  Min  Ave       Total   Max  Min  Ave\n");

        List<String> keys = new ArrayList<String>(statistics.keySet());
        Collections.sort(keys);

        for (String key : keys) {
            InterpStats stats = statistics.get(key);
            s.append(String.format("%-19s %7d %4d %4d", key,
                    stats.gridSliceSentTotal, stats.gridSliceSentMax,
                    stats.gridSliceSentMin));

            if (stats.numberOfRequests > 0) {
                average = (double) stats.gridSliceSentTotal
                        / (double) stats.numberOfRequests;
                s.append(String.format(" %5.1f", average));
            } else {
                s.append(" -----");
            }

            s.append(String.format("     %7d %4d %4d",
                    stats.gridSliceInterpTotal, stats.gridSliceInterpMax,
                    stats.gridSliceInterpMin));

            if (stats.numberOfRequests > 0) {
                average = (double) stats.gridSliceInterpTotal
                        / (double) stats.numberOfRequests;
                s.append(String.format(" %5.1f", average));
            } else {
                s.append(" -----");
            }

            s.append('\n');
        }

        s.append("\n--ParameterName--  --Number of Requests--  "
                + "----Time Requests on Queue----\n");

        s.append(emptyPName).append(
                " Total   Good   Bad    Total     Max     Min     Ave\n");

        for (String key : keys) {
            InterpStats stats = statistics.get(key);

            s.append(String.format("%-19s %7d %5d %5d", key,
                    stats.numberOfRequests, stats.numberOfGoodRequests,
                    stats.numberOfBadRequests));

            s.append(String.format("   %7.3f %7.3f %7.3f",
                    stats.interpReqTimeTotal, stats.interpReqTimeMax,
                    stats.interpReqTimeMin));

            if (stats.numberOfGoodRequests > 0) {
                average = stats.interpReqTimeTotal / stats.numberOfGoodRequests;
                s.append(String.format(" %7.3f", average));
            } else {
                s.append(" ------");
            }

            s.append("\n");
        }

        // output the individual data gridSlice interpolation timing statistics

        s.append("\n--ParameterName--  --Individual Data Grid Interpolation-- \n");

        s.append(emptyPName).append(
                "TotalTime  MaxTime  MinTime   AveTime    Num. Interpd\n");

        for (String key : keys) {
            InterpStats stats = statistics.get(key);

            s.append(String.format("%-19s   %7.3f  %7.3f  %7.3f", key,
                    stats.gridSliceIntTimeTotal, stats.gridSliceIntTimeMax,
                    stats.gridSliceIntTimeMin));

            if (stats.gridSliceCount > 0) {
                average = stats.gridSliceIntTimeTotal / stats.gridSliceCount;
                s.append(String.format("   %7.3f", average));
            } else {
                s.append(" -------");
            }
            s.append(String.format(" %10d\n", stats.gridSliceCount));
        }

        s.append("----------------------------------------------------------\n");

        return s.toString();
    }

    /**
     * Updates the statistics pertaining to request interpolation. This includes
     * the number of data slices sent, the number of data slices interpolated,
     * and the number of requests (total, good, and bad).
     * 
     * 
     * It may be necessary to create a statistics structure if there isn't
     * already one for this parameter. Then it is added to the dictionary. The
     * interpRequest structure is used to determine the parameter name. The
     * statistics for that parameter name is obtained and updated.
     * 
     * @param interpRequest
     */
    private void updateRequestStatCounts(InterpRequest interpRequest) {

        // need to create a new entry?
        InterpStats stats = statistics.get(interpRequest.getParameterName());
        if (stats == null) {
            stats = new InterpStats();

            // add empty entry:
            statistics.put(interpRequest.getParameterName(), stats);
        }

        // gridSliceSent
        int dsLength = interpRequest.getDataSlices().size();
        stats.gridSliceSentTotal += dsLength;

        if (dsLength > stats.gridSliceSentMax) {
            stats.gridSliceSentMax = dsLength;
        }

        if (dsLength < stats.gridSliceSentMin) {
            stats.gridSliceSentMin = dsLength;
        }

        // data gridSlice interpolation counts
        dsLength = interpRequest.remainingSlicesToInterpolate();
        stats.gridSliceInterpTotal += dsLength;
        if (dsLength > stats.gridSliceInterpMax) {
            stats.gridSliceInterpMax = dsLength;
        }

        if (dsLength < stats.gridSliceInterpMin) {
            stats.gridSliceInterpMin = dsLength;
        }

        // number of requests
        stats.numberOfRequests++;
        if (dsLength > 0) {
            stats.numberOfGoodRequests++;
        } else {
            stats.numberOfBadRequests++;
        }

        return;
    }

    /**
     * Updates the timing statistics pertaining to request interpolation. This
     * is the time the request was queued.
     * 
     * Due to updateRequestStatCounts(), there should always be a statistics
     * entry when this routine is called.
     * 
     * @param interpRequest
     */
    private void updateRequestStatTimes(InterpRequest interpRequest) {
        InterpStats stats = statistics.get(interpRequest.getParameterName());
        if (stats == null) {
            // logBug << "No entry in statistics for parameter" << std::endl;
            return;
        }

        double qTime = interpRequest.timeOnQueue();

        stats.interpReqTimeTotal += qTime;

        if (qTime > stats.interpReqTimeMax) {
            stats.interpReqTimeMax = qTime;
        }
        // if (!stats.interpReqTimeMin || qTime < stats.interpReqTimeMin) ???
        if (qTime < stats.interpReqTimeMin) {
            stats.interpReqTimeMin = qTime;
        }

        return;
    }

    /**
     * Updates the timing statistics about how long to interpolate.
     * 
     * Due to updateRequestStatCounts(), there should always be a statistics
     * entry when this routine is called.
     * 
     * @param interpRequest
     */
    private void updateInterpStatTimes(InterpRequest interpRequest) {
        InterpStats stats = statistics.get(interpRequest.getParameterName());

        if (stats == null) {
            Activator
                    .getDefault()
                    .getLog()
                    .log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "No entry in statistics for parameter"));
            return;
        }

        stats.gridSliceCount++;

        // how long it took to interpolate
        double pTime = gridSliceTime / 1000.0;

        stats.gridSliceIntTimeTotal += pTime;

        if (pTime > stats.gridSliceIntTimeMax) {
            stats.gridSliceIntTimeMax = pTime;
        }
        if (pTime < stats.gridSliceIntTimeMin) {
            stats.gridSliceIntTimeMin = pTime;
        }

        return;
    }

}
