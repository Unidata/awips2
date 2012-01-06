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
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;

/**
 * The InterpRequest (Interpolation Request) class contains all of the
 * information necessary to process an interpolation request. Information
 * pertaining to the parameter name, the time span of the data, and the list of
 * data grids are contained in this class, and other infor derived from them
 * used to control interpolation.
 * 
 * "base" data is a GridSlice with already defined data which controls
 * interpolation.
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

public class InterpRequest {

    private final ParmID parmID;

    private final GridParmInfo gridParmInfo;

    private List<IGridSlice> dataSlices;

    private List<TimeRange> requestTimes;

    private String parameterName;

    private TimeRange timeSpan;

    private int slicesToInterpolate;

    private boolean[] interpolationNeeded;

    long startTime;

    private boolean advected;

    /**
     * for an Interpolation Request taking a ParmID, the GridParmInfo, and a ref
     * to sequence of data slices that are properly defined (we hope) for the
     * interpolation. requestTimes are the TimeRanges for the gridSlices in
     * dataSlices. Checks if the request is valid, which function also sets the
     * indexes of the base data set to be used for interpolation, and other
     * member data.
     * 
     * Note numberOfSlicesToInterpolate is first set to zero and that the
     * interpolationNeeded is set to the proper length. Start timer. Sets proper
     * values of numberOfSlicesToInterpolate, and interpolationNeeded in
     * function validateRequest().
     * 
     * @param parmid
     * @param gridparminfo
     * @param dataslices
     * @param requesttimes
     * @param advection
     */
    public InterpRequest(ParmID parmid, GridParmInfo gridparminfo,
            LinkedList<IGridSlice> dataslices,
            LinkedList<TimeRange> requesttimes, boolean advection) {
        this.parmID = parmid;
        this.gridParmInfo = gridparminfo;
        this.dataSlices = dataslices;
        this.requestTimes = requesttimes;
        this.advected = advection;

        this.parameterName = parmID.getParmName();
        this.interpolationNeeded = new boolean[dataslices.size()];
        this.slicesToInterpolate = 0;

        this.startTime = System.currentTimeMillis();

        this.timeSpan = new TimeRange(requestTimes.get(0).getStart(),
                requestTimes.get(requestTimes.size() - 1).getEnd());

        // check request, and set true values for member data
        if (!isValid()) {
            Activator.getDefault().getLog().log(
                    new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "Invalid request submitted "));
            // print out submitted request?
        }
    }

    /**
     * Used to make the "baseDataIndices" variable used by Interp classes.
     * Returns a list of indices that contain "base" data slices. The gridslice
     * array has both base data and the to-be-interpolated data.
     * 
     * This function MUST be called BEFORE any interpolation takes place in
     * order to know the base data indices.
     * 
     * If called after some interpolation, this function will count some
     * interpolated data as base data. No harm done but not much point.
     * 
     * Goes through the sequence of data slices looking for non-NONE data
     * indices and puts those indices in a sequence.
     * 
     * @return
     */
    public int[] baseDataIndices() {
        List<Integer> indices = new ArrayList<Integer>();

        for (int i = 0; i < dataSlices.size(); i++) {
            if (dataSlices.get(i) != null) {
                indices.add(i);
            }
        }

        int[] result = new int[indices.size()];
        int i = 0;
        for (Integer index : indices) {
            result[i++] = index;
        }
        return result;
    }

    /**
     * Routine to return a TimeRange that encompasses all of the NONE-type data
     * slices. Note that the returned value will change as the request is
     * processed.
     * 
     * Goes through each data gridSlice and gets its type. Finds the first and last
     * none-type data gridSlice and combines those times using the time range span
     * command. depends on the arrays of dataSlices and requestTimes being
     * exactly parallel, since null grids have no time range.
     * 
     * @return
     */
    public TimeRange interpolateSliceSpan() {
        int i;
        TimeRange dataSpan = null;

        // find the first one that is null
        for (i = 0; i < dataSlices.size(); i++) {
            if (dataSlices.get(i) == null) {
                dataSpan = requestTimes.get(i);
                break;
            }
        }

        // find the last one that is null
        for (i = dataSlices.size() - 1; i >= 0; i--) {
            if (dataSlices.get(i) == null) {
                dataSpan = dataSpan.span(requestTimes.get(i));
                break;
            }
        }

        // return the result
        return dataSpan;
    }

    /**
     * Returns how many remaining data slices to interpolate.
     * 
     * @return
     */
    public int remainingSlicesToInterpolate() {
        int count = 0;

        for (int i = 0; i < interpolationNeeded.length; i++) {
            if (interpolationNeeded[i]) {
                count++;
            }
        }

        return count;
    }

    /**
     * Returns the length of time that this interpolation request has existed in
     * seconds.
     * 
     * @return
     */
    public double timeOnQueue() {
        return (System.currentTimeMillis() - this.startTime) / 1000.0;
    }

    /**
     * Returns the index of the next data gridSlice to be interpolated. The data can
     * then be retrieved using the Interp classes dataSlice(index) function.
     * This call should never return the same index. Returns -1 if there are no
     * more entries to interpolate.
     * 
     * Goes through the _interpolationNeeded bit array to find the first item
     * set. Then clears that item and returns the index.
     * 
     * @return
     */
    public int getNextInterpolationIndex() {
        for (int i = 0; i < interpolationNeeded.length; i++) {
            if (interpolationNeeded[i]) {
                interpolationNeeded[i] = false;
                return i;
            }
        }

        return -1;
    }

    /**
     * Private function to validate the interpolation request. Checks to see
     * that there is at least one data gridSlice that can be interpolated. Sets the
     * interpolationNeeded flags. Ensures that the data slices are of the same
     * type. Assumes that the private data has already been set up (e.g.,
     * numberOfSlicesToInterpolate is zero, and the bitArray is the same length
     * as the dataSlices array). If the request is valid, then
     * numberOfSlicesToInterpolate will be non-zero after this function returns.
     * 
     * Goes through each data gridSlice and gets its type. Keeps track of the basic
     * type. Figures out the base values.
     * 
     * @return true if request is valid
     */
    private boolean isValid() {
        int i;

        // if there are no data slices to interpolate fail
        if (dataSlices.size() == 0) {
            Activator.getDefault().getLog().log(
                    new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "No grid slices in the interp request"));
            return false;
        }

        // first and last gridSlice must contain data
        if (dataSlices.get(0) == null
                || dataSlices.get(dataSlices.size() - 1) == null) {
            Activator
                    .getDefault()
                    .getLog()
                    .log(
                            new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                                    "First or last grid lacks data for interpolation."));
            return false;
        }

        // set value(s) in the interpolationNeeded bit array,
        // and count of slicesToInterpolate.
        for (i = 1; i < dataSlices.size() - 1; i++) {
            if (dataSlices.get(i) == null) {
                slicesToInterpolate++;
                interpolationNeeded[i] = true;
            } else if (dataSlices.get(i).getGridInfo().getGridType() != gridParmInfo
                    .getGridType()) {
                Activator.getDefault().getLog().log(
                        new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                                "Base grids not all the same data type! "));
                return false;
            }
        }

        // if found no data to interpolate
        if (slicesToInterpolate == 0) {
            Activator.getDefault().getLog().log(
                    new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "No dataslices to interpolate in request"));
            return false;
        }
        return true;
    }

    public ParmID getParmID() {
        return parmID;
    }

    public GridParmInfo getGridParmInfo() {
        return gridParmInfo;
    }

    public List<IGridSlice> getDataSlices() {
        return dataSlices;
    }

    public List<TimeRange> getRequestTimes() {
        return requestTimes;
    }

    public String getParameterName() {
        return parameterName;
    }

    public TimeRange getTimeSpan() {
        return timeSpan;
    }

    public int getSlicesToInterpolate() {
        return slicesToInterpolate;
    }

    public boolean isAdvected() {
        return advected;
    }

    @Override
    public String toString() {
        String s = "--------- Interpolation Request ------------\n";
        s += "Number Of Slices To Interpolate: " + slicesToInterpolate
                + "\n Parameter: " + parameterName;
        s += "\n Interpolate Needed Flags: " + interpolationNeeded;
        s += "\n Time On Queue: " + timeOnQueue() + " seconds";
        s += "\n Parm Info: " + gridParmInfo;
        s += "\n Data Slices: " + dataSlices;
        s += "\n---------- end Interpolation Request --------";

        return s;
    }
}
