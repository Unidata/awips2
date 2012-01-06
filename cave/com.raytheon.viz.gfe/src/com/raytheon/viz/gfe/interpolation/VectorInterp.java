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

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;

/**
 * Class used to create the BaseInfo grids used to control interpolation for a
 * request. Provides the function interpolate() used to interpolate vector
 * magnitude and direction grids for one time, and make into a new vector type
 * gridSlice.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 2, 2008		#1161	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class VectorInterp extends ContInterp {

    private Grid2D<BaseInfo> _magBaseInfo;

    private Grid2D<BaseInfo> _dirBaseInfo;

    /**
     * Constructor for vector interp class taking a parameterName and a sequence
     * of data slices. The array "baseDataIndexes" contains the indexes to the
     * data slices in the request that contain the "base" data.
     * 
     * Passes up the parameterName, data, and baseDataIndexes through the member
     * initialization list to the ContInterp() constructor. The important
     * structures _magBaseInfo and _dirBaseInfo are implicitly made here; they
     * are empty at first. This is where "baseDataIndexes" first appears and is
     * created. see call to this cstr in Interpolator::createInterpClass() - it
     * uses InterpRequest::validDataIndexes to make baseDataIndexes. Then
     * allocates the memory for the BaseInfo types, and then initializes them
     * (puts in data).
     * 
     * @param dataslices
     * @param baseDataIndices
     * @param parmid
     * @param gridparminfo
     * @param gridTimes
     */
    public VectorInterp(List<IGridSlice> dataslices, int[] baseDataIndices,
            ParmID parmid, GridParmInfo gridparminfo, List<TimeRange> gridTimes) {
        super(dataslices, baseDataIndices, parmid, gridparminfo, gridTimes);

        _magBaseInfo = allocateBaseInfoGrid(); // function of ContInterp
        _dirBaseInfo = allocateBaseInfoGrid(); // function of ContInterp
        initializeBaseInfoGrid(); // function of this class
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.interpolation.Interp#dispose()
     */
    @Override
    public void dispose() {
        deallocateBaseInfoGrid(_magBaseInfo);
        deallocateBaseInfoGrid(_dirBaseInfo);

        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.interpolation.ContInterp#interpolate(int)
     */
    @Override
    public IGridSlice interpolate(int index) {
        // Interpolate the grid magnitude and direction grids separately.
        // Uses the ContInterp function interpolateGrid() for magnitude,
        // and a member function of this class for direction.

        // interpolate wind speed as a scalar
        Grid2DFloat magGrid = interpolateGrid(_magBaseInfo, index);

        // perform limit checks - speed must be >= 0.0 at all grid points
        for (int i = 0; i < magGrid.getXdim(); i++) {
            for (int j = 0; j < magGrid.getYdim(); j++) {
                if (magGrid.get(i, j) < 0.0f) {
                    magGrid.set(i, j, 0.0f);
                }
            }
        }

        // interpolate wind direction with this class's function
        Grid2DFloat dirGrid = interpolateDirection(_dirBaseInfo, _magBaseInfo,
                index);

        // Finally make the new gridSlice.
        // make the Grid Data History
        GridDataHistory gdh = new GridDataHistory(OriginType.TIME_INTERPOLATED,
                _parmid, _gridTimes.get(index));

        // use GridSlice vector constructor
        IGridSlice newGS = new VectorGridSlice(_gridTimes.get(index),
                gridParmInfo, new GridDataHistory[] { gdh }, magGrid, dirGrid);

        return newGS;
    }

    /**
     * Routine that initializes the BaseInfo magnitude and direction grids based
     * on the data slices passed in through the constructor (load known data
     * values into the BaseInfo grids). This function assumes that
     * allocateBaseInfoGrid() has already been called to allocate the
     * appropriate memory.
     * 
     */
    private void initializeBaseInfoGrid() {
        // for each base data gridSlice
        for (int t = 0; t < getNumberOfBaseSlices(); t++) {
            // get the mag and direction grids from the data gridSlice
            VectorGridSlice slice = (VectorGridSlice) getGridSlice(getBaseDataIndices()[t]);
            final Grid2DFloat magGrid = slice.getMagGrid();
            final Grid2DFloat dirGrid = slice.getDirGrid();

            // for each point on the grid, load its values into knownDataValues
            for (int i = 0; i < magGrid.getXdim(); i++) {
                for (int j = 0; j < magGrid.getYdim(); j++) {
                    _magBaseInfo.get(i, j).knownDataValues[t] = magGrid.get(i,
                            j);
                    _dirBaseInfo.get(i, j).knownDataValues[t] = dirGrid.get(i,
                            j);
                }
            }
        }

        // using the ContInterp function initializeSplineCoeff(),
        // calculate the coefficients of
        // a spline function fitting magnitude at all data points (at all times
        // with defined or base data); there is one spline function and set
        // of coefficients for each grid point.
        initializeSplineCoeff(_magBaseInfo);
    }

    /**
     * Routine that interpolates direction given a sequence of directions. Input
     * is the grid of BaseInfo data, and an index to a data gridSlice in the request
     * where interpolation is needed.
     * 
     * The most important thing about interpolating direction is not the
     * particular technique to give the most precise result of interpolation,
     * but insuring that the interpolation does not give an utterly wrong value
     * because the shift in direction crosses the 0 to 360 degree boundary or
     * discontinuity (the "North problem"). For example, interpolating midway
     * between the known values 355 and 15 degrees should give us 5 degrees, not
     * 185 degrees. We always take the smaller of the two possible choices in
     * the change of wind direction. It may rarely be wrong, but you can't tell,
     * and this assumption gives the proper results most of the time.
     * 
     * This function has a very simple linear technique using only the two known
     * directions preceding and following the time of interest. Most of the code
     * handles the North problem and related 360 and 180 degrees questions.
     * 
     * One could use a spline function fitted to several known directions. The
     * only advantage would be more smooth interpolated function of directions
     * Whether this more smooth direction has any value is an important
     * question. There is no assurance that the wind shifts smoothly like a
     * spline function, nor do we use this precision in weather forecasting
     * where often wind directions are given to the nearest one-eight, or even
     * one- quarter, of the circle. There is greater difficulty in using
     * splines: First the known direction values would have to be checked for
     * the "North problem" and adjusted accordingly: all values after the first
     * might have to have +/- 360 or +/- 180 added to them before they could
     * proceed. The interpolated result would fit into this new function. It
     * might be off by 180 degrees from neighboring grid points in some special
     * circumstances.
     * 
     * So we will start by using this simple linear technique and see how well
     * it works. In particular we must look for consistent results between
     * neighbor grid points, and as compared to directions before and after in
     * time.
     * 
     * All that we need to know to make the computation is three real numbers:
     * 1. the known directions before and after the interp time, and 2. the
     * fraction in time that the interp time lies from "before" to "after"
     * times. Finding the time fraction takes some doing.
     * 
     * Special case: if one base grid point's wind has both zero speed and
     * direction, we assume this means calm winds. In that case we assume that
     * the interpolation should reduce the other base wind's speed gradually to
     * zero, with no change in direction.
     * 
     * @param baseDirInfo
     * @param baseMagInfo
     * @param index
     * @return
     */
    private Grid2DFloat interpolateDirection(
            final Grid2D<BaseInfo> baseDirInfo,
            final Grid2D<BaseInfo> baseMagInfo, int index) {
        // create an empty output grid
        Grid2DFloat outGrid = new Grid2DFloat(baseDirInfo.getXDim(),
                baseDirInfo.getYDim());

        // find the time fraction: how far this interpolation step is from
        // the starting base or known grid to the end bas grid, on scale 0.0 to
        // 1.0
        float fraction = findTimeFraction(index);

        // Get the data gridSlice with the given "index",
        // the one needing interpolation.
        // GridSlice &ds = gridSlice(index);

        // make its interp time (mid point time)
        Date interpTime = _gridTimes.get(index).getCenterTime();

        // indexes for known data before and after interpTime
        int bdIndexBefore = 0, bdIndexAfter = 0;

        // Look through all the data slices to find the two bracketing interp
        // times
        for (int bdCount = 1; bdCount < getNumberOfBaseSlices(); bdCount++) {
            if ((getKnownTimeValues()[getBaseDataIndices()[bdCount - 1]]
                    .compareTo(interpTime) < 0)
                    && (getKnownTimeValues()[getBaseDataIndices()[bdCount]]
                            .compareTo(interpTime)) >= 0) {
                // Have found the pair of base or known data slices bracketing
                // the
                // one to interpolate.
                bdIndexBefore = bdCount - 1;
                bdIndexAfter = bdCount;
                break;
            }
        }

        float delta; // full change in wind direction at a point
        float newdir; // the interpolated direction made here

        // Now we can interpolate, and handle the possible North problem,
        // for every point on the grid.
        for (int i = 0; i < baseDirInfo.getXDim(); i++) {
            for (int j = 0; j < baseDirInfo.getYDim(); j++) {
                // Check for calm winds in either base grid point:
                // if the wind is calm at either base point, use unchanged
                // direction of other base point's direction.
                if ((baseDirInfo.get(i, j).knownDataValues[bdIndexBefore] == 0.0f && baseMagInfo
                        .get(i, j).knownDataValues[bdIndexBefore] == 0.0f)) {
                    newdir = baseDirInfo.get(i, j).knownDataValues[bdIndexAfter];
                } else if ((baseDirInfo.get(i, j).knownDataValues[bdIndexAfter] == 0.0f && baseMagInfo
                        .get(i, j).knownDataValues[bdIndexAfter] == 0.0f)) {
                    newdir = baseDirInfo.get(i, j).knownDataValues[bdIndexBefore];
                } else {
                    float angle1 = baseDirInfo.get(i, j).knownDataValues[bdIndexBefore];
                    float angle2 = baseDirInfo.get(i, j).knownDataValues[bdIndexAfter];

                    // If outside 0-360 limits, something has gone wrong,
                    // and the "angle" is probably meaningless. Reducing it
                    // to the 0-360 range won't give a direction related
                    // to the wind field either. Replace it with a near good
                    // direction.

                    if (angle1 > 360.0 || angle1 < 0.0) {
                        Activator
                                .getDefault()
                                .getLog()
                                .log(
                                        new Status(
                                                IStatus.ERROR,
                                                Activator.PLUGIN_ID,
                                                "input angle1 "
                                                        + angle1
                                                        + " outside valid range 0-360"));

                        // use already-determined neighbor point's value
                        if (i - 1 >= 0) {
                            outGrid.set(i, j, outGrid.get(i - 1, j));
                            continue;
                        } else if (j - 1 >= 0) {
                            outGrid.set(i, j, outGrid.get(i, j - 1));
                            continue;
                        } else {
                            // case of at point (0,0); with no guidance to help
                            outGrid.set(i, j, 0.0f);
                            continue;
                        }
                    }

                    if (angle2 > 360.0 || angle2 < 0.0) {
                        Activator
                                .getDefault()
                                .getLog()
                                .log(
                                        new Status(
                                                IStatus.ERROR,
                                                Activator.PLUGIN_ID,
                                                "input angle2 "
                                                        + angle2
                                                        + " outside valid range 0-360"));

                        // use neighbor point's value
                        if (i - 1 >= 0) {
                            outGrid.set(i, j, outGrid.get(i - 1, j));
                            continue;
                        } else if (j - 1 >= 0) {
                            outGrid.set(i, j, outGrid.get(i, j - 1));
                            continue;
                        } else {
                            // case of at point (0,0); with no guidance to help
                            outGrid.set(i, j, 0.0f);
                            continue;
                        }
                    }

                    // find change in direction between the two known values
                    // (this will be in range -360 < delta < 360.)
                    delta = angle2 - angle1;

                    // Change of direction can go in two ways; either
                    // is potentially possible. We don't know which is "real."
                    // Select the smaller of the two possible changes (<=180):
                    // we assume |dir| will be < 180 degrees per time step.
                    // (If it's larger the forecaster ought to be
                    // specifying them!)
                    if (delta > 180.0) {
                        delta -= 360.0;
                    }

                    if (delta < -180.0) {
                        delta += 360.0;
                    }

                    // compute interpolated direction by linear change in angle
                    newdir = delta * fraction + angle1;

                    // so far, newdir will be inside range -540 to 540.

                    // convert newdir to equivalent direction in range 0 to 360,
                    // if needed.
                    if (newdir > 360.00) {
                        newdir -= 360;
                    }

                    if (newdir < 0.0) {
                        newdir += 360.0;
                    }
                }
                // put interpolated direction in grid
                outGrid.set(i, j, newdir);
            }
        }

        return outGrid;
    }
}
