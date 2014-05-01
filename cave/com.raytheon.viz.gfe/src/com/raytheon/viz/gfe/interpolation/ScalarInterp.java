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

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.time.TimeRange;

/**
 * <pre>
 *  Class used to create the BaseInfo grid used to control interpolation
 *  for a request, and with the function interpolate() used to interpolate
 *  a single grid of scalar values.
 * 
 *  -- implementation ---------------------------------------------------------
 *  Construction does all the work preceding interpolation. It includes these
 *  8 steps:
 *  1 implicit constructor here (before any executable statements)
 *  2 implicit constructor in ContInterp (before any executable statements)
 *  3 implicit constructor in Interp (before any executable statements)
 *  4 &quot;initializeKnownTimeValues()&quot; in Interp cstr which loads the Interp
 *  data &quot;_knownTimeValues&quot; accessed by SeqOf&lt;AbsTime&gt; Interp::known-
 *  TimeValues().
 *  5 allocateBaseInfoGrid() in ContInterp class which allocates memory space
 *  6 call initializeBaseInfoGrid() in this class which loads the local data
 *  &quot;_baseInfo.knownDataValues[]&quot;. There are &quot;numberOfBaseSlices()&quot; of them.
 *  7 6 calls &quot;initializeSplineCoeff()&quot; from ContInterp, which computes the
 *  spline coeffs of a spline function fitted to the &quot;knowDataValues&quot;
 *  at times given by  &quot;knowTimeValues()&quot;
 *  8 7 calls findSplineCoeff(), defined in ContInterp. This is where half
 *  of the mathematical algorithm for interpolation of real numbers resides.
 * 
 *  &quot;interpolate()&quot; defined here calls &quot;interpolateGrid()&quot; defined in ContInterp
 *  which calls &quot;evalSplineFunc()&quot; of the same class for each point on the
 *  grid. That is where the remainder of the math resides.
 * </pre>
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

public class ScalarInterp extends ContInterp {
    // (BaseInfo is defined in ContInterp.H)
    // grid of data to control interpolation:
    private Grid2D<BaseInfo> _baseInfo;

    /**
     * Constructor for scalar interp class taking a parameterName and a sequence
     * of data slices. The sequence of baseDataIndexes contains the indexes that
     * contain the base data.
     * 
     * Passes up the parameterName, data, and baseDataIndexes through the member
     * initialization list to the ContInterp() constructor. The important
     * structure _baseInfo is implicitly made here; it is empty at first. This
     * is where "baseDataIndexes" first appears and is created. see call to this
     * cstr in Interpolator::createInterpClass() - it uses
     * InterpRequest::validDataIndexes to make baseDataIndexes. Then allocates
     * the memory for the _baseInfo, and then initializes it.
     * 
     * @param dataslices
     * @param baseDataIndices
     * @param parmid
     * @param gridparminfo
     * @param gridTimes
     */
    public ScalarInterp(List<IGridSlice> dataslices, int[] baseDataIndices,
            ParmID parmid, GridParmInfo gridparminfo, List<TimeRange> gridTimes) {
        super(dataslices, baseDataIndices, parmid, gridparminfo, gridTimes);

        _baseInfo = allocateBaseInfoGrid(); // function of ContInterp

        initializeBaseInfoGrid(); // function of this class
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.interpolation.Interp#dispose()
     */
    @Override
    public void dispose() {
        deallocateBaseInfoGrid(_baseInfo);

        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.interpolation.ContInterp#interpolate(int)
     */
    @Override
    public IGridSlice interpolate(int index) {
        // for testing list
        // logEvent << " all grid center times are " <<
        // knownTimeValues() << endl;
        // logEvent << " grid size is " << gridSize() << endl;
        // logEvent << " base data indices are " << baseDataIndices() << endl;

        // interpolate the grid
        Grid2DFloat newGrid = interpolateGrid(_baseInfo, index);

        // Finally make the new gridslice with the interpolated grid of data.
        // make the Grid Data History
        GridDataHistory gdh = new GridDataHistory(OriginType.TIME_INTERPOLATED,
                _parmid, _gridTimes.get(index));

        IGridSlice newGS = new ScalarGridSlice(_gridTimes.get(index),
                getGridParmInfo(), new GridDataHistory[] { gdh }, newGrid);

        // for testing only:
        // cout<<" scalarInterp: interpolated data gridSlice:"<<endl;
        // cout<<ds<<endl;

        return newGS;
    }

    /**
     * Routine that initializes the base info grid based on the data slices
     * passed in through the constructor. This function assumes that
     * allocateBaseInfoGrid() has already been called to allocate the
     * appropriate memory.
     * 
     */
    private void initializeBaseInfoGrid() {
        int i, j, t;

        // logEvent << " call initializeBaseInfoGrid for " <<
        // numberOfBaseSlices() << " base data slices"<<endl;

        // for each base data gridSlice
        for (t = 0; t < getNumberOfBaseSlices(); t++) {
            // get the grid from the data gridSlice
            ScalarGridSlice slice = (ScalarGridSlice) (getGridSlice(getBaseDataIndices()[t]));
            Grid2DFloat grid = slice.getScalarGrid();

            // for each point on the grid, load its value into knownDataValues
            for (i = 0; i < grid.getXdim(); i++) {
                for (j = 0; j < grid.getYdim(); j++) {
                    _baseInfo.get(i, j).knownDataValues[t] = grid.get(i, j);

                    // if (i==1 && j == 1)
                    // logEvent << " known base value at point 1,1 is " <<
                    // _baseInfo(i,j).knownDataValues[t] << endl;
                }
            }
        }

        // using the ContInterp function here, calculate the coefficients of
        // the spline function fitting thru all data points (at all times
        // with defined or base data); one spline function and set
        // of coefficients for each grid point
        initializeSplineCoeff(_baseInfo);

        return;
    }
}
