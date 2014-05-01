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
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Class with the function interpolate() used to interpolate a single grid of
 * scalar values, with advection automatically imposed. This will work for any
 * grid of scalar variables where 0.0 is the normal background value, meaning
 * "no data of interest", and areas of non-zero values of interest change size
 * and shape and move around. Advection is the moving-around part. This will
 * work, for example, for QPF, and for any other weather scalar variables which
 * tend to appear and move around in bounded areas of value > 0.0, surrounded by
 * values of exactly 0.0, such as PoP, visibility, and sky coverage.
 * 
 * Limitation of this particular code: all values > 0.0 are treated as one
 * "area". IF the grid is covered with spots of non-zero data, they will all be
 * lumped together for purposes of finding the center of the area and the motion
 * of the area. This will not make satisfactory interpolated grids if one part
 * of the known grids has lots of non-moving or slowly moving data,a nd another
 * part has an area of data rapidly moving. This is not expected to be an actual
 * or common problem for the data types mentioned on the 740-km scale we are
 * using; the weather values mentioned usually move along pretty much together.
 * 
 * The advection feature only works for data which has no values below zero, and
 * zero playing the role of "nothing present", as in QPF, visibility, PoP,and
 * sky coverage.
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

public class ScalarAdvectInterp extends ContInterp {

    /**
     * <pre>
     * Constructor for scalar interp advection taking a parameterName 
     * and a sequence of data slices. 
     * The sequence of baseDataIndexes contains the indexes that point to data
     * slices that contain the base or previously-known data.
     * 
     * Construction does work preceding interpolation. It includes these
     * steps:
     * 1 implicit constructor here (before any executable statements)
     * 2 implicit constructor in ContInterp (before any executable statements)
     * 3 implicit constructor in Interp (before any executable statements)
     * 
     * Passes up the parameterName, data, and baseDataIndexes through the 
     * member initialization list to the ContInterp() constructor.
     * This is where &quot;;baseDataIndices&quot; first appears and is created.
     * </pre>
     * 
     * @param dataslices
     * @param baseDataIndices
     * @param parmid
     * @param gridparminfo
     * @param gridTimes
     */
    public ScalarAdvectInterp(List<IGridSlice> dataslices,
            int[] baseDataIndices, ParmID parmid, GridParmInfo gridparminfo,
            List<TimeRange> gridTimes) {
        super(dataslices, baseDataIndices, parmid, gridparminfo, gridTimes);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.interpolation.ContInterp#interpolate(int)
     */
    @Override
    public IGridSlice interpolate(int index) {
        // Determine fractional "distance" interpolated grid is from first grid
        // to last grid.
        float zeta = findTimeFraction(index);

        // interpolate (make) the new grid
        Grid2DFloat newGrid = interpolateWithAdvection(index, zeta);

        // Finally make the new gridslice.
        // make the Grid Data History
        GridDataHistory gdh = new GridDataHistory(OriginType.TIME_INTERPOLATED,
                _parmid, _gridTimes.get(index));

        IGridSlice newGS = new ScalarGridSlice(_gridTimes.get(index),
                gridParmInfo, new GridDataHistory[] { gdh }, newGrid);

        // for testing only:
        // cout<<" scalarInterp: interpolated data gridSlice:"<<endl;
        // cout<<ds<<endl;

        return newGS;
    }

    /**
     * Create a new grid of values from two known grids which precede and follow
     * it.
     * 
     * Earlier code guarantees that base data will bracket data gridSlice to
     * interpolate, so first searches should always succeed.
     * 
     * Based on assumption that some of the grid is of "background" value 0.0,
     * representing the case of "none present", such as 0.00 QPF. The non-zero
     * values will be interpolated with advection, that is, bounded areas moved
     * around as units, changing both size shape, and values at each pint.
     * 
     * Zeta is fractional distance from first known grid before the interpolated
     * grid to the known grid afterwards. Ranges from 0.0 at first known grid to
     * 1.0 at last known grid. Really only 0.0<zeta<1.0 correct.
     * 
     * @param index
     * @param zeta
     * @return
     */
    private Grid2DFloat interpolateWithAdvection(int index, float zeta) {
        int t;

        Grid2DFloat lastGrid = null;
        Grid2DFloat firstGrid = null;
        Grid2DFloat result;

        // find known or base data grid just AFTER grid to be interpolated
        for (t = 0; t < getNumberOfBaseSlices(); t++) {
            if (getBaseDataIndices()[t] > index) {
                // get the Grid2DFloat from the data gridSlice
                ScalarGridSlice slice = (ScalarGridSlice) getGridSlice(getBaseDataIndices()[t]);
                lastGrid = slice.getScalarGrid();
                break;
            }
        }

        // find known or base data grid just BEFORE grid to be interpolated
        for (t = getNumberOfBaseSlices() - 1; t >= 0; t--) {
            if (getBaseDataIndices()[t] < index) {
                // get the Grid2DFloat from the data gridSlice
                ScalarGridSlice slice = (ScalarGridSlice) getGridSlice(getBaseDataIndices()[t]);
                firstGrid = slice.getScalarGrid();
                break;
            }
        }

        // call the ContInterp function to make an interpolated grid
        // using automatic advection.
        result = autoScalarAdvection(firstGrid, lastGrid, zeta);

        // cout<<" Interpolated grid"<<endl;
        // cout<< result<<endl;

        return result;
    }
}
