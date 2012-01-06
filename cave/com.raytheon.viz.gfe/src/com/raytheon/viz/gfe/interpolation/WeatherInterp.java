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

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DInteger;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Derived from the Interp ABC. Computes new grids of &quot;weather&quot; given
 * two known (&quot;base&quot;) grids of weather before and after time when
 * interpolation is desired.
 * <p>
 * Works by moving each area of each weather type from the first known grid to
 * the last grid, along a line connecting the center points of the areas on the
 * first and last grids, and taking into account changing shape of weather area,
 * and degree to which it adjoins or is embedded in another area of differing
 * type.
 * <p>
 * Discrete interpolation deals with areas, each of a single weather type, and
 * with discrete boundary. This is exactly the opposite of interpolation of a
 * field of continuous scalars. The weather areas appear to move from one place
 * to another, and to change shape.
 * <p>
 * The components of successful weather interpolation are: determining motion or
 * path from first to final known area; (here done by determining a
 * &quot;center&quot; for each known area, with simple linearly interpolated
 * intermediate positions of centers of areas); and change of shape as each area
 * moves to intermediate postions. Note the motion may be zero allowing for just
 * change in shape. This is all you need if there is only one weather
 * description.
 * <p>
 * If there is more than one weather description, you also need: 1. recognizing
 * differing wx areas which share a contiguous boundary in both first and final
 * known grids, and keeping that boundary contiguous in interpolated areas,
 * without gaps or overlaps appearing; 2. recognizing entirely embedded wxs, one
 * inside others, and maintaining that relation at all interpolated areas
 * without gaps or overlaps; 3. allowing overlaps between areas which dont touch
 * in first and final grid. (allowing overlaps of related crossing weather areas
 * is a arbitrary choice).
 * <p>
 * private data includes _wxTypeList1 and _wxTypeList2: these are info objects
 * about &quot;weather&quot; on the first and last grids: see GridWxInfo.H file
 * for the structure.
 * <p>
 * Array containing control info for _growArea growth: float _gaControl[N][2]; N
 * = number of sectors in a circle = _numSectors _gaControl[i][0] = min distance
 * in sector i to edge of grow area; _gaControl[i][1] = max distance to edge of
 * grow area in same sector i. similar for _fadeArea and _faControl.
 * <p>
 * _allKeys a list of all WeatherKeys found in two base or input grids
 * <p>
 * In AFPS &quot;weather data&quot; for one &quot;GridSlice&quot; contains a
 * grid of bytes, and a list (List) of WeatherSubkeys. The values in the grid
 * (bytes) are indices into the subkey list. So each point in the AFPS grid can
 * have one weather descriptor associated with it. In AFPS the items in the byte
 * grid have no name, but the items in the list, or the list itself, is called a
 * &quot;WeatherKey.&quot;
 * <p>
 * This interpolation scheme works entirely on grids of integers, which are
 * based on the byte grid of indices to the key.
 * <p>
 * Interpolation could work directly on the byte grid (swapping some ints to
 * bytes), but for the fact that every weather data gridSlice has in principle a
 * DIFFERENT list of keys. So two grids representing IDENTICAL weather may have
 * DIFFERENT byte grids (keys). This is to preserve some programming
 * flexibility(!) but requires the confusion to be unscrambled here before
 * interpolation can take place. Working grids based on the input byte grids are
 * constructed, each referring to a new list of weatherkey descriptors called
 * &quot;_allKeys.&quot; With this consistent set of indices interpolation can
 * proceed.
 * <p>
 * The sequence &quot;baseDataIndices&quot; contains the indices to the
 * GridSlices in the Interp &quot;_data&quot; that contain the base (known or
 * input) data, unchanging, which controls interpolation. The sequence of data
 * slices should contain a mix of &quot;NONE-type&quot; and valid GridSlices.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class WeatherInterp extends Interp {

    private static final int NUM_SECTORS = 32;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherInterp.class);

    private int _xDim, _yDim;

    // information about all weather types in beginning input grid
    private List<GridWxInfo> _wxTypeList1;

    // information about all weather types in final input grid
    private List<GridWxInfo> _wxTypeList2;

    // grids to show where moving feature stars and ends in the grids.
    private Grid2DInteger _featureStart;

    private Grid2DInteger _featureEnd;

    // temporary working grids to hold
    // the three components of a single weather area:
    private Grid2DInteger _coreArea;

    private Grid2DInteger _growArea;

    private Grid2DInteger _fadeArea;

    // displacement vector components
    private int _dx, _dy;

    // x,y positions of center of original and final area
    private Point center1, center2;

    // sector controls for growArea and fadeArea
    private Grid2DFloat _gaControl;

    private Grid2DFloat _faControl;

    private Grid2DInteger _wxTypeInfoMatrix;

    // a list of all-weather-descrips needed for two base grids
    private List<WeatherKey> _allKeys;

    @SuppressWarnings("unused")
    public WeatherInterp(List<IGridSlice> dataslices, int[] baseDataIndices,
            ParmID parmid, GridParmInfo gridparminfo, List<TimeRange> gridTimes) {
        super(dataslices, baseDataIndices, parmid, gridparminfo, gridTimes);

        _allKeys = new ArrayList<WeatherKey>();
        _wxTypeList1 = new ArrayList<GridWxInfo>();
        _wxTypeList2 = new ArrayList<GridWxInfo>();

        // get size of our grid
        Point size = gridParmInfo.getGridLoc().gridSize();
        _xDim = size.x;
        _yDim = size.y;

        // for one single weather descriptor:

        // these grids flag points where a certain weather descriptor is found,
        // in the "base" or known first and final input weather grids.
        _featureStart = new Grid2DInteger(size.x, size.y);
        _featureEnd = new Grid2DInteger(size.x, size.y);

        // these grids show points in the weather area, as divided into
        // three components: the core (common to first and final grid, after
        // motion is accounted for), the part that grows from the first grid
        // area
        // to make the final shape, and the part of the first grid area that
        // fades
        // away to achieve the final grid. Motion is not a factor here; all
        // centers lie at same point relative ot the core area.
        // Factor of two is necessary. It allows for weather area moving away
        // from or onto a grid edge, while growing or shrinking in size.
        _coreArea = new Grid2DInteger(size.x, size.y);
        _growArea = new Grid2DInteger(2 * size.x, 2 * size.y);
        _fadeArea = new Grid2DInteger(2 * size.x, 2 * size.y);

        // make sure _numSectors is > 0; if not, assign a reasonable value.
        // oops constant, can't do this.
        if (NUM_SECTORS < 8) {
            statusHandler.handle(Priority.PROBLEM, "Numsectors is too small: "
                    + NUM_SECTORS);
        }

        // Growth and fading of the area is controlled by independent sectors.
        // Second dimension (2) is not variable; holds upper and lower dist
        // limits.
        _gaControl = new Grid2DFloat(NUM_SECTORS, 2);
        _faControl = new Grid2DFloat(NUM_SECTORS, 2);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.interpolation.Interp#interpolate(int)
     */
    @Override
    public IGridSlice interpolate(int index) {
        // Given the desired grid to interpolate indicated by "index",
        // contrive two grids in which the same number stands for the same
        // kind of weather.
        Grid2DInteger workGrid1 = new Grid2DInteger(_xDim, _yDim);
        Grid2DInteger workGrid2 = new Grid2DInteger(_xDim, _yDim);

        // Determine time fraction at interpolation from first input
        // grid to second input grid, and some needed indexes.

        // get end times of that data gridSlice; make its interp time (mid point
        // time)
        Date interpTime = _gridTimes.get(index).getCenterTime();

        // Determine indexes and times for neighboring base data slices

        // indexes for known data before and after interpTime
        int before = 0, after = 0;
        int bdCount, i, j;

        // time Relative to first data gridSlice in the request, Integer type
        long ritime;
        // times of the known data slices before and after interpolation time
        double beforeTime = 0.0, afterTime = 0.0;

        // knownTimeValues exists for all gridslices in the request;
        // knownDataValues, part of the BaseInfo structure, exists only
        // for the slices with base data. Indexes to corresponding
        // times and values are different in these two arrays.

        // Look through all the base slices to find the two bracketing interp
        // time.
        // All times used are relative to the first data gridSlice in the
        // request
        // (an
        // arbitrary reference point)
        for (bdCount = 1; bdCount < getNumberOfBaseSlices(); bdCount++) {
            if ((getKnownTimeValues()[getBaseDataIndices()[bdCount - 1]]
                    .compareTo(interpTime) < 0)
                    && (getKnownTimeValues()[getBaseDataIndices()[bdCount]]
                            .compareTo(interpTime) >= 0)) {
                // indexes to the base data slices before and after interp time,
                // in the array of times of all the data slices,
                // and in the array of data slices.
                before = getBaseDataIndices()[bdCount - 1];
                after = getBaseDataIndices()[bdCount];

                // Use the END time of the preceding known data gridSlice, and
                // the
                // START time of the the following known data gridSlice. This
                // prevents
                // skewed interpolation results if the known data slices are not
                // of equal length.

                // use the end time of the preceding base gridslice
                Date knownDataTime = _gridTimes.get(before).getEnd();
                ritime = knownDataTime.getTime()
                        - getKnownTimeValues()[0].getTime();
                beforeTime = ritime;

                // use start time of the following known data gridSlice
                knownDataTime = _gridTimes.get(after).getStart();
                ritime = knownDataTime.getTime()
                        - getKnownTimeValues()[0].getTime();
                afterTime = ritime;

                break;
            }
        }

        // what if afterTime = beforeTime ?
        // Very unlikely. Should never have gotten this far!
        if (beforeTime == afterTime) {
            statusHandler.handle(Priority.PROBLEM,
                    "Two identical times for base gridslices in the request.");
            return null; // no interpolation done
        }

        // make a relative time of the interp time, double type
        ritime = interpTime.getTime() - getKnownTimeValues()[0].getTime();
        double itime = ritime;

        // now we can find the time fraction
        float fraction = (float) ((itime - beforeTime) / (afterTime - beforeTime));

        // get the base gridSlices used to control interpolation
        WeatherGridSlice baseData1 = (WeatherGridSlice) getGridSlice(before);
        WeatherGridSlice baseData2 = (WeatherGridSlice) getGridSlice(after);

        // set length of "_allKeys."
        _allKeys.clear();

        // Load the two interpolation control input grids, workGrid1 and
        // workGrid2, with consistent weather keys indices,
        // each index standing for the same "weather" in both grids.
        unscrambleWxTypes(baseData1, baseData2, workGrid1, workGrid2);

        // WorkGrid1 and 2 and _allKeys are now set.
        // Now we can do interpolation.

        // working grid to interpolate one weather value :
        Grid2DInteger newGrid = new Grid2DInteger(_xDim, _yDim);

        // working grids for interpolating a group of contiguous weather
        // descrips:
        Grid2DInteger groupGrid1 = new Grid2DInteger(_xDim, _yDim);
        Grid2DInteger groupGrid2 = new Grid2DInteger(_xDim, _yDim);

        // working grid for ALL interpolated weather descriptions:
        Grid2DInteger interpResult = new Grid2DInteger(_xDim, _yDim);

        // arrays to list weather descriptions, identified by number,
        // to interpolate jointly and singly
        List<Integer> joint = new ArrayList<Integer>();
        List<Integer> single = new ArrayList<Integer>();

        // Do local private functions to create information structures
        // about groupings of weather descriptions
        // which are used to control interpolation

        findWxTypes(workGrid1, workGrid2);

        composeWxTypeInfoMatrix();

        findWxTypeGroups(joint, single);

        // do joint interpolation if needed.
        if (joint.size() > 0 && joint.get(0) != 0 && joint.get(1) != 0) {
            jointInterpControl(joint, workGrid1, workGrid2, groupGrid1,
                    groupGrid2, newGrid, fraction, interpResult);
        }

        // if there are wx descriptions for independent interpolation,
        // do all of them singly (without reference to other weather descrip.),
        // and accumulate the results, making combinations if necessary
        if (single.size() > 0) {
            singleInterpControl(single, workGrid1, workGrid2, newGrid,
                    fraction, interpResult);
        }

        // test output only:
        // System.out.println("   interpResult grid at fraction=" + fraction);
        // System.out.println(interpResult);

        // Create the new GridSlice from the grid of interpolated indexes,
        // the associated List<WeatherKey> _allKeys, and the
        // (unchanged) time range.

        // copy int result grid to a byte grid
        Grid2DByte ptIndexes = new Grid2DByte(_xDim, _yDim);
        for (i = 0; i < _xDim; i++) {
            for (j = 0; j < _yDim; j++) {
                ptIndexes.set(i, j, (byte) interpResult.get(i, j));
            }
        }

        // make the Grid Data History
        GridDataHistory gdh = new GridDataHistory(OriginType.TIME_INTERPOLATED,
                _parmid, _gridTimes.get(index));

        IGridSlice newGS = new WeatherGridSlice(_gridTimes.get(index),
                gridParmInfo, new GridDataHistory[] { gdh }, ptIndexes,
                _allKeys.toArray(new WeatherKey[_allKeys.size()]));

        return newGS;
    }

    /**
     * Create list of all weather types in both input grids. Create two working
     * grids to control interpolation, based on the input grids.
     * <p>
     * Interpolation works on the grids of bytes or integers; so their values
     * must both consistently point to the same set of weather text descriptors.
     * That's why we can't use the input grids' byte Arrays directly.
     * 
     * @param baseData1
     * @param baseData2
     * @param workGrid1
     * @param workGrid2
     */
    private void unscrambleWxTypes(WeatherGridSlice baseData1,
            WeatherGridSlice baseData2, Grid2DInteger workGrid1,
            Grid2DInteger workGrid2) {
        // extract the byte index grids for keys
        Grid2DByte grid1 = baseData1.getWeatherGrid();
        Grid2DByte grid2 = baseData2.getWeatherGrid();

        // extract the lists of weatherKeys associated with each index grid
        WeatherKey[] keys1 = baseData1.getKeys();
        WeatherKey[] keys2 = baseData2.getKeys();

        // Make a list of WeatherKeys that spans both keys1 and keys2.

        // We are assuming here that "no weather" is always has index 0.

        // first put in all keys from first set
        _allKeys.addAll(Arrays.asList(keys1));

        // Then add on any Keys found in second set not in first set.
        // for every weather in keys2
        for (WeatherKey key : keys2) {
            if (!_allKeys.contains(key)) {
                _allKeys.add(key);
            }
        }

        // _allkeys is now complete: a new list of all DiscreteKeys
        // from both input weather-type GridSlices.

        // Now construct two working integer grids, corresponding to the
        // input byte index grids, but with values for the weatherKeys
        // in _allKeys.

        byte index;
        WeatherKey key;

        // For every grid point in the grids, load the new working grids
        // so points have the same weather texts as input grids but whose
        // indices point to the new list "_allKeys".
        for (int i = 0; i < _xDim; i++) {
            for (int j = 0; j < _yDim; j++) {
                // get the index value from the actual first input grid of bytes
                index = grid1.get(i, j);

                // Can save a lot of processing here if index=0
                // ALWAYS means "no weather": the workGrid1 and 2 values
                // are already set to 0.

                // get its value
                key = keys1[index];

                // find this key in the new list, and save the corresponding
                // index
                for (int k = 0; k < _allKeys.size(); k++) {
                    if (key.equals(_allKeys.get(k))) {
                        workGrid1.set(i, j, k);
                        break;
                    }
                }

                // get the index value from the actual second input grid of
                // bytes
                index = grid2.get(i, j);
                // get its key
                key = keys2[index];
                // find this key in the new list, and save the corresponding
                // index
                for (int k = 0; k < _allKeys.size(); k++) {
                    if (key.equals(_allKeys.get(k))) {
                        workGrid2.set(i, j, k);
                        break;
                    }
                }
            }
        }

        return;
    }

    /**
     * Load the weather type information arrays _wxTypeList1 and _wxTypeList2,
     * for the two input grids.
     * <p>
     * Called once per pair of input wx grids used for interpolation.
     * <p>
     * Output: makes class data _wxTypeList1 and _wxTypeList2.
     * <p>
     * for each weather type in each grid, creates a struct of form GridWxInfo
     * (see the GridWxInfo.H file). Includes: <br>
     * - the wx type value (non-zero) found in grid; <br>
     * - how many grid points found of this wx type in this grid <br>
     * - this weather's area limits: max x, max y, min x, min y. <br>
     * - boolean if contiguous to any other type, <br>
     * - list of other wx types this one is contiguous to. <br>
     * These are appended to the wxTypeLists
     * 
     * @param grid0
     *            A first grid of integer weather values, assumes points of
     *            value 0 are "no weather".
     * @param grid1
     *            A second grid of integer weather values, assumes points of
     *            value 0 are "no weather".
     */
    private void findWxTypes(Grid2DInteger grid0, Grid2DInteger grid1) {
        int wxCount = 0;
        boolean match = false;

        // check original or first input grid for weather types
        for (int i = 0; i < _xDim; i++) {
            for (int j = 0; j < _yDim; j++) {
                // a non-zero weather type was found
                if (grid0.get(i, j) != 0) {
                    match = false;
                    // search the list for this value
                    for (int k = 0; k < _wxTypeList1.size(); k++) {
                        GridWxInfo gwx = _wxTypeList1.get(k);
                        // this type is already in the list at index=k
                        if (gwx.wxType == grid0.get(i, j)) {
                            // note as found
                            match = true;
                            // increment how many of this type found so far
                            gwx.howMany += 1;

                            // reset max and min limits, if needed:
                            if (i > gwx.maxX) {
                                gwx.maxX = i; // reset max x value
                            }
                            if (j > gwx.maxY) {
                                gwx.maxY = j; // reset max y value
                            }
                            if (i < gwx.minX) {
                                gwx.minX = i; // reset min x value
                            }
                            if (j < gwx.minY) {
                                gwx.minY = j; // reset min y value
                            }

                            // search for contiguous points of other types:
                            findContiguousWX(grid0, i, j, k, _wxTypeList1);
                            break;
                        }
                    }

                    // if this type is not yet in the list
                    if (!match) {
                        wxCount++;
                        // add this type to a grid wx info struct
                        GridWxInfo tempGWI = new GridWxInfo(); // working
                        // struct;
                        tempGWI.wxType = grid0.get(i, j); // set wx type
                        tempGWI.howMany = 1; // 1 point with this type found
                        // so
                        // far
                        tempGWI.maxX = i;
                        tempGWI.maxY = j;
                        tempGWI.minX = i;
                        tempGWI.minY = j;
                        tempGWI.isContig = false; // none yet contiguous

                        _wxTypeList1.add(tempGWI);

                        // find any contiguous points of other types:
                        findContiguousWX(grid0, i, j, wxCount - 1, _wxTypeList1);
                    }
                }
            }
        }

        wxCount = 0;

        // check final or second, base or input grid, for weather types
        for (int i = 0; i < _xDim; i++) {
            for (int j = 0; j < _yDim; j++) {
                // a non-zero weather type was found
                if (grid1.get(i, j) != 0) {
                    match = false;
                    // search the list for this value
                    for (int k = 0; k < _wxTypeList2.size(); k++) {
                        GridWxInfo gwx = _wxTypeList2.get(k);
                        // this type is already in the list at index=k
                        if (gwx.wxType == grid1.get(i, j)) {
                            // note as found
                            match = true;
                            // increment how many of this type found so far
                            gwx.howMany += 1;

                            // reset max and min limits, if needed:
                            if (i > gwx.maxX) {
                                gwx.maxX = i; // reset max x value
                            }
                            if (j > gwx.maxY) {
                                gwx.maxY = j; // reset max y value
                            }
                            if (i < gwx.minX) {
                                gwx.minX = i; // reset min x value
                            }
                            if (j < gwx.minY) {
                                gwx.minY = j; // reset min y value
                            }

                            // search for contiguous points of other types:
                            findContiguousWX(grid1, i, j, k, _wxTypeList2);
                            break;
                        }
                    }

                    // if this type is not yet in the list
                    if (!match) {
                        wxCount++;
                        // add this type to a grid wx info struct
                        GridWxInfo tempGWI = new GridWxInfo(); // working
                        // struct;
                        tempGWI.wxType = grid1.get(i, j); // set wx type
                        tempGWI.howMany = 1; // 1 point with this type found
                        // so
                        // far
                        tempGWI.maxX = i;
                        tempGWI.maxY = j;
                        tempGWI.minX = i;
                        tempGWI.minY = j;
                        tempGWI.isContig = false; // none yet contiguous

                        _wxTypeList2.add(tempGWI);

                        // find any contiguous points of other types:
                        findContiguousWX(grid1, i, j, wxCount - 1, _wxTypeList2);
                    }
                }
            }
        }
    }

    /**
     * Seek and note grid points next to another grid point of differing wx
     * type.
     * <p>
     * Check all 9 neighboring points to grid point (oj,oi). If they have index
     * (x,y) values in the grid "grid", and if they have a weather value which
     * is non-zero, and if the value is NOT the same as the value at (oi,oj).
     * Then set flag for contiguous different wx point existence, (a neighbor
     * point with different weather value), and add its value to the list.
     * 
     * @param grid
     * @param oi
     * @param oj
     * @param index
     * @param wxList
     */
    private void findContiguousWX(Grid2DInteger grid, int oi, int oj,
            int index, List<GridWxInfo> wxList) {
        int x, y;
        int wxValue = grid.get(oi, oj);
        boolean match = false;

        // look at the grid points around point oi, oj
        for (int k = -1; k <= 1; k++) {
            for (int l = -1; l <= 1; l++) {
                x = oi + k;
                y = oj + l;

                // fix x and y so they can't exceed grid limits
                if (x < 0) {
                    x = 0;
                }
                if (x > _xDim - 1) {
                    x = _xDim - 1;
                }
                if (y < 0) {
                    y = 0;
                }
                if (y > _yDim - 1) {
                    y = _yDim - 1;
                }

                // if the point has a different and non-zero value
                if (grid.get(x, y) != 0 && grid.get(x, y) != wxValue) {
                    // have found a contiguous point of differing value

                    // set flag for contiguous point
                    wxList.get(index).isContig = true;

                    // if there aren't any contiguous values listed yet, add
                    // this
                    // (this block may be unnecessary considering next section)
                    if (wxList.get(index).contigWxType.size() == 0) {
                        wxList.get(index).contigWxType.add(grid.get(x, y));
                    }

                    // check all values of contiguous points stored so far
                    // if this value is already stored; note that fact
                    for (int m = 0; m < wxList.get(index).contigWxType.size(); m++) {
                        if (wxList.get(index).contigWxType.get(m).intValue() == grid
                                .get(x, y)) {
                            match = true;
                            break;
                        }
                    }

                    if (!match) {
                        wxList.get(index).contigWxType.add(grid.get(x, y));
                    }
                }
                match = false;
            }
        }
    }

    /**
     * Determine the matrix used to indicate which weather types share a
     * boundary with which other types, in both first and last base grids.
     * <p>
     * Construct the &quot;wxTypeInfoMatix&quot; matrix which indicates 1. all
     * weather type (bytes) appearing in the two grids; 2. which ones are
     * contiguous to each other in the two input grids,
     * <p>
     * Example (size not important for example)
     * 
     * <pre>
     * 0 a b c d e 0
     * a 0 1 1 0 1 0
     * b 1 0 0 0 0 0
     * c 1 0 0 1 1 0
     * d 0 0 1 0 1 0
     * e 1 0 0 0 0 0
     * 0 0 0 0 0 0 0
     * </pre>
     * 
     * where a,b,c,d,e are (non-0) integer weather types of any value and order;
     * (Diagonal values are all 0 and serve no function.) Off-diagonal elements
     * =1 means contiguity between the types indicated in first position of its
     * row and column. Upper right side from diagonal indicates contiguity in
     * first input grid; lower left side indicates contiguity in second input
     * grid. It is not necessarily symmetrical but may be so. The order of
     * a,b,c, etc. is the same in first row and column.
     * <p>
     * Called once per pair of input wx grids used for interpolation (may be
     * used for several interpolations between those same grids).
     * 
     */
    private void composeWxTypeInfoMatrix() {
        // make a list of weather type indices from list 1
        List<Integer> types = new ArrayList<Integer>();
        // first load types from first list
        for (int i = 0; i < _wxTypeList1.size(); i++) {
            types.add(_wxTypeList1.get(i).wxType);
        }

        // now load any other type indices found only in list 2
        for (int i = 0; i < _wxTypeList2.size(); i++) {
            boolean match = false;
            for (int j = 0; j < _wxTypeList1.size(); j++) {
                if (_wxTypeList2.get(i).wxType == _wxTypeList1.get(j).wxType) {
                    match = true;
                    break;
                }
            }

            if (!match) {
                types.add(_wxTypeList2.get(i).wxType);
            }
        }

        // keep number of all weather types in the two input grids
        int total = types.size();

        // Set size = square matrix of dimension equal to the total, plus 1.
        // Add all weather types to the matrix as
        // entries in column one; these serve as labels for rows,
        // and as entries in row one; these serve as labels to columns.
        // Note we start at index 1 since (0,0) has no function.
        _wxTypeInfoMatrix = new Grid2DInteger(total + 1, total + 1);

        _wxTypeInfoMatrix.setAllValues(0);

        for (int i = 0; i < total; i++) {
            _wxTypeInfoMatrix.set(i + 1, 0, types.get(i));
            _wxTypeInfoMatrix.set(0, i + 1, types.get(i));
        }

        // The matrix has been sized, and labeled in row and column.
        // Note that the order of weather types in the first column and
        // in the first row is the same; this simplifies later use.

        // Now have to add matrix entries or flags showing contiguity.

        // (as printed, matrix has (0,0) in lower left corner)

        // upper left from matrix diagonal applies to first grid.
        // lower right from matrix diagonal applies to second wx grid.

        // for every weather type in list 1
        for (int i = 0; i < _wxTypeList1.size(); i++) {
            if (_wxTypeList1.get(i).wxType != 0) {
                // for every position in the info matrix first column (except
                // (0,0)
                for (int j = 1; j < _wxTypeInfoMatrix.getYDim(); j++) {
                    // if types match, and
                    // if it contiguous to other types
                    if (_wxTypeList1.get(i).wxType == _wxTypeInfoMatrix.get(0,
                            j) && _wxTypeList1.get(i).isContig) {
                        // now there is a list of the types this one
                        // is contiguous to, but they may not be in
                        // same order as first column in matrix.
                        for (int k = 0; k < _wxTypeList1.get(i).contigWxType
                                .size(); k++) {
                            // check types in first row of matrix;
                            // when find it, flag in matrix, but only in
                            // upper left half: where j>m
                            for (int m = 1; m < _wxTypeInfoMatrix.getXDim(); m++) {
                                if (j > m
                                        && _wxTypeInfoMatrix.get(m, 0) == _wxTypeList1
                                                .get(i).contigWxType.get(k)
                                                .intValue()) {
                                    _wxTypeInfoMatrix.set(m, j, 1); // done!
                                }
                            }
                        }
                    }
                }
            }
        }

        // for every weather type in list 2
        // (exact same logic as previous block)
        for (int i = 0; i < _wxTypeList2.size(); i++) {
            if (_wxTypeList2.get(i).wxType != 0) {
                for (int m = 1; m < _wxTypeInfoMatrix.getXDim(); m++) {
                    if (_wxTypeList2.get(i).wxType == _wxTypeInfoMatrix.get(m,
                            0) && _wxTypeList2.get(i).isContig) {
                        for (int k = 0; k < _wxTypeList2.get(i).contigWxType
                                .size(); k++) {
                            for (int j = 1; j < _wxTypeInfoMatrix.getXDim(); j++) {
                                if (m > j
                                        && _wxTypeInfoMatrix.get(0, j) == _wxTypeList2
                                                .get(i).contigWxType.get(k)
                                                .intValue()) {
                                    _wxTypeInfoMatrix.set(m, j, 1); // done!
                                }
                            }
                        }
                    }
                }
            }
        }

        // The contiguity matrix is complete.

        // print out for development
        // cout<<" contiguity matrix: "<<std::endl;
        // cout<<_wxTypeInfoMatrix<<std::endl;

        return;
    }

    /**
     * <pre>
     * Determine the weather types to be interpolated independently, and
     * those sharing boundary hence interpolated as one area.
     * 
     * Construct another device, arrays, to be control mechanisms for
     *    interpolation.
     * INPUT is data from:
     *    _wxTypeList1,
     *    _wxTypeList2, and
     *    _wxTypeInfoMatrix.
     * OUTPUT is the arrays
     *  single[], a list of weather types each to be interpolated
     *    independently; and
     *  joint[], a list of weather types all to be interpolated as one
     *    area. These types have contiguous edges in both the first and
     *    second grids.
     *    
     *  To sort the components in joint[], the weather types
     *  in joint are listed in order of decreasing number of points
     *  (wx type with biggest areas is listed first).
     * </pre>
     * 
     * @param joint
     * @param single
     */
    private void findWxTypeGroups(List<Integer> joint, List<Integer> single) {
        boolean jointInterp, match, matchFound;

        // check each weather type (use entries in first row of matrix)
        for (int m = 1; m < _wxTypeInfoMatrix.getXDim(); m++) {
            if (_wxTypeInfoMatrix.get(m, 0) != 0) {
                jointInterp = false;

                // check for a contiguity flag with every other
                // non-zero weather type in its row,
                // and ALSO in the symmetrical position
                // TYPES ARE "JOINT" ONLY IF THEY TOUCH BOTH IN FIRST AND
                // SECOND INPUT GRIDS=symmetrical positions in matrix.

                // loop over left col
                for (int n = 1; n < _wxTypeInfoMatrix.getYDim(); n++) {
                    if (n != m // not on diagonal
                            && _wxTypeInfoMatrix.get(m, n) == 1
                            && _wxTypeInfoMatrix.get(n, m) == 1) {
                        jointInterp = true;

                        // add _wxTypeInfoMatrix(m,0) to joint if not already
                        // there
                        match = false;
                        for (int k = 0; k < joint.size(); k++) {
                            if (joint.get(k)
                                    .equals(_wxTypeInfoMatrix.get(m, 0))) {
                                match = true;
                                break;
                            }
                        }
                        if (!match) {
                            joint.add(_wxTypeInfoMatrix.get(m, 0));
                        }

                        // add _wxTypeInfoMatrix(0,n) to joint if not already
                        // there
                        match = false;
                        // loop over contents in joint
                        for (int k = 0; k < joint.size(); k++) {
                            if (joint.get(k)
                                    .equals(_wxTypeInfoMatrix.get(0, n))) {
                                match = true;
                                break;
                            }
                        }
                        if (!match) {
                            joint.add(_wxTypeInfoMatrix.get(0, n));
                        }
                    }
                }

                // Create array of weather types to interpolate singly.
                // If this weather type is not joint,
                // it may be due for single (independent) interpolation.
                if (!jointInterp) {
                    // 1. Only if this type appears in BOTH list 1 and 2:
                    for (int i = 0; i < _wxTypeList1.size(); i++) {
                        if (_wxTypeList1.get(i).wxType == _wxTypeInfoMatrix
                                .get(m, 0)) {
                            for (int j = 0; j < _wxTypeList2.size(); j++) {
                                if (_wxTypeList2.get(j).wxType == _wxTypeInfoMatrix
                                        .get(m, 0)) {
                                    single.add(_wxTypeInfoMatrix.get(m, 0));
                                }
                            }
                        }
                    }

                    // and, optionally, add types which only appear in one list
                    // or the other, but not in both.
                    // These types will "interpolate" in place (no motion)
                    // since only one position is known.
                    for (int i = 0; i < _wxTypeList1.size(); i++) {
                        if (_wxTypeList1.get(i).wxType == _wxTypeInfoMatrix
                                .get(m, 0)) {
                            matchFound = false;
                            for (int j = 0; j < _wxTypeList2.size(); j++) {
                                if (_wxTypeList2.get(j).wxType == _wxTypeInfoMatrix
                                        .get(m, 0)) {
                                    matchFound = true;
                                }
                            }
                            if (!matchFound) {
                                single.add(_wxTypeList1.get(i).wxType);
                            }
                        }
                    }
                    for (int i = 0; i < _wxTypeList2.size(); i++) {
                        if (_wxTypeList2.get(i).wxType == _wxTypeInfoMatrix
                                .get(m, 0)) {
                            matchFound = false;
                            for (int j = 0; j < _wxTypeList1.size(); j++) {
                                if (_wxTypeList1.get(j).wxType == _wxTypeInfoMatrix
                                        .get(m, 0)) {
                                    matchFound = true;
                                }
                            }
                            if (!matchFound) {
                                single.add(_wxTypeList2.get(i).wxType);
                            }
                        }
                        // "single" has been created
                    }
                }
            }
        }

        // Now want to order elements of joint[] by order in which they
        // enclose one another. Intent is to make first weather type which
        // encloses
        // all other types with which it is contiguous, and so on down the list,
        // each type largely enclosing those listed after it.

        // There are at least two methods to try.

        // Method 1. order by how many grid points are in each type.

        // make an array size[] of how many points of each weather
        // type are in the parallel array joint[].
        int[] size = new int[joint.size()];

        for (int i = 0; i < joint.size(); i++) {
            size[i] = 0;
            // find how many points there are for this type in 1st grid
            for (int k = 0; k < _wxTypeList1.size(); k++) {
                if (_wxTypeList1.get(k).wxType == joint.get(i)) {
                    size[i] = _wxTypeList1.get(k).howMany;
                }
            }
        }

        // ADD to size[] how many from from 2nd grid:
        for (int i = 0; i < joint.size(); i++) {
            for (int k = 1; k < _wxTypeList2.size(); k++) {
                if (_wxTypeList2.get(k).wxType == joint.get(i)) {
                    size[i] += _wxTypeList2.get(k).howMany;
                }
            }
        }

        // now "joint" and "size" are parallel lists of weather types and how
        // many
        // points there are of each type.

        // sort weather types in joint[],
        // in order of decreasing how many points of each type in size[].
        int tempwx, tempsize;
        for (int i = 0; i < joint.size(); i++) {
            if (size[i] != 0) {
                for (int j = i + 1; j < joint.size(); j++) {
                    if (size[j] > size[i] && size[j] > 0) {
                        tempwx = joint.get(i);
                        tempsize = size[i];
                        joint.set(i, joint.get(j));
                        size[i] = size[j];
                        joint.set(j, tempwx);
                        size[j] = tempsize;
                    }
                }
            }
        }

        // Method2. order by comparing grid limits (x,y) of each type:
        // which enclose the others.
        // not implemented yet...

        // print out for tests only
        // cout<<" "<<std::endl;
        // cout<<" do joint interpolation for these weather types: "<<std::endl;
        // cout<<joint<<std::endl;
        // cout<<" do independent interpolation for types: "<<std::endl;
        // cout<<single<<std::endl;
        // cout<<" "<<std::endl;

        // Done. No value in single[] should appear in joint[].
        // joint[] should have 2 or more values non-zero, or none.

        return;
    }

    /**
     * Run this immediately before doing interpolation.
     * <p>
     * Creates the _growArea and the _fadeArea area components used to change
     * shape, and the _gaControl and _faControl arrays of distances which depend
     * on angle from the center of a weather type area.
     * 
     * @param grid0
     * @param grid1
     * @param wxValue
     */
    private void wxInterpolatorSetup(Grid2DInteger grid0, Grid2DInteger grid1,
            int wxValue) {
        // looping controls and temporary grid indices
        int x, y, m, n, mainx, mainy;
        int deltax, deltay, sector;
        float dist;
        double arg;

        // minFAdist=0.0;
        // minGAdist=0.0;
        // maxFAdist=0.0;
        // maxGAdist=0.0;

        // zero values in temporary feature location grids and _coreArea
        _featureStart.setAllValues(0);
        _featureEnd.setAllValues(0);
        _coreArea.setAllValues(0);

        // set zero values in working grids _growArea and _fadeArea.
        // twice as large as data grid, to help edge problems.
        _growArea.setAllValues(0);
        _fadeArea.setAllValues(0);

        // set zeros in distance control info for _growArea (ga)
        // and _fadeArea (fa).
        // first value (j=0) is min distance; second (j=1) is max distance,
        // for grow Area and _fadeArea points in the angle sector i.
        _gaControl.setAllValues(0.0f);
        _faControl.setAllValues(0.0f);

        // A. NOTE ALL POINTS WITH THIS DISCRETE TYPE IN THE GRID.
        // Note which points in the original grid are in the weather area
        // (noted by _featureStart(i,j) = wxValue),
        // and which points in the final grid are in the weather area
        // (noted by _featureEnd(i,j) = wxValue).

        for (int i = 0; i < _xDim; i++) {
            for (int j = 0; j < _yDim; j++) {
                if (grid1.get(i, j) == wxValue) {
                    _featureEnd.set(i, j, wxValue);
                }

                if (grid0.get(i, j) == wxValue) {
                    _featureStart.set(i, j, wxValue);
                }
            }
        }

        // B. Find center and displacement of areas with this weather value.

        center1 = computeAreaCenter(_featureStart, wxValue);
        center2 = computeAreaCenter(_featureEnd, wxValue);

        // if one center x,y pair is 0,0 that means no points of that
        // kind of weather wxValue were found in that grid. Reset the
        // 0,0 to the other center x,y so the area does not move to the
        // corner. It will grow or shrink in the same place.
        if (center1.x == 0 && center1.y == 0) {
            center1.x = center2.x;
            center1.y = center2.y;
        }
        if (center2.x == 0 && center2.y == 0) {
            center2.x = center1.x;
            center2.y = center1.y;
        }

        // compute displacement (dx,dy) to go from first area
        // to second area (motion of centers)
        _dx = center2.x - center1.x;
        _dy = center2.y - center1.y;

        // C. Determine the working grids "_coreArea" and "_fadeArea"
        // Find "_coreArea" which indicates the grid points in BOTH
        // the original and final area, using the displacement.
        // Find points in first grid of this wxValue which do not
        // appear in the final grid when translated by (dx,dy).
        // These points must disappear or fade out at some
        // interpolation step: the _fadeArea points.

        // Check every point on the original grid
        for (int i = 0; i < _xDim; i++) {
            for (int j = 0; j < _yDim; j++) {
                // compute corresponding position on translated part of grid
                x = i + _dx;
                y = j + _dy;

                // if the translated point is still on visible grid
                if (x >= 0 && x < _xDim && y >= 0 && y < _yDim) {
                    // if this point is part of BOTH the original area,
                    // and part of the final area when translated,
                    // it is a point in the "core area."
                    if (_featureStart.get(i, j) == wxValue
                            && _featureEnd.get(x, y) == wxValue) {
                        _coreArea.set(i, j, 1);
                    }

                    // if it appears in original grid but not in final grid,
                    // it will "fade away", so note in _fadeArea.
                    if (_featureStart.get(i, j) == wxValue
                            && _featureEnd.get(x, y) != wxValue) {
                        _fadeArea.set(i + _xDim / 2, j + _yDim / 2, 1);

                        // compute distance "dist" from center of
                        // weather area to this _fadeArea point
                        dist = (float) Math.sqrt((i - center1.x)
                                * (i - center1.x) + (j - center1.y)
                                * (j - center1.y));

                        // compute which angle sector this point lies in
                        deltax = i - center1.x;
                        deltay = j - center1.y;
                        sector = findAngleSector(deltax, deltay);

                        // if dist exceeds an existing limit, reassign it.
                        // set max distance to edge of _fadeArea in this sector
                        if (dist > _faControl.get(sector, 1)) {
                            _faControl.set(sector, 1, dist);
                        }
                        // set min distance to edge of _fadeArea in this sector
                        if (dist < _faControl.get(sector, 0)
                                || _faControl.get(sector, 0) == 0.0f) {
                            _faControl.set(sector, 0, dist);
                        }
                    }
                }

                // for original grid points which land
                // outside data grid when projected:
                // keep 'em as _fadeArea points.
                if (_featureStart.get(i, j) == wxValue
                        && (x < 0 || x >= _xDim || y < 0 || y >= _yDim)) {
                    _fadeArea.set(i + _xDim / 2, j + _yDim / 2, 1);
                    dist = (float) Math.sqrt((i - center1.x) * (i - center1.x)
                            + (j - center1.y) * (j - center1.y));

                    // compute which angle sector this point lies in
                    deltax = i - center1.x;
                    deltay = j - center1.y;
                    sector = findAngleSector(deltax, deltay);

                    // if dist exceeds an existing limit, reassign it.
                    // set max distance to edge of _fadeArea in this sector
                    if (dist > _faControl.get(sector, 1)) {
                        _faControl.set(sector, 1, dist);
                    }
                    // set min distance to edge of _fadeArea in this sector
                    if (dist < _faControl.get(sector, 0)
                            || _faControl.get(sector, 0) == 0.0f) {
                        _faControl.set(sector, 0, dist);
                    }
                }
            }
        }

        // Now work the other way to find the "grow area".
        // The _growArea include points which must suddenly appear
        // at some interpolation step
        // to help create the final grid; points which, when transposed
        // from the final grid
        // back to the original grid coordinates, are either outside
        // the original grid,
        // or on it but not part of orignal weather area.
        // Also find the max and min distance from center,
        // for points in the grow area, in each angle (sector)
        // from center of area.
        // Also adjust grow area for points which are on edge on both
        // original and final grids; will stick to grid edge here, by
        // adjusting grow area points.

        // loop on coordinates for final grid:
        for (y = 0; y < _yDim; y++) {
            for (x = 0; x < _xDim; x++) {
                // If it appears in this wx area in final grid,
                // it will start in _growArea
                // (maybe somewhere outside true grid)
                if (_featureEnd.get(x, y) == wxValue) {
                    // compute corresponding position
                    // in original grid system;
                    // may be outside actual original grid -
                    // points there are also necessary,
                    // to prevent some "edge effects."
                    mainx = x - _dx;
                    mainy = y - _dy;

                    // indices in the _growArea grid:
                    m = mainx + _xDim / 2;
                    n = mainy + _yDim / 2;
                    // (make sure (n,m) inside the _growArea grid)
                    if (m >= 0 && m < 2 * _xDim && n >= 0 && n < 2 * _yDim) {
                        _growArea.set(m, n, 1);

                        // compute distance "dist" from center of
                        // weather area to this _growArea point
                        arg = (mainx - center1.x) * (mainx - center1.x)
                                + (mainy - center1.y) * (mainy - center1.y);
                        dist = (float) Math.sqrt(arg);

                        // compute which angle sector this point lies in
                        deltax = mainx - center1.x;
                        deltay = mainy - center1.y;
                        sector = findAngleSector(deltax, deltay);

                        // if dist exceeds an existing limit, reassign it.
                        // set max distance to edge of _growArea in this sector
                        if (dist > _gaControl.get(sector, 1)) {
                            _gaControl.set(sector, 1, dist);
                        }

                        // set min distance to edge of _growArea in this sector
                        if (dist < _gaControl.get(sector, 0)
                                || _gaControl.get(sector, 0) == 0.0f) {
                            _gaControl.set(sector, 0, dist);
                        }
                    }
                }
            }
        }

        // All the working grids and information have been constructed.

        return;
    }

    /**
     * Find angle sector number (numbers 0 to numSectors for 0 to 360 degrees)
     * that this x,y pair lies in.
     * 
     * @param x
     * @param y
     * @return
     */
    private int findAngleSector(int x, int y) {
        int sector;
        double angle = 0.0;

        // find the angle in radians:
        if (x != 0) {
            angle = Math.atan2(y, x);
        } else {
            if (y > 0) {
                angle = Math.PI / 2.0;
            } else if (y == 0) {
                angle = 0.0;
            } else if (y < 0) {
                angle = -Math.PI / 2.0;
            }
        }

        // angle is in range -pi to +pi radians

        // (A) number of radians per sector = 2 * pi /(how many sectors in
        // circle)
        double step = 2.0 * Math.PI / NUM_SECTORS;

        // compartimentalize into sectors:
        sector = (int) (0.5 + (angle + Math.PI) / step) - 1;

        // (A)
        // bad value clamp (should not occur)
        if (sector < 0 || sector >= NUM_SECTORS) {
            sector = 0;
        }

        return sector;
    }

    /**
     * Compute center of gravity of points in a 2D grid "featureLocation" whose
     * grid point int values == testValue. Ignores other weather values. Used in
     * interpolation of a moving feature between two grids.
     * <p>
     * This method of finding the "center" of the moving feature uses the mean x
     * and y locations of points in the feature location grids with value
     * "testValue."
     * 
     * @param featureLocation
     * @param testValue
     * @return
     */
    private Point computeAreaCenter(Grid2DInteger featureLocation, int testValue) {
        int num = 0;
        float sumX = 0.0f, sumY = 0.0f;

        // check every point on the grid for the proper value,
        // and sum all x and y positions for such points.
        for (int i = 0; i < _xDim; i++) {
            for (int j = 0; j < _yDim; j++) {
                if (featureLocation.get(i, j) == testValue) {
                    sumX += i;
                    sumY += j;
                    num++;
                }
            }
        }

        Point center = new Point();
        if (num != 0) {
            center.x = (int) (0.5 + sumX / num);
            center.y = (int) (0.5 + sumY / num);
        } else {
            center.x = 0;
            center.y = 0;
        }

        return center;
    }

    /**
     * <pre>
     * Create a new grid of weather key integer values
     * by interpolation between two given grids.
     * This really does all the interpolation; it puts together
     * all the pieces that were prepared by other functions of this class.
     * 
     * This function is called by the public function &quot;interpolate()&quot;
     * 
     * INPUT:
     *         wxValue = int value indicating kind of weather (wx key)
     *         zeta is time fraction for desired interpolation from
     *           original grid0 to final grid1,
     *           as 3/4 = 0.75 for 3rd step in 3 interpolated steps.
     * 
     * OUTPUT: is grid &quot;newGrid&quot; - a grid of zeros and the interpolated
     * grid points of value &quot;wxValue&quot;.
     * 
     * Uses concept of a &quot;center&quot; for the area of this weather value;
     * and a displacement (_dx, _dy) which shows how the center of the original
     * area moves to arrive at the center of the final area.
     * 
     * Breaks the whole area for this weather value into three parts:
     * 1. the core which moves from start to final area without changing,
     *    following the displacement;
     * 2. the grow area: points not in the original area which appear
     *    in the final area (if you reverse (dx,dy) starting at a point in the
     *    final area and it does not land in the original area, you have a
     *    point in the grow area);
     * 3. the &quot;fade area&quot; which has points in the original area which when
     *    translated with (dx,dy) do not appear in the final area.
     * 
     * Interpolation is linear in the sense that the grow area grows
     * linearly from inital shape to final shape
     * (but independently along each radius out from center of area),
     * and fade area points drop out similarly.
     * Grow area points are controlled by distance values in _gaControl.
     * Fade area points controlled by _faControl.
     * 
     * Interpolates one weather value, &quot;wxValue&quot;,
     * even if input grids have several values.
     * This is used repetitive times to interpolate for several weather areas.
     * </pre>
     * 
     * @param wxValue
     * @param newGrid
     * @param zeta
     */
    private void weatherInterpolator(int wxValue, Grid2DInteger newGrid,
            float zeta) {
        // temporary looping controls and grid indices; distance variables:
        int upIndex, dnIndex;
        double dist, testGAdist, testFAdist, minDist, maxDist;

        // temp grid indices and a variable for angle sector numbers:
        int mainy, mainx, tmainy, tmainx, sector;

        // zero values in newGrid
        newGrid.setAllValues(0);

        // Add in core points.
        // loop over coordinates in original grid
        for (int i = 0; i < _xDim; i++) {
            for (int j = 0; j < _yDim; j++) {
                // add valid core points to the result
                // at the translated position
                if (_coreArea.get(i, j) != 0) {
                    // equivalent translated position for this step:
                    int jj = j + (int) ((_dy * zeta) + 0.5);
                    int ii = i + (int) ((_dx * zeta) + 0.5);
                    newGrid.set(ii, jj, wxValue);
                }
            }
        }

        // Add in points from grow and fade Areas where appropriate.
        // loop over coordinates for _growArea and _fadeArea grids
        for (int i = 0; i < 2 * _xDim; i++) {
            for (int j = 0; j < 2 * _yDim; j++) {
                // "i,j" positions for grow or fade area
                // as if on on main grid coordinates (there may not actually
                // be any main grid points with these indexes).
                mainx = i - _xDim / 2;
                mainy = j - _yDim / 2;

                // distance from center of initial area to this g or f area
                // point.
                dist = Math.sqrt((mainx - center1.x) * (mainx - center1.x)
                        + (mainy - center1.y) * (mainy - center1.y));

                // translated position for this step:
                // (coordinates in _growArea and _fadeArea grid system)
                int ii = i + (int) ((_dx * zeta) + 0.5);
                int jj = j + (int) ((_dy * zeta) + 0.5);

                // translated position on main grid
                tmainy = jj - _yDim / 2;
                tmainx = ii - _xDim / 2;

                // get angle sector for this point
                sector = findAngleSector(mainx - center1.x, mainy - center1.y);

                // grow area selection:

                // simple control based just on this sector.
                // (may give poor results in very odd-shaped areas,
                // since adjacent areas vary so muc, for example, one
                // area may start growing while apparently detached from
                // rest of area, if it has acurved arm sticking way out.
                minDist = _gaControl.get(sector, 0);
                maxDist = _gaControl.get(sector, 1);

                // improved control:
                // average neighboring distances for smoother control
                upIndex = sector + 1;
                if (upIndex >= NUM_SECTORS) {
                    upIndex = 0;
                }

                dnIndex = sector - 1;
                if (dnIndex < 0) {
                    dnIndex = NUM_SECTORS - 1;
                    // minDist = (_gaControl(sector,0)+_gaControl(upIndex,0)+
                    // _gaControl(dnIndex,0))/3.0;
                    // maxDist = (_gaControl(sector,1)+_gaControl(upIndex,1)+
                    // _gaControl(dnIndex,1))/3.0;
                }

                // expand the growing area out to current fractional size
                if (_growArea.get(i, j) != 0) {
                    // (B) optional code goes here

                    // find growing limit distance for _growArea for this
                    // interpolation step and sector
                    testGAdist = minDist + (zeta * (maxDist - minDist));

                    // if this grow area point is within test distance
                    // from center of moving area, addthis point to the weather
                    // area.
                    // (also make sure it's on the main grid!)
                    if (dist < testGAdist && tmainy >= 0 && tmainx < _xDim
                            && tmainx >= 0 && tmainy < _yDim) {
                        newGrid.set(tmainx, tmainy, wxValue);
                    }

                }

                // fade area selection:

                // simple control: see note above for _gaControl
                minDist = _faControl.get(sector, 0);
                maxDist = _faControl.get(sector, 1);

                // average neighboring distances for smoother control
                upIndex = sector + 1;
                if (upIndex >= NUM_SECTORS) {
                    upIndex = 0;
                }

                dnIndex = sector - 1;
                if (dnIndex < 0) {
                    dnIndex = NUM_SECTORS - 1;
                }

                // minDist = (_faControl(sector,0)+_faControl(upIndex,0)+
                // _faControl(dnIndex,0))/3.0;
                // maxDist = (_faControl(sector,1)+_faControl(upIndex,1)+
                // _faControl(dnIndex,1))/3.0;

                // insert points from fade area only if each is inside
                // the contracting limit distance for its sector.
                if (_fadeArea.get(i, j) != 0) {
                    // find shrinking limit distance for _fadeArea for this
                    // interpolation step and sector
                    // all points > this distance fade out now.
                    // orig:testFAdist = minDist+(zeta*(maxDist-minDist));
                    testFAdist = maxDist - (zeta * (maxDist - minDist));

                    // only keep a _fadeArea point inside the
                    // limiting distance
                    // (also make sure it's on the main grid!)
                    if (dist < testFAdist && tmainy >= 0 && tmainy < _yDim
                            && tmainx >= 0 && tmainx < _xDim) {
                        newGrid.set(tmainx, tmainy, wxValue);
                    }
                }
            }
        }

        return;
    }

    /**
     * interpolate successively areas which touch in both the first and second
     * base or input grids, so that no gaps or overlaps appear in groups of
     * weather types which should move as one unit.
     * <p>
     * starts with the outside one, then next inner one, etc., so that each one
     * includes its own area and the enclosed types' areas. No combination types
     * are made here.
     * 
     * @param joint
     * @param workGrid1
     * @param workGrid2
     * @param groupGrid1
     * @param groupGrid2
     * @param newGrid
     * @param fraction
     * @param interpResult
     */
    private void jointInterpControl(List<Integer> joint,
            Grid2DInteger workGrid1, Grid2DInteger workGrid2,
            Grid2DInteger groupGrid1, Grid2DInteger groupGrid2,
            Grid2DInteger newGrid, float fraction, Grid2DInteger interpResult) {
        int wxType;

        // interpolate successive joint areas,
        // starting with the outside one;
        // each one including its own area and the enclosed types' areas.
        for (int m = 0; m < joint.size(); m++) {
            if (joint.get(m) != 0) {
                // this interpolation will use wx type joint.get(m)
                wxType = joint.get(m);

                // and, this interpolation will use an AREA
                // including all areas from type from joint.get(m) on up
                for (int n = m; n < joint.size(); n++) {
                    if (joint.get(n) != 0) {
                        // add to the 1st working area where this wx type is.
                        for (int i = 0; i < _xDim; i++) {
                            for (int j = 0; j < _yDim; j++) {
                                if (workGrid1.get(i, j) == joint.get(n)) {
                                    groupGrid1.set(i, j, wxType);
                                }
                            }
                        }

                        // add to the 2nd work area where this wx type is.
                        for (int i = 0; i < _xDim; i++) {
                            for (int j = 0; j < _yDim; j++) {
                                if (workGrid2.get(i, j) == joint.get(n)) {
                                    groupGrid2.set(i, j, wxType);
                                }
                            }
                        }
                    }
                }

                // interpolate one weather type
                // call interpolator setup
                wxInterpolatorSetup(groupGrid1, groupGrid2, wxType);

                // create interpolated grid in "newGrid"
                weatherInterpolator(wxType, newGrid, fraction);

                // Overlay this wx type and area on result -
                // do not make combinations since this is to create
                // a joint interpolation with no overlaps (combos) or gaps.
                for (int i = 0; i < _xDim; i++) {
                    for (int j = 0; j < _yDim; j++) {
                        if (newGrid.get(i, j) != 0) {
                            interpResult.set(i, j, newGrid.get(i, j));
                        }
                    }
                }
            }
        }
    }

    /**
     * Interpolate wx areas which do not touch other areas in both the first and
     * second base input grids. Combinations are generated where overlaps occur.
     * 
     * If there are wx types for independent interpolation, do all of them
     * singly (without reference to other weather types), and accumulate the
     * results, making combination wx types if necessary.
     * 
     * @param single
     * @param workGrid1
     * @param workGrid2
     * @param newGrid
     * @param fraction
     * @param interpResult
     */
    private void singleInterpControl(List<Integer> single,
            Grid2DInteger workGrid1, Grid2DInteger workGrid2,
            Grid2DInteger newGrid, float fraction, Grid2DInteger interpResult) {
        int wxValue;

        for (int m = 0; m < single.size(); m++) {
            if (single.get(m) != 0) // don't do no-weather case
            {
                // do independent interpolation for this type
                wxValue = single.get(m);

                // call interpolator setup
                wxInterpolatorSetup(workGrid1, workGrid2, wxValue);

                // create interpolated grid in "newGrid"
                weatherInterpolator(wxValue, newGrid, fraction);

                // combine this wxValue result with result grid;
                // check for combinations and make them if needed.
                for (int i = 0; i < _xDim; i++) {
                    for (int j = 0; j < _yDim; j++) {
                        if (newGrid.get(i, j) != 0) // interpResult starts with
                                                    // 0
                        {
                            if (interpResult.get(i, j) == 0) {
                                // combination
                                // need
                                interpResult.set(i, j, newGrid.get(i, j));
                            } else {
                                // already have some weather type set at
                                // this point. Must make a combination type.
                                // the subkey separator is ^
                                // TextString separator = TextString("^");
                                // add together the exiting type and the
                                // new one.
                                // (do not combine same type with self)
                                // if (_allKeys[interpResult.get(i,j)] !=
                                // _allKeys[newGrid.get(i,j)])
                                WeatherKey newKey = new WeatherKey(
                                        _allKeys.get(interpResult.get(i, j)));
                                newKey.addAll(_allKeys.get(newGrid.get(i, j)));

                                // check if this type already exists
                                boolean match = false;
                                int newIndex = 0;
                                for (int h = 0; h < _allKeys.size(); h++) {
                                    if (_allKeys.get(h).equals(newKey)) {
                                        match = true;
                                        newIndex = h;
                                    }
                                }
                                if (!match) {
                                    // cout<<" new combination wx type: "
                                    // <<newKey<<std::endl;
                                    // get number that will be the index
                                    // for this new combination type
                                    // in _allKeys.
                                    newIndex = _allKeys.size();
                                    // add the new combo type to _allKeys
                                    _allKeys.add(newKey);
                                    // cout<<" new list of wx types: "<<_allKeys
                                    // <<std::endl;
                                }

                                // insert this into the interpolated grid
                                interpResult.set(i, j, newIndex);
                            }
                        }
                    }
                }
            }
        }
    }
}
