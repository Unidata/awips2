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
package com.raytheon.viz.gfe.smarttool;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.IGrid2D;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.MissingDataModeMsg;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Ported from GridCycler.C. Retrieves and stores data on a grid.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2008            njensen     Initial creation
 * Mar 13, 2013 1791       bsteffen    Implement bulk getGrids to improve
 *                                     performance.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GridCycler {

    private static GridCycler instance;

    private ArrayList<String> missingData = new ArrayList<String>();

    private GridCycler() {

    }

    public static GridCycler getInstance() {
        if (instance == null) {
            instance = new GridCycler();
        }

        return instance;
    }

    /**
     * Get the grids for the given parm the correspond to the timeRange.
     * 
     * @param argParm
     *            the parm to get grids for
     * @param timeRange
     *            the requested time range
     * @param mode
     *            "TimeWtAverage", "Average", "Min", "Max", "Sum" -- time
     *            weighted average, average, min, max, or sum of corresponding
     *            grids
     * @param dataMode
     *            how to handle missing data
     * @return an IGridData of the data or null
     * @throws GFEOperationFailedException
     */
    public IGridData[] getCorrespondingResult(Parm argParm,
            TimeRange timeRange, String mode, MissingDataMode dataMode)
            throws GFEOperationFailedException {
        IGridData[] grids = argParm.getGridInventory(timeRange);

        IGridData resultGrid = null;
        IGridData[] resultGrids = new IGridData[0];

        if (grids.length == 0) {
            if (dataMode == MissingDataMode.CREATE)
            // Missing Data Mode is Create
            {
                grids = argParm.createCorrespondingGrids(timeRange);

                if (grids.length != 0) {
                    for (int i = 0; i < grids.length; i++) {
                        String msg = "Created: " + argParm.expressionName()
                                + ":" + timeRange.toString() + "\n";
                        addMissingData(msg);
                    }
                }
            }
        }

        if (grids.length == 0) {
            ;
        } else if ("First".equals(mode) || grids.length == 1) {
            grids[0].populate();
            resultGrid = grids[0];
            resultGrids = new IGridData[] { resultGrid };
        } else if ("List".equals(mode)) {
            for (IGridData grid : grids) {
                grid.populate();
            }
            resultGrids = grids;
        } else {
            Parm tempParm = null;
            if ("Max".equals(mode)) {
                tempParm = argParm.max(timeRange);
            } else if ("Min".equals(mode)) {
                tempParm = argParm.min(timeRange);
            } else if ("Sum".equals(mode)) {
                tempParm = argParm.sum(timeRange);
            } else if ("Average".equals(mode)) {
                tempParm = argParm.avg(timeRange);
            } else if ("TimeWtAverage".equals(mode)) {
                tempParm = argParm.twavg(timeRange);
            } else {
                throw new UnsupportedOperationException(
                        "GridCycler Problem with 'mode' argument " + mode);
            }

            if (tempParm != null) {
                grids = tempParm.getGridInventory();
                resultGrid = grids[0];
                resultGrids = new IGridData[] { resultGrid };
            }
        }

        return resultGrids;
    }

    /**
     * This is a wrapper around the 4-arg method, that just uses the current GFE
     * preferences.
     * 
     * @param argParm
     *            the parm to get grids for
     * @param timeRange
     *            the requested time range
     * @param mode
     *            "TimeWtAverage", "Average", "Min", "Max", "Sum" -- time
     *            weighted average, average, min, max, or sum of corresponding
     *            grids
     * @return an IGridData of the data or null
     * @throws GFEOperationFailedException
     */
    public IGridData[] getCorrespondingResult(Parm argParm,
            TimeRange timeRange, String mode)
            throws GFEOperationFailedException {
        String missingMode = Message
                .inquireLastMessage(MissingDataModeMsg.class).getMode()
                .toString();
        MissingDataMode dataMode = MissingDataMode.valueOf(missingMode
                .toUpperCase());
        return getCorrespondingResult(argParm, timeRange, mode, dataMode);
    }

    /**
     * Performs the same basic function as getCorrespondingResult but operates
     * on an array of timeRanges and returns an array of data(one entry for each
     * corresponding timeRange).
     * 
     * @param argParm
     *            the parm to get grids for
     * @param timeRanges
     *            the requested time ranges
     * @param mode
     *            "TimeWtAverage", "Average", "Min", "Max", "Sum" -- time
     *            weighted average, average, min, max, or sum of corresponding
     *            grids
     * @return an IGridData[][] of the data
     * @throws GFEOperationFailedException
     */
    public IGridData[][] getCorrespondingResult(Parm argParm,
            TimeRange[] timeRanges, String mode)
            throws GFEOperationFailedException {
        // first step is to determine which grids need to be populated.
        List<IGridData> grids = new ArrayList<IGridData>();
        for (TimeRange timeRange : timeRanges) {
            IGridData[] inv = argParm.getGridInventory(timeRange);
            for (IGridData data : inv) {
                if (!data.isPopulated()) {
                    grids.add(data);
                }
            }
        }
        // next step populate any unpopulated.
        if (!grids.isEmpty()) {
            argParm.populateGrids(grids);
        }
        // finally just process each range individually.
        IGridData[][] results = new IGridData[timeRanges.length][];
        for (int i = 0; i < timeRanges.length; i += 1) {
            results[i] = getCorrespondingResult(argParm, timeRanges[i],
                    mode);
        }
        return results;
    }

    /**
     * Store the given Numeric Python grid in the given parm and timeRange
     * masked by the given edit area
     * 
     * @param parm
     *            the parm being stored
     * @param timeRange
     *            the time range to store
     * @param refset
     *            the edited area
     * @param result
     *            the numeric grid to store, as returned by SmartTool.execute()
     * @throws GFEOperationFailedException
     */
    public void storeNumericGrid(Parm parm, TimeRange timeRange,
            ReferenceData refset, Object result)
            throws GFEOperationFailedException {
        IGridData[] grids = parm.getGridInventory(timeRange);
        final Grid2DBit pointsToSet = refset.getGrid();
        if (pointsToSet.isAnyBitsSet()) {
            try {
                switch (parm.getGridInfo().getGridType()) {
                case SCALAR: {
                    for (int i = 0; i < grids.length; i++) {
                        ((ScalarGridData) grids[i]).set((Grid2DFloat) result,
                                pointsToSet);
                    }
                    break;
                }
                case VECTOR: {
                    Grid2DFloat[] cast = (Grid2DFloat[]) result;
                    Grid2DFloat mag = cast[0];
                    Grid2DFloat dir = cast[1];
                    for (int i = 0; i < grids.length; i++) {
                        ((VectorGridData) grids[i]).set(mag, dir, pointsToSet);
                    }
                    break;
                }
                case WEATHER: {
                    Object[] cast = (Object[]) result;
                    Grid2DByte bgrid = (Grid2DByte) cast[0];
                    List<?> keys = (List<?>) cast[1];
                    List<WeatherKey> keyList = new ArrayList<WeatherKey>();
                    String siteId = parm.getParmID().getDbId().getSiteId();
                    for (Object o : keys) {
                        keyList.add(new WeatherKey(siteId, (String) o));
                    }

                    for (int i = 0; i < grids.length; i++) {
                        ((WeatherGridData) grids[i]).set(bgrid, keyList,
                                pointsToSet);
                    }
                    break;
                }
                case DISCRETE: {
                    ParmID parmId = parm.getParmID();
                    String siteId = parmId.getDbId().getSiteId();

                    Object[] cast = (Object[]) result;
                    List<?> pkeys = (List<?>) cast[1];
                    List<DiscreteKey> keys = new ArrayList<DiscreteKey>();
                    for (Object o : pkeys) {
                        keys.add(new DiscreteKey(siteId, (String) o, parmId));
                    }

                    Grid2DByte bgrid = (Grid2DByte) cast[0];
                    for (int i = 0; i < grids.length; i++) {
                        ((DiscreteGridData) grids[i]).set(bgrid, keys,
                                pointsToSet);
                    }
                    break;
                }
                }

            } catch (Exception e) {
                throw new GFEOperationFailedException(
                        "Error storing numeric grid", e);
            }
        }
    }

    /**
     * Clears the missing data messages list
     */
    public void clearMissingData() {
        missingData.clear();
    }

    /**
     * Adds a new message about missing data
     * 
     * @param msg
     *            the message about how missing data was handled
     */
    public void addMissingData(String msg) {
        missingData.add(msg);
    }

    /**
     * @return the list of missing data messages
     */
    public List<String> getMissingData() {
        return missingData;
    }

    /**
     * Create grid data of the appropriate type from the input parameters.
     * 
     * @param parm
     *            The Parm with which the grid data will be associated
     * @param timeRange
     *            The time range of the grid data
     * @param grid
     *            A 2D grid of floats or bytes
     * @param auxGrid
     *            A second grid, for vector parms
     * @param auxKeys
     *            key strings, for discrete grid data
     * @return a one-element array with the grid data
     * @throws GFEServerException
     */
    public IGridData[] makeGridDataFromNumeric(Parm parm, TimeRange timeRange,
            IGrid2D grid, IGrid2D auxGrid, List<String> auxKeys)
            throws GFEServerException {
        ParmID parmID = parm.getParmID();
        GridParmInfo gpi = parm.getGridInfo();

        // Create a data history array
        GridDataHistory gdh = new GridDataHistory(OriginType.CALCULATED,
                parmID, timeRange);
        GridDataHistory[] gdha = new GridDataHistory[] { gdh };

        IGridSlice slice = null;
        IGridData[] data = new IGridData[1];

        GridType gridType = gpi.getGridType();

        if (GridType.SCALAR == gridType) {
            slice = new ScalarGridSlice(timeRange, gpi, gdha,
                    (Grid2DFloat) grid);
            data[0] = new ScalarGridData(parm, slice);
        } else if (GridType.VECTOR == gridType) {
            Grid2DFloat mag = (Grid2DFloat) grid;
            Grid2DFloat dir = (Grid2DFloat) auxGrid;
            slice = new VectorGridSlice(timeRange, gpi, gdha, mag, dir);
            data[0] = new VectorGridData(parm, slice);
        } else if (GridType.WEATHER == gridType) {
            // Build an array of all the discrete keys in the definition.
            List<WeatherKey> keyList = new ArrayList<WeatherKey>(auxKeys.size());

            String siteId = parmID.getDbId().getSiteId();
            for (String key : auxKeys) {
                keyList.add(new WeatherKey(siteId, key));
            }

            // final String NONE = "<NoCov>:<NoWx>:<NoInten>:<NoVis>:";
            // WeatherKey noneKey = new WeatherKey(NONE);
            // makeFirstKey(noneKey, grid, keyList);

            WeatherKey[] weatherKeys = keyList.toArray(new WeatherKey[] {});
            // Create a grid gridSlice from the grid and discrete keys
            slice = new WeatherGridSlice(timeRange, gpi, gdha,
                    (Grid2DByte) grid, weatherKeys);
            data[0] = new WeatherGridData(parm, slice);
        } else if (GridType.DISCRETE == gridType) {
            ParmID parmId = parm.getParmID();
            String siteId = parmId.getDbId().getSiteId();

            // Build an array of all the discrete keys in the definition.
            List<DiscreteKey> dkList = new ArrayList<DiscreteKey>(2);
            for (String key : auxKeys) {
                dkList.add(new DiscreteKey(siteId, key, parmId));
            }

            // final String NONE = "<None>";
            // DiscreteKey noneKey = hzdef.keyFromString(NONE);
            // makeFirstKey(noneKey, grid, dkList);

            DiscreteKey[] discreteKeys = dkList.toArray(new DiscreteKey[] {});

            // Create a grid gridSlice from the grid and discrete keys
            slice = new DiscreteGridSlice(timeRange, gpi, gdha,
                    (Grid2DByte) grid, discreteKeys);

            // Create a DiscreteGridData and save it in the output array
            data[0] = new DiscreteGridData(parm, slice);
        } else {
            throw new RuntimeException("Unknown grid type " + gridType);
        }

        return data;
    }

    /**
     * We were forcing the first key of generated weather or discrete grids to
     * be NoWx or None, but the byte grid indices were thrown off because they
     * weren't being adjusted. Fixing that behavior broke some formatter code
     * that apparently expected NoWx to be the first key. This is the
     * complicated version.
     * <p>
     * If NoWx/None is already the first key, nothing happens. If it is not in
     * the grid at all, it is added as the last key and then swapped with key 0.
     * If it is in the grid, but not as key 0, then the keys for NoWx/None and
     * key 0 are swapped. After any key change, the index grid is adjusted
     * accordingly.
     * <p>
     * 
     * @param noneKey
     *            The key to force to dklist[0] (assumed to be NoWx or None)
     * @param grid
     *            The Grid2DByte of a WEATHER or DISCRETE grid
     * @param dkList
     *            The discrete keys of the grid
     */
    private void makeFirstKey(DiscreteKey noneKey, IGrid2D grid,
            List<DiscreteKey> dkList) {
        Grid2DByte byteGrid = (Grid2DByte) grid;
        int noneIdx = dkList.indexOf(noneKey);
        switch (noneIdx) {
        case 0:
            break; // this is the desired condition
        case -1:
            // Append old key 0 to end, replace key 0 with NoWx
            dkList.add(dkList.get(0));
            dkList.set(0, noneKey);
            byteGrid.setAllOfValue((byte) 0, (byte) (dkList.size() - 1));
            break;
        default: // swap keys to put NoWx at key 0
            dkList.set(noneIdx, dkList.get(0));
            dkList.set(0, noneKey);
            byteGrid.setAllOfValue((byte) 0, Byte.MAX_VALUE);
            byteGrid.setAllOfValue((byte) noneIdx, (byte) 0);
            byteGrid.setAllOfValue(Byte.MAX_VALUE, (byte) noneIdx);
            break;
        }
    }
}
