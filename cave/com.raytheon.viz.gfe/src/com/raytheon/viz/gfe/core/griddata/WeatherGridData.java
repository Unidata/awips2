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
package com.raytheon.viz.gfe.core.griddata;

import java.awt.Point;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.mutable.MutableByte;
import org.geotools.geometry.jts.JTS;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.parm.ParmState.CombineMode;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.MultiPolygon;

/**
 * Grid containing weather data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 15, 2011           randerso  Initial creation
 * Jan 30, 2013  15719    jdynina   Allowed more than 128 chars in wx strings
 * Feb 19, 2013  1637     randerso  Added throws declarations to
 *                                  translateDataFrom
 * Apr 01, 2014  17187    randerso  (code checked in by zhao) To allow over 128
 *                                  wx elements
 * Apr 23, 2015  4259     njensen   Removed unused INumpyable
 * Aug 02, 2016  5744     mapeters  Remove unused cache code
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Changes to support IDataObject. Code cleanup
 * May 17, 2018  7286     dgilling  Improve error message in gridSet.
 * Jan 04, 2019  7705     randerso  Fix misspelled method name.
 *
 * </pre>
 *
 * @author randerso
 */

public class WeatherGridData extends AbstractGridData {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherGridData.class);

    private String siteId;

    /**
     * Constructor
     *
     * @param parm
     * @param slice
     * @param unsaved
     *            true if data is unsaved and must not be depopulated
     */
    public WeatherGridData(Parm parm, IGridSlice slice, boolean unsaved) {
        super(parm, slice, unsaved);
        this.siteId = parm.getParmID().getDbId().getSiteId();
        if (!(slice instanceof WeatherGridSlice)) {
            throw new IllegalArgumentException(
                    "slice must be an instance of WeatherGridSlice, received: "
                            + slice.getClass().getName());
        }
    }

    /**
     * Constructor, not for general use
     *
     * @param parm
     * @param dataObject
     */
    public WeatherGridData(Parm parm, WeatherDataObject dataObject) {
        super(parm, dataObject);
        this.siteId = parm.getParmID().getDbId().getSiteId();
    }

    /**
     * Copy constructor
     *
     * @param other
     */
    public WeatherGridData(WeatherGridData other) {
        super(other);
        this.siteId = other.siteId;
    }

    @Override
    public WeatherGridData copy() {
        return new WeatherGridData(this);
    }

    @Override
    protected Grid2DBit doSmooth(Date time, Grid2DBit pointsToSmooth) {
        Grid2DByte grid = getWeatherGrid();

        if ((grid.getXdim() != pointsToSmooth.getXdim())
                || (grid.getYdim() != pointsToSmooth.getYdim())) {
            statusHandler.handle(Priority.ERROR,
                    "Dimension mismatch in doSmooth: "
                            + getWeatherGrid().getXdim() + ','
                            + getWeatherGrid().getYdim() + ' '
                            + pointsToSmooth.getXdim() + ','
                            + pointsToSmooth.getYdim());
            return new Grid2DBit(grid.getXdim(), grid.getYdim());
        }

        Grid2DByte originalGrid;
        WeatherKey[] originalKeys;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            WeatherDataObject dataObject = (WeatherDataObject) p.getSecond()
                    .getDataObject();
            originalGrid = dataObject.getWeatherGrid();
            originalKeys = dataObject.getKeys();
        } else {
            originalGrid = getWeatherGrid().copy();
            originalKeys = getKeys();
        }

        Point ll = new Point();
        Point ur = new Point();
        int maxCount, maxIndex, same;
        short histo[] = new short[originalKeys.length]; // histogram

        // Get the smooth factor and divide by 2 for the loop
        int ss = getParm().getParmState().getSmoothSize() / 2;

        // check if points to smooth contains valid points, and get grid
        // limits
        if (pointsToSmooth.extremaOfSetBits(ll, ur)) {
            // for all points in the region of selected points
            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    // if this point is one to smooth
                    if (pointsToSmooth.getAsBoolean(i, j)) {

                        // There are many possible ways to do this.
                        // nine point average;
                        // uses fewer than nine points if near grid edge,
                        // but always has at least four values to average.
                        Arrays.fill(histo, (short) 0);
                        for (int newx = i - ss; newx <= (i + ss); newx++) {
                            for (int newy = j - ss; newy <= (j + ss); newy++) {
                                // if inside grid limits, make a
                                // smoothed value
                                if (originalGrid.isValid(newx, newy)) {
                                    histo[0xFF
                                            & originalGrid.get(newx, newy)]++;
                                }
                            }
                        }

                        // find the max occurrence and assign
                        maxCount = -1;
                        maxIndex = 0;
                        for (int k = 0; k < originalKeys.length; k++) {
                            if (histo[k] > maxCount) {
                                maxCount = histo[k];
                                maxIndex = k;
                            }
                        }

                        // check for need to combine (more than 1 at
                        // maxCount)
                        // if overlapping type of weather
                        same = 0;
                        for (int k = 0; k < originalKeys.length; k++) {
                            if (histo[k] == maxCount) {
                                same++;
                            }
                        }

                        if (same == 1) {
                            grid.set(i, j, (byte) maxIndex);
                        } else {
                            // make a combined key
                            WeatherKey ky = new WeatherKey(siteId,
                                    "<NoCov>:<NoWx>:<NoInten>:<NoVis>:");
                            for (int k = 0; k < originalKeys.length; k++) {
                                if (histo[k] == maxCount) {
                                    ky.addAll(originalKeys[k]);
                                }
                            }
                            byte index = lookupKeyValue(ky);
                            grid.set(i, j, index);
                        }
                    }
                }
            }
        }

        // return the points that were changed
        return pointsToSmooth;

    }

    @Override
    protected boolean translateDataFrom(IGridData sourceGrid)
            throws FactoryException, TransformException {
        if (!(sourceGrid instanceof WeatherGridData)) {
            throw new IllegalArgumentException(
                    "sourceGrid must be an instance of WeatherGridData, received: "
                            + sourceGrid.getClass().getName());
        }

        // simple case - no translation necessary - direct copy
        if (parm.getGridInfo().getGridLoc()
                .equals(sourceGrid.getParm().getGridInfo().getGridLoc())) {
            substituteDataObject(sourceGrid);
        }

        // complex case - translation is necessary
        else {
            // copy the key from the source to the destination
            setKeys(((WeatherDataObject) sourceGrid.getDataObject()).getKeys());

            // find no weather key, which is always the 1st one
            int nowx = 0;

            RemapGrid remap = new RemapGrid(
                    sourceGrid.getParm().getGridInfo().getGridLoc(),
                    getGridInfo().getGridLoc());
            setGrid(remap.remap(((WeatherDataObject) sourceGrid.getDataObject())
                    .getWeatherGrid(), (byte) 255, (byte) nowx));
        }
        return true;
    }

    @Override
    public boolean applyDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange) {
        throw new UnsupportedOperationException("Attempt to applyDelta: ");
    }

    @Override
    public WxValue getWxValue(int x, int y) {
        int index = 0xFF & getWeatherGrid().get(x, y);
        WeatherWxValue tmpWeatherWxValue = new WeatherWxValue(getKeys()[index],
                getParm());

        return tmpWeatherWxValue;
    }

    @Override
    public IDataObject gridMax(IDataObject dataObject) {
        throw new UnsupportedOperationException(
                "Attempt to gridMax: on a Weather Grid");
    }

    @Override
    public IDataObject gridMin(IDataObject dataObject) {
        throw new UnsupportedOperationException(
                "Attempt to gridMin: on a Weather Grid");
    }

    @Override
    public IDataObject gridMultiply(float factor) {
        throw new UnsupportedOperationException(
                "Attempt to gridMultiply: on a Weather Grid");
    }

    @Override
    public IDataObject gridSum(IDataObject dataObject) {
        throw new UnsupportedOperationException(
                "Attempt to gridSum: on a Weather Grid");
    }

    @Override
    protected Grid2DBit doContiguous(Date time, Point location) {
        Point size = getParm().getGridInfo().getGridLoc().gridSize();
        Grid2DBit valid = new Grid2DBit(size.x, size.y, true);

        Grid2DByte originalGrid;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            valid = p.getFirst();
            originalGrid = ((WeatherDataObject) p.getSecond().getDataObject())
                    .getWeatherGrid();
        } else {
            originalGrid = getWeatherGrid();
        }

        Grid2DBit contig = new Grid2DBit(size.x, size.y);
        byte value = originalGrid.get(location.x, location.y);

        Point ll = new Point();
        Point ur = new Point();

        // check if points to check contains valid points, and get grid limits
        if (valid.extremaOfSetBits(ll, ur)) {
            // for all points in the region of valid points
            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    // if this point is one to check
                    if (valid.getAsBoolean(i, j)
                            && (originalGrid.get(i, j) == value)) {
                        contig.set(i, j);
                    }
                }
            }
        }

        // return the points that were changed
        return contig.contiguousBitArray(location);
    }

    @Override
    protected Grid2DBit doPencilStretch(Date time, WxValue value,
            Coordinate[] path, Grid2DBit editArea) {
        Grid2DByte grid = getWeatherGrid();
        if ((grid.getXdim() != editArea.getXdim())
                || (grid.getYdim() != editArea.getYdim())) {
            statusHandler.handle(Priority.ERROR,
                    "Dimension mismatch in doPencilStretch: " + grid.getXdim()
                            + ',' + grid.getYdim() + ' ' + editArea.getXdim()
                            + ',' + editArea.getYdim());
            return new Grid2DBit(grid.getXdim(), grid.getYdim());
        }

        // Make a Grid2DBit defined by the area

        // convert path to polygons
        MultiPolygon mp = GfeUtil.createPolygon(path);

        // convert polygon to grid coordinates
        GridLocation gloc = this.parm.getGridInfo().getGridLoc();

        Grid2DBit editmask = null;
        try {
            mp = (MultiPolygon) JTS.transform(mp, MapUtil
                    .getTransformFromLatLon(PixelOrientation.CENTER, gloc));

            // create grid from polygon
            editmask = GfeUtil.filledBitArray(mp, gloc);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Error creating edit mask", e);
            editmask = new Grid2DBit(gloc.getNx(), gloc.getNy(), false);
        }

        // restrict the area of influence to the active edit area
        if (editArea.isAnyBitsSet()) {
            editmask.andEquals(editArea);
        }

        // And set new weather values over the region
        doSet(value, editmask);

        return editmask;
    }

    @Override
    public void set(Point gridLoc, WxValue wxValue) {
        throw new UnsupportedOperationException("Attempt to set: ");
    }

    /**
     * Compares a specified value to this grid using the specified op
     *
     * @param value
     * @param op
     * @return mask containing grid cells where comparison is true
     */
    public Grid2DBit comparisonOperate(String value, Op op) {
        Grid2DByte grid = getWeatherGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
        case NOT_EQ: {
            // Convert the textString to a WeatherKey
            WeatherKey dkey = new WeatherKey(siteId, value);
            if (!dkey.isValid()) {
                return bits;
            }
            bits = getDataObject().eq(dkey);
            if (op == Op.NOT_EQ) {
                bits.negate();
            }
            break;
        }
        case ALMOST:
            bits = getDataObject().almost(value);
            break;
        case NOT_ALMOST:
            bits = getDataObject().almost(value);
            bits.negate();
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in WeatherGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    /**
     * Compares a specified grid to this grid using the specified op
     *
     * @param gridData
     * @param op
     * @return mask containing grid cells where comparison is true
     */
    public Grid2DBit comparisonOperate(IGridData gridData, Op op) {
        if (!(gridData instanceof WeatherGridData)) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid gridData type in WeatherGridData::operate().");
            return new Grid2DBit();
        }

        WeatherDataObject wxDataObject = (WeatherDataObject) gridData
                .getDataObject();

        Grid2DByte grid = getWeatherGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
            bits = getDataObject().eq(wxDataObject);
            break;
        case NOT_EQ:
            bits = getDataObject().notEq(wxDataObject);
            break;
        case ALMOST:
            bits = getDataObject().almost(wxDataObject);
            break;
        case NOT_ALMOST:
            bits = getDataObject().almost(wxDataObject);
            bits.negate();
            break;
        default:
            statusHandler.handle(Priority.PROBLEM, "Invalid operator: " + op
                    + " in WeatherGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    /**
     * Compares a specified weather key to this grid using the specified op
     *
     * @param value
     * @param op
     * @return mask containing grid cells where comparison is true
     */
    public Grid2DBit comparisonOperate(WeatherKey value, Op op) {
        Grid2DByte grid = getWeatherGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        if (!value.isValid()) {
            statusHandler.handle(Priority.ERROR,
                    "Illegal weather key in doComparisonOperate()");
            return bits;
        }

        switch (op) {
        case EQ:
            bits = getDataObject().eq(value);
            break;
        case NOT_EQ:
            bits = getDataObject().notEq(value);
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in WeatherGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    /**
     * Performs the specified op on this grid using another weather grid and
     * edit area edit area
     *
     * @param gridData
     * @param op
     * @param refData
     * @return true if successful
     */
    public boolean operate(WeatherGridData gridData, Op op,
            ReferenceData refData) {
        if (gridData.getParm().getGridInfo()
                .getGridType() != GridType.WEATHER) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid gridData type in WeatherGridData::operate().");
            return false;
        }

        Grid2DBit bits = refData.getGrid();
        boolean didIt = false;
        switch (op) {
        case ASSIGN:
            didIt = getDataObject().assign((gridData.getDataObject()), bits);
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in WeatherGridData::operate.");
            break;
        }

        return didIt;
    }

    /**
     * Performs the specified op on this grid using a specified weather value
     *
     * @param value
     * @param op
     * @param refData
     * @return true if successful
     */
    public boolean operate(WxValue value, Op op, ReferenceData refData) {
        boolean result = false;
        switch (value.getParm().getGridInfo().getGridType()) {
        case WEATHER:
            result = operate(((WeatherWxValue) value).getWeatherKey(), op,
                    refData);
            break;

        default:
            statusHandler.handle(Priority.ERROR,
                    "Invalid WxValue type in WeatherGridData::operate().");
            break;
        }
        return result;
    }

    /**
     * Performs the specified op on this grid using a specified weather key
     *
     * @param value
     * @param op
     * @param refData
     * @return true if successful
     */
    public boolean operate(WeatherKey value, Op op, ReferenceData refData) {
        if (!value.isValid()) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid WeatherKey() for doOperate()");
            return false;
        }

        Grid2DBit bits = refData.getGrid();
        boolean didIt = false;
        switch (op) {
        case ASSIGN:
            didIt = getDataObject().assign(value, bits);
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in WeatherGridData::operate.");
            break;
        }

        return didIt;
    }

    @Override
    public Grid2DBit doSet(WxValue value, Grid2DBit pointsToSet) {
        GridType gridType = value.getParm().getGridInfo().getGridType();
        Grid2DByte weatherGrid = getWeatherGrid();
        int xDim = weatherGrid.getXdim();
        int yDim = weatherGrid.getYdim();

        if (gridType != GridType.WEATHER) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid WxValue type in WeatherGridData::doSet().");
            return new Grid2DBit(xDim, yDim); // unchanged
        }
        WeatherKey wk = ((WeatherWxValue) value).getWeatherKey();
        if (!wk.isValid()) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid weather key in doSet()");

            // unchanged
            return new Grid2DBit(xDim, yDim);
        }
        byte index = lookupKeyValue(wk);

        // If there are no points to change, return an empty Grid2DBit.
        // Otherwise find the corners of a rectangular area in the grid
        // enclosing all the points to change.
        Point ll = new Point();
        Point ur = new Point();
        if (!pointsToSet.extremaOfSetBits(ll, ur)) {
            return new Grid2DBit(xDim, yDim); // unchanged
        }

        // combine mode application
        if (CombineMode.COMBINE
                .equals(getParm().getParmState().getCombineMode())) {

            // fancy code in here to prevent lots of repeated combining
            // for efficiency.
            // Make an array of byte...init to MAX_VALUE
            byte newValues[] = new byte[255];
            Arrays.fill(newValues, (byte) -1);
            byte[] gridA = weatherGrid.getBuffer().array();
            byte[] pToSetA = pointsToSet.getBuffer().array();

            // Loop over the rows and columns that might have points to set
            for (int row = ll.y; row <= ur.y; row++) {
                int rowOffset = row * xDim;
                for (int col = ll.x; col <= ur.x; col++) {

                    if ((byte) 1 == pToSetA[rowOffset + col]) {
                        // pointsToSet selects this grid point
                        byte dataPoint = gridA[rowOffset + col];
                        int dataPointIdx = 0xFF & dataPoint;
                        if (dataPoint != index) {
                            // value needs to change
                            if (newValues[dataPointIdx] == (byte) -1) {
                                // new key hasn't been found
                                WeatherKey combinedKey = new WeatherKey(wk);
                                combinedKey.addAll(getKeys()[dataPointIdx]);

                                // Store new key index in lookup table
                                newValues[dataPointIdx] = lookupKeyValue(
                                        combinedKey);
                            }
                            // Update the grid
                            gridA[rowOffset + col] = newValues[dataPointIdx];
                        }
                    }
                }
            }
        } else {
            // replace mode application
            byte[] gridA = weatherGrid.getBuffer().array();
            byte[] pToSetA = pointsToSet.getBuffer().array();
            for (int row = ll.y; row <= ur.y; row++) {
                int rowOffset = row * xDim;
                for (int col = ll.x; col <= ur.x; col++) {
                    if ((byte) 1 == pToSetA[rowOffset + col]) {
                        gridA[rowOffset + col] = index;
                    }
                }
            }
        }
        setGrid(weatherGrid);

        return pointsToSet;
    }

    /**
     * Lookup index of a key. If not present it is added to the list of keys
     *
     * @param key
     * @return the index of the specified key
     */
    public byte lookupKeyValue(WeatherKey key) {
        // first check to see if it already is in the weather key
        int i = -1;
        WeatherKey keys[] = getKeys();
        for (int j = 0; j < keys.length; j++) {
            if (keys[j].equals(key)) {
                i = j;
            }
        }
        if (i != -1) {
            return (byte) i;
        }

        // not in weather key, must allocate a new entry
        WeatherKey keyArray[] = new WeatherKey[keys.length + 1];
        System.arraycopy(keys, 0, keyArray, 0, keys.length);
        keyArray[keyArray.length - 1] = key;
        setKeys(keyArray);
        return (byte) (getKeys().length - 1);
    }

    /**
     * After looking in four directions, returns the byte value for the first
     * point set in gridCells at the minimum distance of the four directions.
     *
     * -- implementation
     *
     * Make an array of coords that define the directions up, right, down, and
     * left. Next loop through all directions, searching for the byte value at
     * the first point not set in gridCells. If a set point is found that is
     * closer than the current minimum distance, then reset the byte value and
     * distance. Return true if a byte value as found. Return false if we bumped
     * into the edge of the grid before we found a byte value.
     *
     * @param startCoord
     * @param gridCells
     * @param value
     * @return
     */
    private boolean getMinDistEdgeValue(Point startCoord,
            final Grid2DBit gridCells, MutableByte value) {
        int direction, distance, minDistance = Integer.MAX_VALUE;
        boolean setIt = false;

        // Create and init the CartCoord array
        final int dirX[] = new int[] { 0, 1, 0, -1 };
        final int dirY[] = new int[] { 1, 0, -1, 0 };

        for (direction = 0; direction <= 3; direction++) {
            // Now search for the first set bit and get its value
            Point coord = startCoord;
            coord.x += dirX[direction];
            coord.y += dirY[direction];
            distance = 1;
            boolean done = false;
            while (!done) {
                // Check for bounds
                if ((coord.x < 0) || (coord.x >= gridCells.getXdim())
                        || (coord.y < 0) || (coord.y >= gridCells.getYdim())) {
                    done = true;
                }
                // Check for bit set
                else if (gridCells.getAsBoolean(coord.x, coord.y)) {
                    distance++;
                    coord.x += dirX[direction];
                    coord.y += dirY[direction];
                } else {
                    // We found one
                    if (distance < minDistance) {
                        Grid2DByte grid = getWeatherGrid();
                        value.setValue(grid.get(coord.x, coord.y));
                        minDistance = distance;
                        setIt = true;
                    }
                    done = true;
                }
            }
        }

        return setIt;
    }

    @Override
    protected Grid2DBit doDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange) {
        throw new UnsupportedOperationException("Attempt to pencilStretch: ");
    }

    @Override
    protected Grid2DBit doCopy(Date time, Grid2DBit pointsToCopy, Point delta) {
        Grid2DByte grid = getWeatherGrid();
        if ((grid.getXdim() != pointsToCopy.getXdim())
                || (grid.getYdim() != pointsToCopy.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToChange have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToCopy.getXdim(),
                    pointsToCopy.getYdim()));
        }

        // set up translation matrix
        // byte translate[] = new byte[128];
        byte translate[] = new byte[255];
        Arrays.fill(translate, (byte) -1);

        // get the grid
        Grid2DByte originalGrid;
        WeatherKey[] originalKeys;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            WeatherDataObject dataObject = (WeatherDataObject) p.getSecond()
                    .getDataObject();
            originalGrid = dataObject.getWeatherGrid();
            originalKeys = dataObject.getKeys();
        } else {
            originalGrid = getWeatherGrid().copy();
            originalKeys = getKeys();
        }

        Point ll = new Point();
        Point ur = new Point();
        int newx, newy, i, j;

        // For each point in the set of selected points, copy original
        // grid's point value to working grid, using offset.

        // if points to copy contains valid points, get grid limits ll and ur.
        if (pointsToCopy.extremaOfSetBits(ll, ur)) {
            // modify some points in the region of selected points
            for (j = ll.y; j <= ur.y; j++) {
                newy = j + delta.y;
                for (i = ll.x; i <= ur.x; i++) {
                    // if this point is one to copy
                    if (pointsToCopy.getAsBoolean(i, j)) {
                        // determine the new position
                        newx = i + delta.x;

                        // if inside grid limits, copy value to new position
                        // of working grid.
                        if (grid.isValid(newx, newy)) {
                            // byte og = originalGrid.get(i, j);
                            int og = 0xFF & originalGrid.get(i, j);
                            byte v = translate[og];
                            if (v == -1) {
                                v = lookupKeyValue(originalKeys[og]);
                                translate[og] = v;
                            }
                            grid.set(newx, newy, og);
                        }
                    }
                }
            }
            setGrid(grid);
        }

        return pointsToCopy.translate(delta);

    }

    @Override
    protected Grid2DBit doFillIn(Date time, Grid2DBit pointsToFillIn) {
        Point lowerLeft = new Point();
        Point upperRight = new Point();
        Grid2DByte grid = getWeatherGrid();
        if (!pointsToFillIn.extremaOfSetBits(lowerLeft, upperRight)) {
            return new Grid2DBit(grid.getXdim(), grid.getYdim());
        }

        MutableByte value = new MutableByte();
        for (int i = lowerLeft.x; i <= upperRight.x; i++) {
            for (int j = lowerLeft.y; j <= upperRight.y; j++) {
                if (pointsToFillIn.getAsBoolean(i, j)) {
                    if (getMinDistEdgeValue(new Point(i, j), pointsToFillIn,
                            value)) {
                        grid.set(i, j, value.byteValue());
                    }
                }
            }
        }
        setGrid(grid);

        return pointsToFillIn;
    }

    @Override
    public void setDataObject(IDataObject dataObject) {
        if (!(dataObject instanceof WeatherDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of WeatherDataObject, received: "
                            + dataObject.getClass().getName());
        }
        super.setDataObject(dataObject);
    }

    /**
     * Sets multiple (weather) values in the grid.
     *
     * @param values
     * @param keys
     * @param pointsToChange
     */
    public void set(Grid2DByte values, List<WeatherKey> keys,
            Grid2DBit pointsToChange) {
        checkOkayForEdit();

        gridSet(values, keys, pointsToChange);
        setChangedPoints(pointsToChange);
    }

    protected void gridSet(Grid2DByte values, List<WeatherKey> keys,
            Grid2DBit pointsToChange) {
        Grid2DByte grid = getWeatherGrid();
        Point dim = new Point(grid.getXdim(), grid.getYdim());
        if ((values.getXdim() != dim.x) || (values.getYdim() != dim.y)) {
            throw new IllegalArgumentException(String.format(
                    "This grid and the supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    dim.x, dim.y, values.getXdim(), values.getYdim()));
        }

        if ((pointsToChange.getXdim() != dim.x)
                || (pointsToChange.getYdim() != dim.y)) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToChange have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    dim.x, dim.y, pointsToChange.getXdim(),
                    pointsToChange.getYdim()));
        }

        // ensure values doesn't exceed keys, and keys are good
        for (WeatherKey wKey : keys) {
            if (!wKey.isValid()) {
                throw new IllegalArgumentException(
                        "Invalid weather key in keys: " + wKey.getOrigStr());
            }
        }
        // search for grid out of bounds
        int numValues = values.getXdim() * values.getYdim();
        byte[] bp = values.getBuffer().array();
        for (int i = 0; i < numValues; i++) {
            if ((0xFF & bp[i]) > (keys.size() - 1)) {
                throw new IndexOutOfBoundsException(String.format(
                        "Invalid index in values[%d,%d]. Index: %d, Size: %d",
                        i % values.getXdim(), i / values.getXdim(),
                        (0XFF & bp[i]), keys.size()));
            }
        }

        // REPLACE mode is easy
        if (parm.getParmState()
                .getCombineMode() == ParmState.CombineMode.REPLACE) {
            // create remap array
            byte[] remap = new byte[256];
            for (int i = 0; i < keys.size(); i++) {
                remap[i] = lookupKeyValue(keys.get(i));
            }
            // key
            for (int i = 0; i < dim.x; i++) {
                for (int j = 0; j < dim.y; j++) {
                    if (pointsToChange.getAsBoolean(i, j)) {
                        grid.set(i, j, remap[0xFF & values.get(i, j)]);
                    }
                }
            }
        }
        // COMBINE mode is more difficult, have to do each one
        else {
            for (int i = 0; i < dim.x; i++) {
                for (int j = 0; j < dim.y; j++) {
                    if (pointsToChange.get(i, j) == 1) {
                        WeatherKey combined = new WeatherKey(
                                keys.get(0xFF & values.get(i, j)));
                        combined.addAll(doGetWeatherValue(i, j));
                        grid.set(i, j, lookupKeyValue(combined));
                    }
                }
            }
        }
    }

    protected WeatherKey doGetWeatherValue(int x, int y) {
        byte gridValue = getWeatherGrid().get(x, y);
        int gridValueIdx = 0xFF & gridValue;
        return getKeys()[gridValueIdx];
    }

    @Override
    public boolean isSupportedEditOp(EditOp editOp) {
        // Delta and Smooth operations are not supported at the current time.
        // NOTE: original AWIPS 1 comment said Delta and Smooth not supported,
        // but code actually returned false for Delta and Contour
        switch (editOp) {
        case SET:
        case MOVE_COPY:
        case SMOOTH:
        case FILLINHOLE:
        case PENCILSTRETCH:
            return true;

        case DELTA:
        case CONTOUR:
            return false;

        default:
            statusHandler.handle(Priority.PROBLEM,
                    "Unsupported EditOp: " + editOp.name());
            return false;
        }
    }

    @Override
    public synchronized WeatherDataObject getDataObject() {
        return (WeatherDataObject) super.getDataObject();
    }

    private Grid2DByte getWeatherGrid() {
        return getDataObject().getWeatherGrid();
    }

    private void setGrid(Grid2DByte grid) {
        WeatherDataObject dataObject = getDataObject();
        dataObject.setWeatherGrid(grid);
        setDataObject(dataObject);
    }

    private WeatherKey[] getKeys() {
        return getDataObject().getKeys();
    }

    private void setKeys(WeatherKey[] keys) {
        WeatherDataObject dataObject = getDataObject();
        dataObject.setKeys(keys);
        setDataObject(dataObject);
    }

    @Override
    protected boolean doValid() {
        String emsg = "Grid contains data which exceeds limits for this parm. ";

        if (!getGridTime().isValid() || (getParm() == null)
                || (getDataObject() == null)) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid grid time, bad parm or data slice");
            // time, parm, or data slice not valid
            return false;
        }

        // check grid size
        Point dim = getParm().getGridInfo().getGridLoc().gridSize();
        Grid2DByte grid = getWeatherGrid();
        Point gridDim = grid.getGridSize();
        if (!gridDim.equals(dim)) {
            statusHandler.handle(Priority.PROBLEM, "Grid dimensions " + gridDim
                    + " do not match Parm dimensions " + dim);
            return false;
        }

        // check data values
        byte[] data = grid.getBuffer().array();
        WeatherKey[] keys = getKeys();
        // byte keySize = (byte) keys.length;
        int keySize = keys.length;

        for (int j = 0; j < data.length; j++) {
            int value = 0xFF & data[j];
            if (value > keySize) {
                statusHandler.handle(Priority.PROBLEM,
                        emsg + "Data=" + value + " Min=0 Max=" + keySize);
                return false;
            }
        }

        // check key validity, then check for weather key validity
        for (int i = 0; i < keys.length; i++) {
            if (!keys[i].isValid()) {
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid weather key: " + keys[i]);
                return false;
            }
        }

        return true;
    }

    @Override
    protected IGridSlice createSlice() {
        return new WeatherGridSlice(getGridTime(), getGridInfo(), getHistory(),
                getWeatherGrid(), getKeys());
    }

    @Override
    protected String doValidateData(IDataObject dataObject) {
        WeatherDataObject wdo = (WeatherDataObject) dataObject;

        String retVal = wdo.checkDims(gridParmInfo.getGridLoc().getNx(),
                gridParmInfo.getGridLoc().getNy());

        if (retVal == null) {
            retVal = wdo.checkKey();
        }

        if (retVal == null) {
            retVal = wdo.checkKeyAndData();
        }

        return retVal;
    }
}
