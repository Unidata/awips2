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
import java.util.ArrayList;
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
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.parm.ParmState.CombineMode;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.MultiPolygon;

/**
 * Grid containing discrete data.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Apr 29, 2008           dfitch      Initial creation.
 * Jun 17, 2008           njensen     Added set() and gridSet()
 * Aug 05, 2008  1383     ebabin      Fix for time shift not working.
 * Nov 06, 2008  1591     wdougherty  Fix isValid() so it can return true Tweak
 *                                    doSet() for filtered grids, fix bugs
 * Jan 30, 2013  15719    jdynina     Fixed allowed field size to accept more
 *                                    than 128 characters
 * Feb 19, 2013  1637     randerso    Added throws declarations to
 *                                    translateDataFrom
 * Oct 31, 2013  2508     randerso    Change to use DiscreteGridSlice.getKeys()
 * Apr 23, 2015  4259     njensen     Removed unused INumpyable
 * Apr 04, 2016  5539     randerso    Fix unsigned byte issues
 * Aug 02, 2016  5744     mapeters    Remove unused cache code
 * Dec 13, 2017  7178     randerso    Code formatting and cleanup
 * Jan 04, 2018  7178     randerso    Changes to support IDataObject. Code
 *                                    cleanup
 * Jan 04, 2019  7705     randerso    Fix misspelled method name.
 *
 * </pre>
 *
 * @author chammack
 */
public class DiscreteGridData extends AbstractGridData {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteGridData.class);

    /**
     * Constructor
     *
     * @param parm
     * @param slice
     * @param unsaved
     *            true if data is unsaved and must not be depopulated
     */
    public DiscreteGridData(Parm parm, IGridSlice slice, boolean unsaved) {
        super(parm, slice, unsaved);
        if (!(slice instanceof DiscreteGridSlice)) {
            throw new IllegalArgumentException(
                    "slice must be an instance of DiscreteGridSlice, received: "
                            + slice.getClass().getName());
        }
    }

    /**
     * Constructor, not for general use
     *
     * @param parm
     * @param dataObject
     */
    public DiscreteGridData(Parm parm, DiscreteDataObject dataObject) {
        super(parm, dataObject);
    }

    /**
     * Copy constructor
     *
     * @param other
     */
    public DiscreteGridData(DiscreteGridData other) {
        super(other);
    }

    @Override
    public DiscreteGridData copy() {
        return new DiscreteGridData(this);
    }

    @Override
    protected Grid2DBit doSmooth(Date time, Grid2DBit pointsToSmooth) {
        Grid2DByte grid = getDiscreteGrid();

        if ((grid.getXdim() != pointsToSmooth.getXdim())
                || (grid.getYdim() != pointsToSmooth.getYdim())) {
            statusHandler.error(
                    "Dimension mismatch in doSmooth: " + grid.getXdim() + ','
                            + grid.getYdim() + ' ' + pointsToSmooth.getXdim()
                            + ',' + pointsToSmooth.getYdim());
            return new Grid2DBit(grid.getXdim(), grid.getYdim());
        }

        Grid2DByte originalGrid;
        DiscreteKey[] originalKeys;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            DiscreteDataObject dataObject = (DiscreteDataObject) p.getSecond()
                    .getDataObject();
            originalGrid = dataObject.getDiscreteGrid();
            originalKeys = dataObject.getKeys();
        } else {
            originalGrid = getDiscreteGrid().copy();
            originalKeys = getKeys();
        }

        Point ll = new Point();
        Point ur = new Point();
        int maxCount, maxIndex, same;
        short histo[] = new short[originalKeys.length]; // histogram

        // Get the smooth factor and divide by 2 for the loop
        int ss = getParm().getParmState().getSmoothSize() / 2;

        // overlapping discrete?
        ParmID parmId = getParm().getParmID();
        String siteId = parmId.getDbId().getSiteId();
        boolean overlapping = DiscreteKey.discreteDefinition(siteId)
                .overlaps(parmId.getCompositeName());

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
                                // if inside grid limits, make a smoothed value
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

                        // check for need to combine (more than 1 at maxCount)
                        // if overlapping type of discrete
                        if (overlapping) {
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
                                List<String> subKeys = new ArrayList<>();
                                for (int k = 0; k < originalKeys.length; k++) {
                                    if (histo[k] == maxCount) {
                                        subKeys.addAll(
                                                originalKeys[k].getSubKeys());
                                    }
                                }

                                // allocate the key
                                DiscreteKey ky = new DiscreteKey(siteId,
                                        subKeys, parmId);
                                byte index = lookupKeyValue(ky);
                                grid.set(i, j, index);
                            }
                        } else {
                            // non-overlapping
                            grid.set(i, j, (byte) maxIndex);
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
        if (!(sourceGrid instanceof DiscreteGridData)) {
            throw new IllegalArgumentException(
                    "sourceGrid must be an instance of DiscreteGridData, received: "
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
            setKey(((DiscreteDataObject) sourceGrid.getDataObject()).getKeys());

            // find no discrete key, which is always the 1st one
            int nowx = 0;

            RemapGrid remap = new RemapGrid(
                    sourceGrid.getParm().getGridInfo().getGridLoc(),
                    getGridInfo().getGridLoc());
            setGrid(remap
                    .remap(((DiscreteDataObject) sourceGrid.getDataObject())
                            .getDiscreteGrid(), (byte) 255, (byte) nowx));
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
        int index = 0xFF & getDiscreteGrid().get(x, y);
        DiscreteWxValue tmpDiscreteWxValue = new DiscreteWxValue(
                getKeys()[index], getParm());

        return tmpDiscreteWxValue;
    }

    @Override
    public IDataObject gridMax(IDataObject dataObject) {
        throw new UnsupportedOperationException(
                "Attempt to gridMax: on a Discrete Grid");
    }

    @Override
    public IDataObject gridMin(IDataObject dataObject) {
        throw new UnsupportedOperationException(
                "Attempt to gridMin: on a Discrete Grid");
    }

    @Override
    public IDataObject gridMultiply(float factor) {
        throw new UnsupportedOperationException(
                "Attempt to gridMultiply: on a Discrete Grid");
    }

    @Override
    public IDataObject gridSum(IDataObject dataObject) {
        throw new UnsupportedOperationException(
                "Attempt to gridSum: on a Discrete Grid");
    }

    @Override
    protected Grid2DBit doContiguous(Date time, Point location) {
        Point size = getParm().getGridInfo().getGridLoc().gridSize();
        Grid2DBit valid = new Grid2DBit(size.x, size.y, true);

        Grid2DByte originalGrid;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            valid = p.getFirst();
            originalGrid = ((DiscreteDataObject) p.getSecond().getDataObject())
                    .getDiscreteGrid();
        } else {
            originalGrid = getDiscreteGrid();
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
        Grid2DByte grid = getDiscreteGrid();
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

        Grid2DBit editMask = null;
        try {
            mp = (MultiPolygon) JTS.transform(mp, MapUtil
                    .getTransformFromLatLon(PixelOrientation.CENTER, gloc));

            // create grid from polygon
            editMask = GfeUtil.filledBitArray(mp, gloc);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Error creating edit mask", e);
            editMask = new Grid2DBit(gloc.getNx(), gloc.getNy(), false);
        }

        // restrict the area of influence to the active edit area
        if (editArea.isAnyBitsSet()) {
            editMask.andEquals(editArea);
        }

        // And set new discrete values over the region
        doSet(value, editMask);

        return editMask;
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
        Grid2DByte grid = getDiscreteGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
        case NOT_EQ: {
            // Convert the textString to a DiscreteKey
            ParmID parmId = getParm().getParmID();
            String siteId = parmId.getDbId().getSiteId();
            DiscreteKey dkey = new DiscreteKey(siteId, value, parmId);
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
                    + " in DiscreteGridData::comparisonOperate.");
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
        if (!(gridData instanceof DiscreteGridData)) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid gridData type in DiscreteGridData::operate().");
            return new Grid2DBit();
        }

        DiscreteDataObject discreteDataObject = (DiscreteDataObject) gridData
                .getDataObject();

        Grid2DByte grid = getDiscreteGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
            bits = getDataObject().eq(discreteDataObject);
            break;
        case NOT_EQ:
            bits = getDataObject().notEq(discreteDataObject);
            break;
        case ALMOST:
            bits = getDataObject().almost(discreteDataObject);
            break;
        case NOT_ALMOST:
            bits = getDataObject().almost(discreteDataObject);
            bits.negate();
            break;
        default:
            statusHandler.handle(Priority.PROBLEM, "Invalid operator: " + op
                    + " in DiscreteGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    /**
     * Compares a specified discrete key to this grid using the specified op
     *
     * @param value
     * @param op
     * @return mask containing grid cells where comparison is true
     */
    public Grid2DBit comparisonOperate(DiscreteKey value, Op op) {
        Grid2DByte grid = getDiscreteGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        if (!value.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Illegal discrete key in doComparisonOperate()");
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
                    + " in DiscreteGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    /**
     * Performs the specified op on this grid using another discrete grid and
     * edit area edit area
     *
     * @param gridData
     * @param op
     * @param refData
     * @return true if successful
     */
    public boolean operate(DiscreteGridData gridData, Op op,
            ReferenceData refData) {
        if (gridData.getParm().getGridInfo()
                .getGridType() != GridType.DISCRETE) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid gridData type in DiscreteGridData::operate().");
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
                    + " in DiscreteGridData::operate.");
            break;
        }

        return didIt;
    }

    /**
     * Performs the specified op on this grid using a specified discrete value
     *
     * @param value
     * @param op
     * @param refData
     * @return true if successful
     */
    public boolean operate(WxValue value, Op op, ReferenceData refData) {
        boolean result = false;
        switch (value.getParm().getGridInfo().getGridType()) {
        case DISCRETE:
            result = operate(((DiscreteWxValue) value).getDiscreteKey(), op,
                    refData);
            break;

        default:
            statusHandler.handle(Priority.ERROR,
                    "Invalid WxValue type in DiscreteGridData::operate().");
            break;
        }
        return result;
    }

    /**
     * Performs the specified op on this grid using a specified discrete key
     *
     * @param value
     * @param op
     * @param refData
     * @return true if successful
     */
    public boolean operate(DiscreteKey value, Op op, ReferenceData refData) {
        if (!value.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid DiscreteKey() for doOperate()");
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
                    + " in DiscreteGridData::operate.");
            break;
        }

        return didIt;
    }

    @Override
    public Grid2DBit doSet(WxValue value, Grid2DBit pointsToSet) {
        GridType gridType = value.getParm().getGridInfo().getGridType();
        Grid2DByte discreteGrid = getDiscreteGrid();
        int xDim = discreteGrid.getXdim();
        int yDim = discreteGrid.getYdim();
        if (gridType != GridType.DISCRETE) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid WxValue type in DiscreteGridData::doSet().");

            // unchanged
            return new Grid2DBit(xDim, yDim);
        }

        DiscreteKey dk = ((DiscreteWxValue) value).getDiscreteKey();
        if (!dk.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid discrete key in doSet()");

            // unchanged
            return new Grid2DBit(xDim, yDim);
        }
        byte index = lookupKeyValue(dk);

        // If there are no points to change, return an empty Grid2DBit.
        // Otherwise find the corners of a rectangular area in the grid
        // enclosing all the points to change.
        Point ll = new Point();
        Point ur = new Point();
        if (!pointsToSet.extremaOfSetBits(ll, ur)) {
            return new Grid2DBit(xDim, yDim); // unchanged
        }

        ParmID parmId = getParm().getParmID();
        String siteId = parmId.getDbId().getSiteId();
        boolean overlapping = DiscreteKey.discreteDefinition(siteId)
                .overlaps(parmId.getCompositeName());

        // combine mode application
        if (CombineMode.COMBINE.equals(
                getParm().getParmState().getCombineMode()) && overlapping) {
            // convert the WxValue into a DiscreteKey
            // the discrete value from WxValue

            // fancy code in here to prevent lots of repeated combining
            // for efficiency.
            // Make an array of byte...init to MAX_VALUE
            byte newValues[] = new byte[255];
            Arrays.fill(newValues, (byte) -1);
            byte[] gridA = discreteGrid.getBuffer().array();
            byte[] pToSetA = pointsToSet.getBuffer().array();

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
                                DiscreteKey combinedKey = DiscreteKey
                                        .combine(dk, getKeys()[dataPointIdx]);

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
            byte[] gridA = discreteGrid.getBuffer().array();
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
        setGrid(discreteGrid);

        return pointsToSet;
    }

    /**
     * Lookup index of a key. If not present it is added to the list of keys
     *
     * @param key
     * @return the index of the specified key
     */
    public byte lookupKeyValue(DiscreteKey key) {
        // first check to see if it already is in the discrete key
        int i = -1;
        DiscreteKey keys[] = getKeys();
        for (int j = 0; j < keys.length; j++) {
            if (keys[j].equals(key)) {
                i = j;
            }
        }
        if (i != -1) {
            return (byte) i;
        }

        // not in discrete key, must allocate a new entry
        DiscreteKey keyArray[] = new DiscreteKey[keys.length + 1];
        System.arraycopy(keys, 0, keyArray, 0, keys.length);
        keyArray[keyArray.length - 1] = key;
        setKey(keyArray);
        return (byte) (getKeys().length - 1);
    }

    @Override
    protected Grid2DBit doDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange) {
        throw new UnsupportedOperationException("Attempt to pencilStretch: ");
    }

    @Override
    protected Grid2DBit doCopy(Date time, Grid2DBit pointsToCopy, Point delta) {
        Grid2DByte grid = getDiscreteGrid();
        if ((grid.getXdim() != pointsToCopy.getXdim())
                || (grid.getYdim() != pointsToCopy.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToChange have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToCopy.getXdim(),
                    pointsToCopy.getYdim()));
        }

        // set up translation matrix
        byte translate[] = new byte[255];
        Arrays.fill(translate, (byte) -1);

        Grid2DByte originalGrid;
        DiscreteKey[] originalKeys;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            DiscreteDataObject dataObject = (DiscreteDataObject) p.getSecond()
                    .getDataObject();
            originalGrid = dataObject.getDiscreteGrid();
            originalKeys = dataObject.getKeys();
        } else {
            originalGrid = getDiscreteGrid().copy();
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
                            grid.set(newx, newy, v);
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
        Grid2DByte grid = getDiscreteGrid();
        if ((grid.getXdim() != pointsToFillIn.getXdim())
                || (grid.getYdim() != pointsToFillIn.getYdim())) {
            statusHandler.handle(Priority.ERROR,
                    "Dimension mismatch in doFillIn: " + grid.getXdim() + ','
                            + grid.getYdim() + ' ' + pointsToFillIn.getXdim()
                            + ',' + pointsToFillIn.getYdim());
            return new Grid2DBit();
        }

        Point lowerLeft = new Point();
        Point upperRight = new Point();
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

    // -- private
    // ----------------------------------------------------------------
    // DiscreteGridData::getMinDistEdgeValue()
    // After looking in four directions, returns the byte value for the first
    // point set in gridCells at the minimum distance of the four directions.
    // -- implementation
    // ---------------------------------------------------------
    // Make an array of coords that define the directions up, right, down, and
    // left. Next loop through all directions, searching for the byte value at
    // the first point not set in gridCells. If a set point is found that
    // is closer than the current minimum distance, then reset the byte value
    // and distance. Return true if a byte value as found. Return false if we
    // bumped into the edge of the grid before we found a byte value.
    // ---------------------------------------------------------------------------
    private boolean getMinDistEdgeValue(Point startCoord, Grid2DBit gridCells,
            MutableByte value) {
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
                        Grid2DByte grid = getDiscreteGrid();
                        value.setValue(grid.get(coord.x, coord.y));
                        minDistance = distance;
                        setIt = true;
                    }
                    done = true;
                }
            } // while
        } // for

        return setIt;
    }

    @Override
    public void setDataObject(IDataObject dataObject) {
        if (!(dataObject instanceof DiscreteDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of DiscreteDataObject, received: "
                            + dataObject.getClass().getName());
        }
        super.setDataObject(dataObject);
    }

    /**
     * Sets multiple (discrete) values in the grid.
     *
     * @param values
     * @param keys
     * @param pointsToChange
     */
    public void set(Grid2DByte values, List<DiscreteKey> keys,
            Grid2DBit pointsToChange) {
        checkOkayForEdit();

        gridSet(values, keys, pointsToChange);
        setChangedPoints(pointsToChange);
    }

    protected void gridSet(Grid2DByte values, List<DiscreteKey> keys,
            Grid2DBit pointsToChange) {
        Grid2DByte grid = getDiscreteGrid();
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
        for (DiscreteKey dKey : keys) {
            if (!dKey.isValid()) {
                throw new IllegalArgumentException(
                        "Invalid discrete key in keys: " + dKey);
            }
        }

        // search for grid out of bounds
        int numValues = values.getXdim() * values.getYdim();
        byte[] bp = values.getBuffer().array();
        for (int i = 0; i < numValues; i++) {
            if ((0xFF & bp[i]) >= keys.size()) {
                throw new IndexOutOfBoundsException(String.format(
                        "Invalid index in values[%d,%d]. Index: %d, Size: %d",
                        i % values.getXdim(), i / values.getXdim(),
                        (0XFF & bp[i]), keys.size()));
            }
        }

        ParmID parmId = getParm().getParmID();
        String siteId = parmId.getDbId().getSiteId();
        boolean overlapping = DiscreteKey.discreteDefinition(siteId)
                .overlaps(parmId.getCompositeName());

        // REPLACE mode is easy
        if ((parm.getParmState()
                .getCombineMode() == ParmState.CombineMode.REPLACE)
                || !overlapping) {
            // create remap array
            byte[] remap = new byte[256];
            for (int i = 0; i < keys.size(); i++) {
                remap[i] = lookupKeyValue(keys.get(i));
            }
            // key
            for (int i = 0; i < dim.x; i++) {
                for (int j = 0; j < dim.y; j++) {
                    if (pointsToChange.get(i, j) == 1) {
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
                        DiscreteKey combined = DiscreteKey.combine(
                                keys.get(0xFF & values.get(i, j)),
                                doGetDiscreteValue(i, j));
                        grid.set(i, j, lookupKeyValue(combined));
                    }
                }
            }
        }

        setGrid(grid);
    }

    protected DiscreteKey doGetDiscreteValue(int x, int y) {
        byte gridValue = getDiscreteGrid().get(x, y);
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
    public synchronized DiscreteDataObject getDataObject() {
        return (DiscreteDataObject) super.getDataObject();
    }

    private Grid2DByte getDiscreteGrid() {
        return getDataObject().getDiscreteGrid();
    }

    private void setGrid(Grid2DByte grid) {
        DiscreteDataObject dataObject = getDataObject();
        dataObject.setDiscreteGrid(grid);
        setDataObject(dataObject);
    }

    private DiscreteKey[] getKeys() {
        return getDataObject().getKeys();
    }

    private void setKey(DiscreteKey[] key) {
        DiscreteDataObject dataObject = getDataObject();
        dataObject.setKeys(key);
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
        Grid2DByte grid = getDiscreteGrid();
        Point gridDim = grid.getGridSize();
        if (!gridDim.equals(dim)) {
            statusHandler.handle(Priority.PROBLEM, "Grid dimensions " + gridDim
                    + " do not match Parm dimensions " + dim);
            return false;
        }

        // check data values
        byte[] data = grid.getBuffer().array();
        DiscreteKey[] keys = getKeys();
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

        // check key validity, then check for discrete key validity
        for (int i = 0; i < keys.length; i++) {
            if (!keys[i].isValid()) {
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid discrete key: " + keys[i]);
                return false;
            }
        }

        return true;
    }

    @Override
    protected IGridSlice createSlice() {
        return new DiscreteGridSlice(getGridTime(), getGridInfo(), getHistory(),
                getDiscreteGrid(), getKeys());
    }

    @Override
    protected String doValidateData(IDataObject dataObject) {
        DiscreteDataObject ddo = (DiscreteDataObject) dataObject;

        String retVal = ddo.checkDims(gridParmInfo.getGridLoc().getNx(),
                gridParmInfo.getGridLoc().getNy());

        if (retVal == null) {
            retVal = ddo.checkKey();
        }

        if (retVal == null) {
            retVal = ddo.checkKeyAndData();
        }

        return retVal;
    }
}
