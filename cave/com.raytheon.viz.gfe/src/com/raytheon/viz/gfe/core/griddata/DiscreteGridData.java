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

import jep.INumpyable;

import org.apache.commons.lang.mutable.MutableByte;
import org.geotools.geometry.jts.JTS;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
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
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.parm.ParmState.CombineMode;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * Ported discrete grid data implementation
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/29/2008              dfitch      Initial creation.
 * 06/17/2008              njensen     Added set() and gridSet()
 * 05Aug2008    #1383       ebabin      Fix for time shift not working.
 * 06Nov2008    #1591      wdougherty  Fix isValid() so it can return true
 *                                     Tweak doSet() for filtered grids, fix bugs
 * 30Jan2013    #15719     jdynina     Fixed allowed field size to accept more
 *                                     than 128 characters
 * 02/19/2013   1637       randerso    Added throws declarations to translateDataFrom
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class DiscreteGridData extends AbstractGridData implements INumpyable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteGridData.class);

    public DiscreteGridData(Parm aParm, IGridSlice aSlice) {
        super(aParm, aSlice);
        if (!(aSlice instanceof DiscreteGridSlice)) {
            throw new IllegalArgumentException(
                    "DiscreteGridSlice required for DiscreteGridData");
        }
    }

    @Override
    public DiscreteGridData clone() throws CloneNotSupportedException {
        DiscreteGridData agd = new DiscreteGridData(this.parm,
                this.gridSlice.clone());
        return agd;
    }

    @Override
    protected Grid2DBit doSmooth(Date time, Grid2DBit pointsToSmooth) {
        DiscreteGridSlice thisSlice = getDiscreteSlice();
        Grid2DByte grid = thisSlice.getDiscreteGrid();

        if (grid.getXdim() != pointsToSmooth.getXdim()
                || grid.getYdim() != pointsToSmooth.getYdim()) {
            statusHandler.handle(
                    Priority.ERROR,
                    "Dimension mismatch in doSmooth: " + getGrid().getXdim()
                            + ',' + getGrid().getYdim() + ' '
                            + pointsToSmooth.getXdim() + ','
                            + pointsToSmooth.getYdim());
            return new Grid2DBit(getGrid().getXdim(), getGrid().getYdim());
        }

        Grid2DByte originalGrid;
        DiscreteKey[] originalKey;
        try {
            DiscreteGridSlice slice = thisSlice.clone();
            if (iscMode()) {
                getISCGrid(time, slice);
            }
            originalGrid = slice.getDiscreteGrid();
            originalKey = slice.getKey();
        } catch (CloneNotSupportedException e) {
            originalGrid = new Grid2DByte();
            originalKey = new DiscreteKey[0];
        }

        Point ll = new Point();
        Point ur = new Point();
        int maxCount, maxIndex, same;
        short histo[] = new short[originalKey.length]; // histogram

        // Get the smooth factor and divide by 2 for the loop
        int ss = getParm().getParmState().getSmoothSize() / 2;

        // overlapping discrete?
        ParmID parmId = getParm().getParmID();
        String siteId = parmId.getDbId().getSiteId();
        boolean overlapping = DiscreteKey.discreteDefinition(siteId).overlaps(
                parmId.getCompositeName());

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
                        for (int newx = i - ss; newx <= i + ss; newx++) {
                            for (int newy = j - ss; newy <= j + ss; newy++) {
                                // if inside grid limits, make a smoothed value
                                if (originalGrid.isValid(newx, newy)) {
                                    histo[0xFF & originalGrid.get(newx, newy)]++;
                                }
                            }
                        }

                        // find the max occurrence and assign
                        maxCount = -1;
                        maxIndex = 0;
                        for (int k = 0; k < originalKey.length; k++) {
                            if (histo[k] > maxCount) {
                                maxCount = histo[k];
                                maxIndex = k;
                            }
                        }

                        // check for need to combine (more than 1 at maxCount)
                        // if overlapping type of discrete
                        if (overlapping) {
                            same = 0;
                            for (int k = 0; k < originalKey.length; k++) {
                                if (histo[k] == maxCount) {
                                    same++;
                                }
                            }

                            if (same == 1) {
                                grid.set(i, j, (byte) maxIndex);
                            } else {
                                // make a combined key
                                List<String> subKeys = new ArrayList<String>();
                                for (int k = 0; k < originalKey.length; k++) {
                                    if (histo[k] == maxCount) {
                                        subKeys.addAll(originalKey[k]
                                                .getSubKeys());
                                    }
                                }

                                // allocate the key
                                DiscreteKey ky = new DiscreteKey(siteId,
                                        subKeys, parmId);
                                byte index = lookupKeyValue(ky);
                                grid.set(i, j, index);
                            }
                        } else {// non-overlapping
                            grid.set(i, j, (byte) maxIndex);
                        }
                    }
                }
            }
        }

        thisSlice.setDiscreteGrid(grid);

        // return the points that were changed
        return pointsToSmooth;

    }

    @Override
    protected boolean translateDataFrom(IGridData sourceGrid)
            throws FactoryException, TransformException {
        if (!(sourceGrid instanceof DiscreteGridData)) {
            throw new IllegalArgumentException(
                    "Expected DiscreteGridData as source.");
        }

        // simple case - no translation necessary - direct copy
        if (parm.getGridInfo().getGridLoc()
                .equals(sourceGrid.getParm().getGridInfo().getGridLoc())) {
            substitudeDS(sourceGrid.getGridSlice());
        }

        // complex case - translation is necessary
        else {
            // copy the key from the source to the destination
            setKey(((DiscreteGridSlice) sourceGrid.getGridSlice()).getKey());

            // find no discrete key, which is always the 1st one
            int nowx = 0;

            RemapGrid remap = new RemapGrid(sourceGrid.getParm().getGridInfo()
                    .getGridLoc(), gridSlice.getGridInfo().getGridLoc());
            setGrid(remap.remap(((DiscreteGridSlice) sourceGrid.getGridSlice())
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
        // throw new UnsupportedOperationException("Attempt to getWxValue: ");
        populate();
        int index = 0xFF & getGrid().get(x, y);
        DiscreteWxValue tmpDiscreteWxValue = new DiscreteWxValue(
                getKey()[index], getParm());

        return tmpDiscreteWxValue;
    }

    @Override
    public IGridSlice gridMax(IGridSlice gridSlice) {
        throw new UnsupportedOperationException(
                "Attempt to gridMax: on a Discrete Grid");
    }

    @Override
    public IGridSlice gridMin(IGridSlice gridSlice) {
        throw new UnsupportedOperationException(
                "Attempt to gridMin: on a Discrete Grid");
    }

    @Override
    public IGridSlice gridMultiply(float factor) {
        throw new UnsupportedOperationException(
                "Attempt to gridMultiply: on a Discrete Grid");
    }

    @Override
    public IGridSlice gridSum(IGridSlice gridSlice) {
        throw new UnsupportedOperationException(
                "Attempt to gridSum: on a Discrete Grid");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#doContiguous(java
     * .util.Date, java.awt.Point)
     */
    @Override
    protected Grid2DBit doContiguous(Date time, Point location) {
        Point size = getParm().getGridInfo().getGridLoc().gridSize();
        Grid2DBit valid = new Grid2DBit(size.x, size.y, true);

        // get the grid
        Grid2DByte originalGrid;
        try {
            DiscreteGridSlice slice = this.getDiscreteSlice().clone();
            if (iscMode()) {
                valid = getISCGrid(time, slice);
            }
            originalGrid = slice.getDiscreteGrid();
        } catch (CloneNotSupportedException e) {
            originalGrid = new Grid2DByte();
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
        Grid2DByte grid = getGrid();
        if (grid.getXdim() != editArea.getXdim()
                || grid.getYdim() != editArea.getYdim()) {
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

    public Grid2DBit comparisonOperate(String value, Op op) {
        Grid2DByte grid = getGrid();
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
            bits = getDiscreteSlice().eq(dkey);
            if (op == Op.NOT_EQ) {
                bits.negate();
            }
            break;
        }
        case ALMOST:
            bits = getDiscreteSlice().almost(value);
            break;
        case NOT_ALMOST:
            bits = getDiscreteSlice().almost(value);
            bits.negate();
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in DiscreteGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    public Grid2DBit comparisonOperate(IGridData gridData, Op op) {
        if (gridData.getParm().getGridInfo().getGridType() != GridType.DISCRETE) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid gridData type in DiscreteGridData::operate().");
            return new Grid2DBit();
        }

        Grid2DByte grid = getGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
            bits = getDiscreteSlice().eq(
                    (DiscreteGridSlice) gridData.getGridSlice());
            break;
        case NOT_EQ:
            bits = getDiscreteSlice().notEq(
                    (DiscreteGridSlice) gridData.getGridSlice());
            break;
        case ALMOST:
            bits = getDiscreteSlice().almost(
                    (DiscreteGridSlice) gridData.getGridSlice());
            break;
        case NOT_ALMOST:
            bits = getDiscreteSlice().almost(
                    (DiscreteGridSlice) gridData.getGridSlice());
            bits.negate();
            break;
        default:
            statusHandler.handle(Priority.PROBLEM, "Invalid operator: " + op
                    + " in DiscreteGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    public Grid2DBit comparisonOperate(DiscreteKey value, Op op) {
        Grid2DByte grid = getGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        if (!value.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Illegal discrete key in doComparisonOperate()");
            return bits;
        }

        switch (op) {
        case EQ:
            bits = getDiscreteSlice().eq(value);
            break;
        case NOT_EQ:
            bits = getDiscreteSlice().notEq(value);
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in DiscreteGridData::comparisonOperate.");
            break;
        }

        return bits;
    }

    public boolean operate(DiscreteGridData gridData, Op op,
            ReferenceData refData) {
        if (gridData.getParm().getGridInfo().getGridType() != GridType.DISCRETE) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid gridData type in DiscreteGridData::operate().");
            return false;
        }

        Grid2DBit bits = refData.getGrid();
        boolean didIt = false;
        switch (op) {
        case ASSIGN:
            didIt = getDiscreteSlice().assign((gridData.getDiscreteSlice()),
                    bits);
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in DiscreteGridData::operate.");
            break;
        }

        return didIt;
    }

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
            didIt = getDiscreteSlice().assign(value, bits);
            break;
        default:
            statusHandler.handle(Priority.ERROR, "Invalid operator: " + op
                    + " in DiscreteGridData::operate.");
            break;
        }

        return didIt;
    }

    /**
     * @see com.raytheon.viz.gfe.core.griddata.AbstractGridData#doSet(com.raytheon.viz.gfe.core.wxvalue.WxValue,
     *      com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit)
     */
    @Override
    public Grid2DBit doSet(WxValue value, Grid2DBit pointsToSet) {
        GridType gridType = value.getParm().getGridInfo().getGridType();
        Grid2DByte discreteGrid = getGrid();
        int xDim = discreteGrid.getXdim();
        int yDim = discreteGrid.getYdim();
        if (gridType != GridType.DISCRETE) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid WxValue type in DiscreteGridData::doSet().");
            return new Grid2DBit(xDim, yDim); // unchanged
        }

        DiscreteKey dk = ((DiscreteWxValue) value).getDiscreteKey();
        if (!dk.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid discrete key in doSet()");
            return new Grid2DBit(xDim, yDim); // unchanged
        }
        byte index = lookupKeyValue(dk); // look up key

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
        boolean overlapping = DiscreteKey.discreteDefinition(siteId).overlaps(
                parmId.getCompositeName());

        // combine mode application
        if (CombineMode.COMBINE.equals(getParm().getParmState()
                .getCombineMode()) && overlapping) {
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
                                DiscreteKey combinedKey = DiscreteKey.combine(
                                        dk, getKey()[dataPointIdx]);

                                // Store new key index in lookup table
                                newValues[dataPointIdx] = lookupKeyValue(combinedKey);
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

    public byte lookupKeyValue(DiscreteKey wxkey) {
        // first check to see if it already is in the discrete key
        int i = -1;
        DiscreteKey keys[] = getKey();
        for (int j = 0; j < keys.length; j++) {
            if (keys[j].equals(wxkey)) {
                i = j;
            }
        }
        if (i != -1) {
            return (byte) i;
        }

        // not in discrete key, must allocate a new entry
        DiscreteKey keyArray[] = new DiscreteKey[keys.length + 1];
        System.arraycopy(keys, 0, keyArray, 0, keys.length);
        keyArray[keyArray.length - 1] = wxkey;
        setKey(keyArray);
        return (byte) (getKey().length - 1);
    }

    @Override
    protected Grid2DBit doDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange) {
        throw new UnsupportedOperationException("Attempt to pencilStretch: ");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#doCopy(java.util.
     * Date, com.raytheon.edex.grid.Grid2DBit, java.awt.Point)
     */
    @Override
    protected Grid2DBit doCopy(Date time, Grid2DBit pointsToCopy, Point delta) {
        DiscreteGridSlice thisSlice = getDiscreteSlice();
        Grid2DByte sliceGrid = thisSlice.getDiscreteGrid();
        if (sliceGrid.getXdim() != pointsToCopy.getXdim()
                || sliceGrid.getYdim() != pointsToCopy.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doCopy: "
                    + sliceGrid.getXdim() + ',' + sliceGrid.getYdim() + ' '
                    + pointsToCopy.getXdim() + ',' + pointsToCopy.getYdim());
        }

        // set up translation matrix
        byte translate[] = new byte[255];
        Arrays.fill(translate, (byte) -1);

        // get the grid
        Grid2DByte originalGrid;
        DiscreteKey[] originalKey;
        try {
            DiscreteGridSlice slice = thisSlice.clone();
            if (iscMode()) {
                getISCGrid(time, slice);
            }
            originalGrid = slice.getDiscreteGrid();
            originalKey = slice.getKey();
        } catch (CloneNotSupportedException e) {
            originalGrid = new Grid2DByte();
            originalKey = new DiscreteKey[0];
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
                        if (sliceGrid.isValid(newx, newy)) {
                            // byte og = originalGrid.get(i, j);
                            int og = 0xFF & originalGrid.get(i, j);
                            byte v = translate[og];
                            if (v == -1) {
                                v = lookupKeyValue(originalKey[og]);
                                translate[og] = v;
                            }
                            sliceGrid.set(newx, newy, v);
                        }
                    }
                }
            }
            setGrid(sliceGrid);
        }

        return pointsToCopy.translate(delta);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#doFillIn(java.util
     * .Date, com.raytheon.edex.grid.Grid2DBit)
     */
    @Override
    protected Grid2DBit doFillIn(Date time, Grid2DBit pointsToFillIn) {
        Grid2DByte grid = getGrid();
        if (grid.getXdim() != pointsToFillIn.getXdim()
                || grid.getYdim() != pointsToFillIn.getYdim()) {
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
                if (coord.x < 0 || coord.x >= gridCells.getXdim()
                        || coord.y < 0 || coord.y >= gridCells.getYdim()) {
                    done = true;
                }
                // Check for bit set
                else if (gridCells.getAsBoolean(coord.x, coord.y)) {
                    distance++;
                    coord.x += dirX[direction];
                    coord.y += dirY[direction];
                } else // We found one
                {
                    if (distance < minDistance) {
                        Grid2DByte grid = getGrid();
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
    public Object[] getNumPy() {
        return new Object[] { this.getGrid().getBuffer().array() };
    }

    @Override
    public int getNumpyX() {
        return this.getGrid().getXdim();
    }

    @Override
    public int getNumpyY() {
        return this.getGrid().getYdim();
    }

    @Override
    public void setGridSlice(IGridSlice gridSlice) {
        if (!(gridSlice instanceof DiscreteGridSlice)) {
            throw new IllegalArgumentException(
                    "Called DiscreteGridData.setGridSlice with "
                            + gridSlice.getClass().getSimpleName());
        }
        if (this.gridSlice != null) {
            // clear out previous cache
            ((DiscreteGridSlice) this.gridSlice).setDiscreteGrid(null);
        }
        boolean wasCached = this.gridSlice.getUseCache();
        this.gridSlice = gridSlice;
        this.gridSlice.setUseCache(wasCached);
    }

    public void set(Grid2DByte values, List<DiscreteKey> key, Grid2DBit points) {
        populate();
        checkOkayForEdit();

        gridSet(values, key, points);
        setChangedPoints(points);
    }

    protected void gridSet(Grid2DByte values, List<DiscreteKey> key,
            Grid2DBit points) {
        Grid2DByte grid = getGrid();
        Point dim = new Point(grid.getXdim(), grid.getYdim());
        if (values.getXdim() != dim.x || values.getYdim() != dim.y
                || points.getXdim() != dim.x || points.getYdim() != dim.y) {
            throw new IllegalArgumentException(
                    "bad values/points dimensions for grid for: "
                            + this.getParm().getParmID() + " gridDim="
                            + values.getXdim() + ',' + values.getYdim()
                            + " pointsDim=" + points.getXdim() + ','
                            + points.getYdim() + " parmDim=" + dim);
        }

        // ensure values doesn't exceed keys, and keys are good
        for (DiscreteKey dKey : key) {
            if (!dKey.isValid()) {
                throw new IllegalArgumentException(
                        "Illegal discrete key in gridSet()");
            }
        }
        // search for grid out of bounds
        int numValues = values.getXdim() * values.getYdim();
        byte[] bp = values.getBuffer().array();
        for (int i = 0; i < numValues; i++) {
            if ((0xFF & bp[i]) > key.size() - 1) {
                throw new IllegalArgumentException(
                        "Illegal discrete grid (bad values) in gridSet()");
            }
        }

        ParmID parmId = getParm().getParmID();
        String siteId = parmId.getDbId().getSiteId();
        boolean overlapping = DiscreteKey.discreteDefinition(siteId).overlaps(
                parmId.getCompositeName());

        // REPLACE mode is easy
        if (parm.getParmState().getCombineMode() == ParmState.CombineMode.REPLACE
                || !overlapping) {
            // create remap array
            byte[] remap = new byte[256];
            for (int i = 0; i < key.size(); i++) {
                remap[i] = lookupKeyValue(key.get(i)); // look up
            }
            // key
            for (int i = 0; i < dim.x; i++) {
                for (int j = 0; j < dim.y; j++) {
                    if (points.get(i, j) == 1) {
                        grid.set(i, j, remap[0xFF & values.get(i, j)]);
                    }
                }
            }
        }
        // COMBINE mode is more difficult, have to do each one
        else {
            for (int i = 0; i < dim.x; i++) {
                for (int j = 0; j < dim.y; j++) {
                    if (points.get(i, j) == 1) {
                        DiscreteKey combined = DiscreteKey.combine(
                                key.get(values.get(i, j)),
                                doGetDiscreteValue(i, j));
                        grid.set(i, j, lookupKeyValue(combined));
                    }
                }
            }
        }

        setGrid(grid);
    }

    protected DiscreteKey doGetDiscreteValue(int x, int y) {
        byte gridValue = getGrid().get(x, y);
        int gridValueIdx = 0xFF & gridValue;
        return getKey()[gridValueIdx];
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#setGridSliceDataToNull
     * ()
     */
    @Override
    protected void setGridSliceDataToNull() {
        // Clone the slice with no data
        this.gridSlice = new DiscreteGridSlice(this.gridSlice.getValidTime(),
                this.gridSlice.getGridInfo(), this.gridSlice.getHistory(),
                null, new DiscreteKey[0]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.IGridData#isSupportedEditOp(com.raytheon
     * .viz.gfe.core.griddata.IGridData.EditOp)
     */
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
            statusHandler.handle(Priority.PROBLEM, "Unsupported EditOp: "
                    + editOp.name());
            return false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#isPopulated()
     */
    @Override
    public synchronized boolean isPopulated() {
        return ((DiscreteGridSlice) this.gridSlice).isPopulated();
    }

    private Grid2DByte getGrid() {
        return ((DiscreteGridSlice) getGridSlice()).getDiscreteGrid();
    }

    private void setGrid(Grid2DByte grid) {
        ((DiscreteGridSlice) getGridSlice()).setDiscreteGrid(grid);
    }

    private DiscreteKey[] getKey() {
        return ((DiscreteGridSlice) getGridSlice()).getKey();
    }

    private void setKey(DiscreteKey[] key) {
        ((DiscreteGridSlice) getGridSlice()).setKey(key);
    }

    public DiscreteGridSlice getDiscreteSlice() {
        return (DiscreteGridSlice) getGridSlice();
    }

    @Override
    protected boolean doValid() {
        String emsg = "Grid contains data which exceeds limits for this parm. ";

        if (!getGridTime().isValid() || getParm() == null
                || getGridSlice() == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid grid time, bad parm or data slice");
            return false; // time, parm, or data slice not valid
        }

        // if (!dataTypeOkay()) {
        // return false;
        // }

        // check grid size
        Point dim = getParm().getGridInfo().getGridLoc().gridSize();
        Grid2DByte grid = getGrid();
        Point gridDim = grid.getGridSize();
        if (!gridDim.equals(dim)) {
            statusHandler.handle(Priority.PROBLEM, "Grid dimensions " + gridDim
                    + " do not match Parm dimensions " + dim);
            return false;
        }

        // check data values
        byte[] data = grid.getBuffer().array();
        DiscreteKey[] keys = getKey();
        // byte keySize = (byte) keys.length;
        int keySize = keys.length;

        for (int j = 0; j < data.length; j++) {
            int value = 0xFF & data[j];
            if (value > keySize) {
                statusHandler.handle(Priority.PROBLEM, emsg + "Data=" + value
                        + " Min=0 Max=" + keySize);
                return false;
            }
        }

        // check key validity, then check for discrete key validity
        for (int i = 0; i < keys.length; i++) {
            if (!keys[i].isValid()) {
                statusHandler.handle(Priority.PROBLEM, "Invalid discrete key: "
                        + keys[i]);
                return false;
            }
        }

        return true;
    }
}
