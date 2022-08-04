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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;

import jep.NDArray;

/**
 * Discrete Data Object
 *
 * Contains the grid and discrete keys for a GFE Discrete grid
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 15, 2017           randerso  Initial creation
 * Feb 13, 2019  7732     randerso  Fixed copy constructor to clone the
 *                                  DiscreteKeys.
 *
 * </pre>
 *
 * @author randerso
 */

public class DiscreteDataObject implements IDataObject {

    private Grid2DByte discreteGrid;

    private DiscreteKey[] keys;

    /**
     * Constructor
     *
     * @param grid
     * @param keys
     */
    public DiscreteDataObject(Grid2DByte grid, DiscreteKey[] keys) {
        this.discreteGrid = grid;
        this.keys = keys;
    }

    /**
     * Copy constructor
     *
     * @param other
     *            DiscreteDataObject to copy
     */
    public DiscreteDataObject(DiscreteDataObject other) {
        Grid2DByte grid = other.getDiscreteGrid().copy();
        this.discreteGrid = grid;

        DiscreteKey[] copiedKeys = new DiscreteKey[other.keys.length];
        for (int i = 0; i < copiedKeys.length; i++) {
            copiedKeys[i] = new DiscreteKey(other.keys[i]);
        }
        setKeys(copiedKeys);
    }

    @Override
    public void assign(IDataObject other) {
        if (!(other instanceof DiscreteDataObject)) {
            throw new IllegalArgumentException(
                    "other must be an instance of DiscreteDataObject, received: "
                            + other.getClass().getName());
        }
        this.assign((DiscreteDataObject) other);
    }

    @Override
    public boolean equals(Object rhs) {
        if (this == rhs) {
            return true;
        }
        if (rhs == null) {
            return false;
        }
        if (getClass() != rhs.getClass()) {
            return false;
        }

        DiscreteDataObject dataObject = (DiscreteDataObject) rhs;
        Grid2DByte rhsGrid = dataObject.getDiscreteGrid();

        if (discreteGrid == null) {
            return (rhsGrid == null);
        } else if (rhsGrid == null) {
            return false;
        }

        if ((discreteGrid.getXdim() != rhsGrid.getXdim())
                || (discreteGrid.getYdim() != rhsGrid.getYdim())) {
            return false;
        }

        byte[] thisData = discreteGrid.getBuffer().array();
        byte[] rhsData = rhsGrid.getBuffer().array();
        for (int i = 0; i < thisData.length; i++) {
            if (!this.keys[0xFF & thisData[i]]
                    .equals(dataObject.keys[0xFF & rhsData[i]])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Checks dimensions of grids with dimensions specified in GridParmInfo to
     * ensure they are the same. Returns the status.
     *
     * @param x
     *            X dimension from GridParmInfo
     * @param y
     *            Y dimension from GridParmInfo
     *
     * @return String if issue, otherwise null if ok.
     */
    protected String checkDims(int x, int y) {
        if ((x != discreteGrid.getXdim()) || (y != discreteGrid.getYdim())) {
            return "Grid Dimensions and GridParmInfo Dimensions are not identical GridDim: "
                    + discreteGrid.getXdim() + "," + discreteGrid.getYdim()
                    + " GridParmInfoDim: " + x + "," + y;
        }

        return null;
    }

    /**
     * Checks the validity of the discrete key. Returns the status. The status
     * is set to Invalid Key found on failure. Success is always returned for
     * scalar and vector data. Uses the DiscreteKey's isValid() to determine is
     * a key is valid.
     *
     * @return null if the key is ok, otherwise the reason why not
     */
    public String checkKey() {
        for (int i = 0; i < keys.length; i++) {
            if (!this.keys[i].isValid()) {
                return "Invalid Key found in Grid. Key Position is " + i
                        + " Key is: " + keys[i].getOrigStr();
            }
        }
        return null;
    }

    /**
     * Checks the key and grid values to ensure that there is a key entry for
     * every grid value. Returns the status. Checks that all data in the grid
     * has a key by using the key's length.
     *
     * @return null if everything was ok, otherwise the reason why not
     */
    public String checkKeyAndData() {
        int keyLength = keys.length;
        byte[] b = discreteGrid.getBuffer().array();
        for (int i = 0; i < b.length; i++) {
            int index = 0xFF & b[i];
            if (index >= keyLength) {
                return "Data Values Exceeded in Grid at coordinate: "
                        + (i % discreteGrid.getXdim()) + ","
                        + (i / discreteGrid.getXdim()) + " Value=" + index
                        + " MinAllowed=0 MaxAllowed=" + (keyLength - 1);
            }
        }
        return null;
    }

    /**
     * Assigns the specified discrete value to the GridSlice.
     *
     * @param aValue
     *            value to assign into the grid
     * @param editArea
     *            area to assign
     * @return result of assignment
     */
    public boolean assign(DiscreteKey aValue, Grid2DBit editArea) {
        if (!aValue.isValid()) {
            return false;
        }

        Point gridSize = new Point(discreteGrid.getXdim(),
                discreteGrid.getYdim());
        if ((editArea.getXdim() != gridSize.x)
                || (editArea.getYdim() != gridSize.y)) {
            return false;
        }

        Point ll = new Point();
        Point ur = new Point();
        editArea.extremaOfSetBits(ll, ur);

        // find a key match
        byte dByte = 0;
        boolean found = false;
        for (int k = 0; k < keys.length; k++) {
            if (keys[k].equals(aValue)) {
                dByte = (byte) k;
                found = true;
            }
        }

        if (!found) {
            DiscreteKey newKey[] = new DiscreteKey[keys.length + 1];
            System.arraycopy(keys, 0, newKey, 0, keys.length);
            newKey[newKey.length - 1] = aValue;
            keys = newKey;
            dByte = (byte) (keys.length - 1);
        }

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    discreteGrid.set(i, j, dByte);
                }
            }
        }

        return true;
    }

    /**
     * Assigns the specified value to the GridSlice.
     *
     * @param dataObject
     *            data object to assign from
     * @param editArea
     *            area to assign
     * @return result of assignment
     */
    public boolean assign(DiscreteDataObject dataObject, Grid2DBit editArea) {
        if ((editArea.getXdim() != discreteGrid.getXdim())
                || (editArea.getYdim() != discreteGrid.getYdim())) {
            return false;
        }

        Grid2DByte sourceGrid = dataObject.getDiscreteGrid();
        Point ll = new Point();
        Point ur = new Point();
        editArea.extremaOfSetBits(ll, ur);

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    // Get the DiscreteKey from the source grid
                    byte dByte = sourceGrid.get(i, j);
                    DiscreteKey dKey = dataObject.keys[0xFF & dByte];
                    // See if this key already exists in target grid
                    boolean found = false;
                    byte keyIndex = 0;
                    for (int k = 0; k < keys.length; k++) {
                        if (keys[k] == dKey) {
                            found = true;
                            keyIndex = (byte) k;
                        }
                    }
                    if (!found) {
                        // not found, so add the key
                        DiscreteKey newKey[] = new DiscreteKey[keys.length + 1];
                        System.arraycopy(keys, 0, newKey, 0, keys.length);
                        newKey[newKey.length - 1] = dKey;
                        keys = newKey;
                        keyIndex = (byte) (keys.length - 1);
                    }

                    discreteGrid.set(i, j, keyIndex);
                }
            }
        }

        return true;
    }

    /**
     * Assigns the specified discrete value to the GridSlice.
     *
     * @param aValue
     *            the value to set this grid as
     * @return the result of the assignment
     */
    public boolean assign(DiscreteKey aValue) {
        if (!aValue.isValid()) {
            return false;
        }

        // find a key match
        byte dByte = 0;
        boolean found = false;
        for (int k = 0; k < keys.length; k++) {
            if (keys[k].equals(aValue)) {
                dByte = (byte) k;
                found = true;
            }
        }

        if (!found) {
            DiscreteKey newKey[] = new DiscreteKey[keys.length + 1];
            System.arraycopy(keys, 0, newKey, 0, keys.length);
            newKey[newKey.length - 1] = aValue;
            keys = newKey;
            dByte = (byte) (keys.length - 1);
        }

        Arrays.fill(discreteGrid.getBuffer().array(), dByte);

        return true;
    }

    /**
     * Assigns the specified value to this
     *
     * @param dataObject
     *            data Object to assign from
     * @return result of assignment
     */
    public boolean assign(DiscreteDataObject dataObject) {
        if ((discreteGrid.getXdim() != dataObject.getDiscreteGrid().getXdim())
                || (discreteGrid.getYdim() != dataObject.getDiscreteGrid()
                        .getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    discreteGrid.getXdim(), discreteGrid.getYdim(),
                    dataObject.getDiscreteGrid().getXdim(),
                    dataObject.getDiscreteGrid().getYdim()));
        }

        Grid2DByte newDiscreteGrid = dataObject.discreteGrid.copy();

        List<DiscreteKey> currentKeys = new ArrayList<>(
                Arrays.asList(this.keys));
        byte[] data = newDiscreteGrid.getBuffer().array();
        int thisB;
        for (int i = 0; i < data.length; i++) {
            thisB = 0xFF & data[i];
            byte keyIndex;
            if ((keyIndex = (byte) currentKeys
                    .indexOf(dataObject.keys[thisB])) != -1) {
                data[i] = keyIndex;
            } else {
                data[i] = (byte) currentKeys.size();
                currentKeys.add(new DiscreteKey(dataObject.keys[thisB]));
            }
        }

        this.discreteGrid = newDiscreteGrid;
        this.keys = currentKeys.toArray(new DiscreteKey[currentKeys.size()]);

        return true;
    }

    /**
     * Returns a Grid2DBit that corresponds to the gridcells that are equal to
     * the specified DiscreteKey value.
     *
     * @param value
     *            the DiscreteKey to test for
     * @return a Grid2DBit with bits set that match the input DiscreteKey
     */
    public Grid2DBit eq(DiscreteKey value) {
        if (!value.isValid()) {
            throw new IllegalArgumentException(
                    "Supplied key is invalid: " + value);
        }

        Point gridSize = new Point(discreteGrid.getXdim(),
                discreteGrid.getYdim());
        Grid2DBit bits = new Grid2DBit(gridSize.x, gridSize.y);

        // Get or make a Discrete
        byte dByte = 0;
        boolean found = false;
        for (int k = 0; k < keys.length; k++) {
            if (keys[k].equals(value)) {
                dByte = (byte) k;
                found = true;
            }
        }

        if (!found) {
            return bits;
        }

        for (int i = 0; i < gridSize.x; i++) {
            for (int j = 0; j < gridSize.y; j++) {
                if ((discreteGrid.get(i, j)) == dByte) {
                    bits.set(i, j);
                }
            }
        }

        return bits;
    }

    /**
     * Returns the inverse of the eq function. See eq() above for details.
     *
     * @param value
     *            the DiscreteKey to test for
     * @return a Grid2DBit with bits set that do not match the input DiscreteKey
     */
    public Grid2DBit notEq(DiscreteKey value) {
        Grid2DBit bits = eq(value);

        if (bits.isValid()) {
            bits.negate();
        } else {
            return null;
        }

        return bits;
    }

    /**
     * Returns a Grid2DBit whose bits are set wherever the discrete type is the
     * same as that specified in the TextString value.
     *
     * @param value
     * @return a new Grid2DBit indicating where the String value is
     */
    public Grid2DBit almost(String value) {
        Point gridSize = new Point(discreteGrid.getXdim(),
                discreteGrid.getYdim());
        Grid2DBit bits = new Grid2DBit(gridSize.x, gridSize.y);

        // Check for each value
        // Get the list of subkey permutations from the value given
        List<String> searchKeys = keys[0].descriptionSubKeys(value);

        // Get the byte values that correspond to the specified textStrings
        List<Byte> byteValues = new ArrayList<>();

        // Check each discrete key for a match of a subkey
        for (int i = 0; i < keys.length; i++) {
            // Check each subkey
            for (int j = 0; j < keys[i].getSubKeys().size(); j++) {
                for (int k = 0; k < searchKeys.size(); k++) {
                    if (keys[i].getSubKeys().get(j).equals(searchKeys.get(k))) {
                        byteValues.add((byte) i);
                        break;
                    }
                }
            }
        }

        if (byteValues.isEmpty()) {
            return bits;
        }

        // it was found so set all bits with a value of subKeyIndex
        for (int i = 0; i < gridSize.x; i++) {
            for (int j = 0; j < gridSize.y; j++) {
                if (byteValues.contains(discreteGrid.get(i, j))) {
                    bits.set(i, j);
                }
            }
        }

        return bits;
    }

    /**
     * Almost is not supported for discrete grids
     *
     * @param dataObject
     * @param fuzz
     * @return mask with no bits set
     */
    public Grid2DBit almost(DiscreteDataObject dataObject, float fuzz) {
        return new Grid2DBit(discreteGrid.getXdim(), discreteGrid.getYdim());
    }

    /**
     * Almost is not supported for discrete grids
     *
     * @param dataObject
     * @return mask with no bits set
     */
    public Grid2DBit almost(DiscreteDataObject dataObject) {
        return almost(dataObject, 0);
    }

    /**
     * Returns a Grid2DBit whose bits are set where ever the specified GridSlice
     * is equal to the same value as this DataObject.
     *
     * @param dataObject
     * @return Grid2DBit
     */
    public Grid2DBit eq(DiscreteDataObject dataObject) {
        Grid2DBit bits = new Grid2DBit(discreteGrid.getXdim(),
                discreteGrid.getYdim());

        byte[] thisB = discreteGrid.getBuffer().array();
        byte[] rhsB = dataObject.getDiscreteGrid().getBuffer().array();
        byte[] b = bits.getBuffer().array();
        for (int i = 0; i < thisB.length; i++) {
            if (keys[0xFF & thisB[i]].equals(dataObject.keys[0xFF & rhsB[i]])) {
                b[i] = (byte) 1;
            }
        }

        return bits;
    }

    /**
     * Returns the inverse of the eq() function. See eq() above for more
     * details.
     *
     * @param dataObject
     * @return Grid2DBit
     */
    public Grid2DBit notEq(DiscreteDataObject dataObject) {
        Grid2DBit bits = eq(dataObject);
        bits.negate();
        return bits;
    }

    /**
     * Collapses the discrete key/grid to not contain extra key definitions.
     */
    @Override
    public void collapse() {
        if (discreteGrid == null) {
            return;
        }

        /*
         * for duplicate keys, set all locations in grid that match that key to
         * the "first" of the duplicate keys
         */
        for (int i = 0; i < (keys.length - 1); i++) {
            for (int j = i; j < keys.length; j++) {
                if (keys[i].equals(keys[j])) {
                    discreteGrid.setAllOfValue((byte) j, (byte) i);
                }
            }
        }

        /* find all keys that exist in the grid */
        List<Byte> usedKeys = new ArrayList<>();
        byte[] b = discreteGrid.getBuffer().array();
        for (int i = 0; i < b.length; i++) {
            if (!usedKeys.contains(b[i])) {
                usedKeys.add(b[i]);
            }
        }

        /*
         * Now remove all unused keys
         */
        Collections.sort(usedKeys);
        int keyIndex = 0;
        DiscreteKey newKey[] = new DiscreteKey[usedKeys.size()];
        for (Iterator<Byte> usedKeysI = usedKeys.iterator(); usedKeysI
                .hasNext(); keyIndex++) {
            byte thisByte = usedKeysI.next();
            discreteGrid.setAllOfValue(thisByte, (byte) keyIndex);
            newKey[keyIndex] = keys[0xFF & thisByte];
        }

        keys = newKey;
    }

    @Override
    public DiscreteDataObject copy() {
        return new DiscreteDataObject(this);
    }

    /**
     * Return the discrete grid
     *
     * @return the discrete grid
     */
    public Grid2DByte getDiscreteGrid() {
        return this.discreteGrid;
    }

    /**
     * Return the discrete keys
     *
     * @return the discrete keys
     * @deprecated use getKeys() instead
     */
    @Deprecated
    public DiscreteKey[] getKey() {
        return this.keys;
    }

    /**
     * Return the discrete keys
     *
     * @return the discrete keys
     */
    public DiscreteKey[] getKeys() {
        return this.keys;
    }

    /**
     * @param grid
     */
    public void setDiscreteGrid(Grid2DByte grid) {
        this.discreteGrid = grid;
    }

    /**
     * @param keys
     *            the keys to set
     * @deprecated use setKeys() instead
     */
    @Deprecated
    public void setKey(DiscreteKey[] keys) {
        this.keys = keys;
    }

    /**
     * Used by iscMosaic.py
     *
     * @param key
     */
    public void setKey(List<DiscreteKey[]> key) {
        setKeys(key.toArray(new DiscreteKey[] {}));
    }

    /**
     * @param keys
     *            the keys to set
     */
    public void setKeys(DiscreteKey[] keys) {
        this.keys = keys;
    }

    @Override
    public NDArray<byte[]> getNDArray() {
        /*
         * FIXME We reverse the x and y dimensions because that's what AWIPS 1
         * did and that makes the pre-existing python code compatible. Java
         * ordering is x,y while python is ordering is y,x. It's confusing and
         * questionable at best so someday someone should correct all that. Good
         * luck.
         */
        return new NDArray<>(discreteGrid.getBytes(), discreteGrid.getYdim(),
                discreteGrid.getXdim());
    }

    /**
     * @return list of keys as strings
     */
    public List<String> getKeyList() {
        List<String> list = new ArrayList<>(keys.length);
        for (DiscreteKey k : keys) {
            list.add(k.toString());
        }
        return list;
    }
}
