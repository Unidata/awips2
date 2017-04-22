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
package com.raytheon.uf.common.dataplugin.gfe.slice;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import jep.NDArray;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Grid slice for Discrete weather elements
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 29, 2008           chammack  Initial Creation.
 * Feb 13, 2008  879      rbell     Legacy conversion
 * Jun 10, 2009  2159     rjpeter   Updated checkDims to check grid for null
 * Jan 30, 2013  15719    jdynina   Allowed more than 128 char width wx string
 * Aug 13, 2013  1571     randerso  Removed toString to stop it from hanging the
 *                                  debugger when trying to display the grid
 * Oct 29, 2013  2476     njensen   Updated getNumpy() and added getKeyList()
 * Oct 31, 2013  2508     randerso  Added getKeys(), deprecated getKey()
 * Apr 23, 2015  4259     njensen   Updated for new JEP API
 * Feb 24, 2106  5411     randerso  Fixed null pointer in equals()
 * Aug 02, 2016  5744     mapeters  Removed dead cache code
 * Aug 08, 2016  5744     randerso  Fix bad clone method exposed by previous
 *                                  change
 * 
 * </pre>
 * 
 * @author chammack
 */
@DynamicSerialize
public class DiscreteGridSlice extends AbstractGridSlice implements Cloneable {

    @DynamicSerializeElement
    private Grid2DByte discreteGrid;

    @DynamicSerializeElement
    private DiscreteKey[] keys;

    /**
     * Constructor for serialization only.
     */
    public DiscreteGridSlice() {
        super();
    }

    /**
     * Constructor with TimeRange, GFERecord, Grid2DByte, and a DiscreteKey
     * array.
     * 
     * @param validTime
     * @param gfeRecord
     * @param aGrid
     * @param aKey
     */
    public DiscreteGridSlice(TimeRange validTime, GFERecord gfeRecord,
            Grid2DByte aGrid, DiscreteKey[] aKey) {
        super(validTime, gfeRecord);
        this.discreteGrid = aGrid;
        keys = aKey;
    }

    /**
     * Constructor with TimeRange, GridParmInfo, GridDataHistory, Grid2DByte,
     * and a DiscreteKey array.
     * 
     * @param validTime
     * @param gpi
     * @param history
     * @param aGrid
     * @param aKey
     */
    public DiscreteGridSlice(TimeRange validTime, GridParmInfo gpi,
            GridDataHistory[] history, Grid2DByte aGrid, DiscreteKey[] aKey) {
        super(validTime, gpi, history);
        this.discreteGrid = aGrid;
        keys = aKey;
    }

    public DiscreteGridSlice(TimeRange validTime, GridParmInfo gpi,
            List<GridDataHistory> history, Grid2DByte aGrid,
            List<DiscreteKey> aKey) {
        this(validTime, gpi, history
                .toArray(new GridDataHistory[history.size()]), aGrid, aKey
                .toArray(new DiscreteKey[aKey.size()]));
    }

    /**
     * Copy constructor
     * 
     * @param rhs
     *            DiscreteGridSlice to copy
     */
    public DiscreteGridSlice(DiscreteGridSlice rhs) {
        super(rhs);

        Grid2DByte grid = rhs.getDiscreteGrid().clone();
        this.discreteGrid = grid;

        this.keys = new DiscreteKey[rhs.keys.length];
        System.arraycopy(rhs.keys, 0, this.keys, 0, rhs.keys.length);
    }

    @Override
    public void assign(IGridSlice gs) {
        if (!(gs instanceof DiscreteGridSlice)) {
            throw new IllegalArgumentException(
                    "Attempted to assign DiscreteGridSlice to non-DiscreteGridSlice object");
        }

        super.assign(gs);

        DiscreteGridSlice slice = (DiscreteGridSlice) gs;
        Grid2DByte gsDiscreteGrid = slice.getDiscreteGrid();

        if (gsDiscreteGrid != null) {
            if ((discreteGrid.getXdim() != gsDiscreteGrid.getXdim())
                    || (discreteGrid.getYdim() != gsDiscreteGrid.getYdim())) {
                throw new IllegalArgumentException(
                        "Supplied grid is not of same dimension");
            }

            discreteGrid.assign(gsDiscreteGrid);

            this.keys = new DiscreteKey[slice.keys.length];
            System.arraycopy(slice.keys, 0, this.keys, 0, slice.keys.length);
        } else {
            this.discreteGrid = null;
            this.keys = new DiscreteKey[0];
        }
    }

    @Override
    public String isValid() {
        String testNull;
        if ((testNull = super.isValid()) != null) {
            return testNull;
        }

        if ((testNull = checkDims()) != null) {
            return testNull;
        }

        if ((testNull = checkKey()) != null) {
            return testNull;
        }

        if ((testNull = checkKeyAndData()) != null) {
            return testNull;
        }

        return null;
    }

    @Override
    public boolean equals(Object rhs) {
        if (!(rhs instanceof DiscreteGridSlice)) {
            return false;
        }

        if (!super.equals(rhs)) {
            return false;
        }

        DiscreteGridSlice rhsDiscreteGridSlice = (DiscreteGridSlice) rhs;
        Grid2DByte rhsGrid = rhsDiscreteGridSlice.getDiscreteGrid();

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
                    .equals(rhsDiscreteGridSlice.keys[0xFF & rhsData[i]])) {
                return false;
            }
        }

        return true;
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
     * Checks the validity of the weather key. Returns the status. The status is
     * set to InvalidWeatherKey on failure. Success is always returned for
     * scalar and vector data. Uses the WeatherKey's isValid() to determine is a
     * key is valid.
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
     * @param gs
     *            grid slice to assign from
     * @param editArea
     *            are to assign
     * @return result of assignment
     */
    public boolean assign(DiscreteGridSlice gs, Grid2DBit editArea) {
        if ((editArea.getXdim() != discreteGrid.getXdim())
                || (editArea.getYdim() != discreteGrid.getYdim())) {
            return false;
        }

        Grid2DByte gsDiscreteGrid = gs.getDiscreteGrid();
        Point ll = new Point();
        Point ur = new Point();
        editArea.extremaOfSetBits(ll, ur);

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    // Get the DiscreteKey from the source grid
                    byte dByte = gsDiscreteGrid.get(i, j);
                    DiscreteKey dKey = gs.keys[0xFF & dByte];
                    // See if this key already exists in target grid
                    boolean found = false;
                    byte keyIndex = 0;
                    for (int k = 0; k < keys.length; k++) {
                        if (keys[k] == dKey) {
                            found = true;
                            keyIndex = (byte) k;
                        }
                    }
                    if (!found) // not found, so add the key
                    {
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
     * Assigns the specified weather value to the GridSlice.
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
     * Assigns the specified value to the GridSlice.
     * 
     * @param gs
     *            grid slice to assign from
     * @return result of assignment
     */
    public boolean assign(DiscreteGridSlice gs) {
        super.assign(gs);
        Grid2DByte discreteGrid = gs.discreteGrid.clone();

        List<DiscreteKey> currentKeys = new ArrayList<>(
                Arrays.asList(this.keys));
        byte[] data = discreteGrid.getBuffer().array();
        int thisB;
        for (int i = 0; i < data.length; i++) {
            thisB = 0xFF & data[i];
            byte keyIndex;
            if ((keyIndex = (byte) currentKeys.indexOf(gs.keys[thisB])) != -1) {
                data[i] = keyIndex;
            } else {
                data[i] = (byte) currentKeys.size();
                currentKeys.add(new DiscreteKey(gs.keys[thisB]));
            }
        }

        this.discreteGrid = discreteGrid;
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
            throw new IllegalArgumentException("Supplied keys is invalid");
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

        if (byteValues.size() == 0) {
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

    public Grid2DBit almost(DiscreteGridSlice gs, float fuzz) {
        return new Grid2DBit(discreteGrid.getXdim(), discreteGrid.getYdim());
    }

    public Grid2DBit almost(DiscreteGridSlice gs) {
        return almost(gs, 0);
    }

    /**
     * Returns a Grid2DBit whose bits are set where ever the specified GridSlice
     * is equal to the same value as this GridSlice.
     * 
     * @param gs
     * @return Grid2DBit
     */
    public Grid2DBit eq(DiscreteGridSlice gs) {
        Grid2DBit bits = new Grid2DBit(discreteGrid.getXdim(),
                discreteGrid.getYdim());

        byte[] thisB = discreteGrid.getBuffer().array();
        byte[] rhsB = gs.getDiscreteGrid().getBuffer().array();
        byte[] b = bits.getBuffer().array();
        for (int i = 0; i < thisB.length; i++) {
            if (keys[0xFF & thisB[i]].equals(gs.keys[0xFF & rhsB[i]])) {
                b[i] = (byte) 1;
            }
        }

        return bits;
    }

    /**
     * Returns the inverse of the eq() function. See eq() above for more
     * details.
     * 
     * @param gs
     * @return Grid2DBit
     */
    public Grid2DBit notEq(DiscreteGridSlice gs) {
        Grid2DBit bits = eq(gs);
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
    public DiscreteGridSlice clone() throws CloneNotSupportedException {
        TimeRange aValidTime = this.validTime.clone();
        GridParmInfo aGpi = this.gridParmInfo.clone();
        GridDataHistory[] aHistory = new GridDataHistory[this.gridDataHistory
                .size()];
        for (int i = 0; i < aHistory.length; i++) {
            aHistory[i] = this.gridDataHistory.get(i).clone();
        }

        Grid2DByte aGrid = null;
        if (discreteGrid != null) {
            aGrid = discreteGrid.clone();
        }

        DiscreteKey[] aKey = new DiscreteKey[this.keys.length];
        for (int i = 0; i < aKey.length; i++) {
            aKey[i] = new DiscreteKey(this.keys[i]);
        }
        return new DiscreteGridSlice(aValidTime, aGpi, aHistory, aGrid, aKey);
    }

    private String checkDims() {
        if (discreteGrid == null) {
            return "Grid data not populated";
        }

        int x = discreteGrid.getXdim();
        int y = discreteGrid.getYdim();

        if ((x != gridParmInfo.getGridLoc().getNx())
                || (y != gridParmInfo.getGridLoc().getNy())) {
            return "Grid Dimensions and GridParmInfo Dimensions are not identical GridDim: "
                    + x
                    + ","
                    + y
                    + " GridParmInfoDim: "
                    + gridParmInfo.getGridLoc().getNx()
                    + ","
                    + gridParmInfo.getGridLoc().getNy();
        }

        return null;
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

    public boolean isPopulated() {
        return discreteGrid != null;
    }

    public List<String> getKeyList() {
        List<String> list = new ArrayList<>(keys.length);
        for (DiscreteKey k : keys) {
            list.add(k.toString());
        }
        return list;
    }
}
