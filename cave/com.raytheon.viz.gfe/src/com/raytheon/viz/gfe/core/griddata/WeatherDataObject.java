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
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import jep.NDArray;

/**
 * Weather Data Object
 *
 * Contains the grid and discrete keys for a GFE Discrete grid
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 02, 2018           randerso  Initial creation
 * Feb 13, 2019  7732     randerso  Fixed copy constructor to clone the
 *                                  WeatherKeys.
 *
 * </pre>
 *
 * @author randerso
 */

public class WeatherDataObject implements IDataObject {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherDataObject.class);

    private Grid2DByte weatherGrid;

    private WeatherKey[] keys;

    /**
     * Constructor with TimeRange, GFERecord, Grid2DByte, and a WeatherKey
     * array.
     *
     * @param grid
     * @param keys
     */
    public WeatherDataObject(Grid2DByte grid, WeatherKey[] keys) {
        this.weatherGrid = grid;
        this.keys = keys;
    }

    /**
     * Copy constructor
     *
     * @param other
     *            WeatherDataObject to copy
     */
    public WeatherDataObject(WeatherDataObject other) {
        Grid2DByte grid = other.getWeatherGrid().copy();
        this.weatherGrid = grid;

        WeatherKey[] copiedKeys = new WeatherKey[other.keys.length];
        for (int i = 0; i < copiedKeys.length; i++) {
            copiedKeys[i] = new WeatherKey(other.keys[i]);
        }
        setKeys(copiedKeys);
    }

    @Override
    public void assign(IDataObject other) {
        if (!(other instanceof WeatherDataObject)) {
            throw new IllegalArgumentException(
                    "other must be an instance of WeatherDataObject, received: "
                            + other.getClass().getName());
        }
        this.assign((WeatherDataObject) other);
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

        WeatherDataObject dataObject = (WeatherDataObject) rhs;
        Grid2DByte rhsGrid = dataObject.getWeatherGrid();

        if (weatherGrid == null) {
            return (rhsGrid == null);
        } else if (rhsGrid == null) {
            return false;
        }

        if ((weatherGrid.getXdim() != rhsGrid.getXdim())
                || (weatherGrid.getYdim() != rhsGrid.getYdim())) {
            return false;
        }

        byte[] thisData = weatherGrid.getBuffer().array();
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
        if ((x != weatherGrid.getXdim()) || (y != weatherGrid.getYdim())) {
            return "Grid Dimensions and GridParmInfo Dimensions are not identical GridDim: "
                    + weatherGrid.getXdim() + "," + weatherGrid.getYdim()
                    + " GridParmInfoDim: " + x + "," + y;
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
     * Checks the key and grid values to ensure that there is a key entry for
     * every grid value. Returns the status. Checks that all data in the grid
     * has a key by using the key's length.
     *
     * @return null if everything was ok, otherwise the reason why not
     */
    public String checkKeyAndData() {
        int keyLength = keys.length;
        byte[] b = weatherGrid.getBuffer().array();
        for (int i = 0; i < b.length; i++) {
            int index = 0xFF & b[i];
            if (index >= keyLength) {
                return "Data Values Exceeded in Grid at coordinate: "
                        + (i % weatherGrid.getXdim()) + ","
                        + (i / weatherGrid.getXdim()) + " Value=" + index
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
    public boolean assign(WeatherKey aValue, Grid2DBit editArea) {
        if (!aValue.isValid()) {
            return false;
        }

        Point gridSize = new Point(weatherGrid.getXdim(),
                weatherGrid.getYdim());
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
            WeatherKey newKey[] = new WeatherKey[keys.length + 1];
            System.arraycopy(keys, 0, newKey, 0, keys.length);
            newKey[newKey.length - 1] = aValue;
            keys = newKey;
            dByte = (byte) (keys.length - 1);
        }

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    weatherGrid.set(i, j, dByte);
                }
            }
        }

        return true;
    }

    /**
     * Assigns the specified value to the DataObject.
     *
     * @param dataObject
     *            data object to assign from
     * @param editArea
     *            area to assign
     * @return result of assignment
     */
    public boolean assign(WeatherDataObject dataObject, Grid2DBit editArea) {
        if ((editArea.getXdim() != weatherGrid.getXdim())
                || (editArea.getYdim() != weatherGrid.getYdim())) {
            return false;
        }

        Grid2DByte sourceGrid = dataObject.getWeatherGrid();
        Point ll = new Point();
        Point ur = new Point();
        editArea.extremaOfSetBits(ll, ur);

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    // Get the WeatherKey from the source grid
                    byte dByte = sourceGrid.get(i, j);
                    WeatherKey dKey = dataObject.keys[0xFF & dByte];
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
                        WeatherKey newKey[] = new WeatherKey[keys.length + 1];
                        System.arraycopy(keys, 0, newKey, 0, keys.length);
                        newKey[newKey.length - 1] = dKey;
                        keys = newKey;
                        keyIndex = (byte) (keys.length - 1);
                    }

                    weatherGrid.set(i, j, keyIndex);
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
    public boolean assign(WeatherKey aValue) {
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
            WeatherKey newKey[] = new WeatherKey[keys.length + 1];
            System.arraycopy(keys, 0, newKey, 0, keys.length);
            newKey[newKey.length - 1] = aValue;
            keys = newKey;
            dByte = (byte) (keys.length - 1);
        }

        Arrays.fill(weatherGrid.getBuffer().array(), dByte);

        return true;
    }

    /**
     * Assigns the specified value to the GridSlice.
     *
     * @param dataObject
     *            grid slice to assign from
     * @return result of assignment
     */
    public boolean assign(WeatherDataObject dataObject) {
        if ((weatherGrid.getXdim() != dataObject.getWeatherGrid().getXdim())
                || (weatherGrid.getYdim() != dataObject.getWeatherGrid()
                        .getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    weatherGrid.getXdim(), weatherGrid.getYdim(),
                    dataObject.getWeatherGrid().getXdim(),
                    dataObject.getWeatherGrid().getYdim()));
        }

        Grid2DByte weatherGrid = dataObject.weatherGrid.copy();

        List<WeatherKey> currentKeys = new ArrayList<>(
                Arrays.asList(this.keys));
        byte[] data = weatherGrid.getBuffer().array();
        int thisB;
        for (int i = 0; i < data.length; i++) {
            thisB = 0xFF & data[i];
            byte keyIndex;
            if ((keyIndex = (byte) currentKeys
                    .indexOf(dataObject.keys[thisB])) != -1) {
                data[i] = keyIndex;
            } else {
                data[i] = (byte) currentKeys.size();
                currentKeys.add(new WeatherKey(dataObject.keys[thisB]));
            }
        }

        this.weatherGrid = weatherGrid;
        this.keys = currentKeys.toArray(new WeatherKey[currentKeys.size()]);

        return true;
    }

    /**
     * Returns a Grid2DBit that corresponds to the grid cells that are equal to
     * the specified WeatherKey value.
     *
     * @param value
     *            the WeatherKey to test for
     * @return a Grid2DBit with bits set that match the input WeatherKey
     */
    public Grid2DBit eq(WeatherKey value) {
        if (!value.isValid()) {
            throw new IllegalArgumentException(
                    "Supplied key is invalid: " + value);
        }

        Point gridSize = new Point(weatherGrid.getXdim(),
                weatherGrid.getYdim());
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
                if ((weatherGrid.get(i, j)) == dByte) {
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
     *            the WeatherKey to test for
     * @return a Grid2DBit with bits set that do not match the input WeatherKey
     */
    public Grid2DBit notEq(WeatherKey value) {
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
     * @return the resulting grid
     */
    public Grid2DBit almost(String value) {
        Point gridSize = new Point(weatherGrid.getXdim(),
                weatherGrid.getYdim());
        Grid2DBit bits = new Grid2DBit(gridSize.x, gridSize.y);

        // Check for each value
        // Get the list of subkey permutations from the value given
        List<WeatherSubKey> searchKeys = keys[0].descriptionSubKeys(value);

        // Get the byte values that correspond to the specified textStrings
        List<Byte> byteValues = new ArrayList<>();

        // Check each weather key against the permutations of the
        // description given
        int k = 0;
        for (WeatherKey key : keys) {
            // Check each subkey
            for (WeatherSubKey subKey : key.getSubKeys()) {
                for (WeatherSubKey searchKey : searchKeys) {
                    if (subKey.equals(searchKey)) {
                        byteValues.add((byte) k);
                        break;
                    }
                }
            }
            k++;
        }

        if (byteValues.isEmpty()) {
            return bits;
        }

        // it was found so set all bits with a value of subKeyIndex
        for (int i = 0; i < gridSize.x; i++) {
            for (int j = 0; j < gridSize.y; j++) {
                if (byteValues.contains(weatherGrid.get(i, j))) {
                    bits.set(i, j);
                }
            }
        }

        return bits;
    }

    /**
     * Almost is not supported for weather grids
     *
     * @param dataObject
     * @param fuzz
     * @return mask with no bits set
     */
    public Grid2DBit almost(WeatherDataObject dataObject, float fuzz) {
        return new Grid2DBit(weatherGrid.getXdim(), weatherGrid.getYdim());
    }

    /**
     * Almost is not supported for weather grids
     *
     * @param dataObject
     * @return mask with no bits set
     */
    public Grid2DBit almost(WeatherDataObject dataObject) {
        return almost(dataObject, 0);
    }

    /**
     * Returns a Grid2DBit whose bits are set where ever the specified GridSlice
     * is equal to the same value as this DataObject.
     *
     * @param dataObject
     * @return the resulting grid
     */
    public Grid2DBit eq(WeatherDataObject dataObject) {
        Grid2DBit bits = new Grid2DBit(weatherGrid.getXdim(),
                weatherGrid.getYdim());

        byte[] thisB = weatherGrid.getBuffer().array();
        byte[] rhsB = dataObject.getWeatherGrid().getBuffer().array();
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
     * @return the resulting grid
     */
    public Grid2DBit notEq(WeatherDataObject dataObject) {
        Grid2DBit bits = eq(dataObject);
        bits.negate();
        return bits;
    }

    /**
     * Collapses the discrete key/grid to not contain extra key definitions.
     */
    @Override
    public void collapse() {
        if (weatherGrid == null) {
            return;
        }

        try {
            int max = 0;
            for (byte b : weatherGrid.getBytes()) {
                int unsigned = 0xFF & b;
                if (unsigned > max) {
                    max = unsigned;
                }
            }

            if (max >= keys.length) {
                throw new IndexOutOfBoundsException("Grid contains index ("
                        + max + ") > keys.length (" + keys.length + ")");
            }

            // make a histogram, indicating what is and what isn't
            // used in the weather keys
            boolean[] used = new boolean[keys.length];
            int[] invMapping = new int[keys.length];
            for (int i = 0; i < used.length; i++) {
                invMapping[i] = i;
                used[i] = false;
            }

            // process the grid
            for (int i = 0; i < weatherGrid.getXdim(); i++) {
                for (int j = 0; j < weatherGrid.getYdim(); j++) {
                    // indicate used
                    used[0xFF & weatherGrid.get(i, j)] = true;
                }
            }

            // clear the invmapping if not used
            for (int i = 0; i < used.length; i++) {
                if (!used[i]) {
                    invMapping[i] = -1;
                }
            }

            // eliminate duplicate keys
            int nk = 0;
            List<WeatherKey> tmpKeys = new ArrayList<>();
            for (int i = 0; i < used.length; i++) {
                if (used[i]) {
                    tmpKeys.add(keys[i]);
                    invMapping[i] = nk;
                    for (int j = i + 1; j < used.length; j++) {
                        if (keys[i].equals(keys[j])) {
                            // key index
                            invMapping[j] = nk;

                            // to prevent reprocessing
                            used[j] = false;
                        }
                    }
                    nk++;
                }
            }
            WeatherKey[] newKeys = tmpKeys
                    .toArray(new WeatherKey[tmpKeys.size()]);

            // anything to do?
            if (Arrays.equals(newKeys, keys)) {
                return;
            }

            // now remap the data
            for (int i = 0; i < weatherGrid.getXdim(); i++) {
                for (int j = 0; j < weatherGrid.getYdim(); j++) {
                    weatherGrid.set(i, j,
                            (byte) invMapping[0xFF & weatherGrid.get(i, j)]);
                }
            }

            keys = newKeys;
        } catch (IndexOutOfBoundsException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            throw e;
        }
    }

    @Override
    public WeatherDataObject copy() {
        return new WeatherDataObject(this);
    }

    /**
     * Return the discrete grid
     *
     * @return the discrete grid
     */
    public Grid2DByte getWeatherGrid() {
        return this.weatherGrid;
    }

    /**
     * Return the weather keys
     *
     * @return the weather keys as array
     */
    public WeatherKey[] getKeys() {
        return this.keys;
    }

    /**
     * @param grid
     */
    public void setWeatherGrid(Grid2DByte grid) {
        this.weatherGrid = grid;
    }

    /**
     * @param keys
     */
    public void setKeys(WeatherKey[] keys) {
        this.keys = keys;
    }

    @Override
    public Object getNDArray() {
        /*
         * FIXME We reverse the x and y dimensions because that's what AWIPS 1
         * did and that makes the pre-existing python code compatible. Java
         * ordering is x,y while python is ordering is y,x. It's confusing and
         * questionable at best so someday someone should correct all that. Good
         * luck.
         */
        return new NDArray<>(weatherGrid.getBytes(), weatherGrid.getYdim(),
                weatherGrid.getXdim());
    }

    /**
     * @return true if populated
     */
    public boolean isPopulated() {
        return weatherGrid != null;
    }

    /**
     * @return weather keys as list of strings
     */
    public List<String> getKeyList() {
        List<String> list = new ArrayList<>(keys.length);
        for (WeatherKey k : keys) {
            list.add(k.toString());
        }
        return list;
    }

}
