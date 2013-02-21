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
import java.util.List;

import com.raytheon.uf.common.cache.CacheException;
import com.raytheon.uf.common.cache.CacheFactory;
import com.raytheon.uf.common.cache.ICache;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.IGrid2D;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 15, 2011            randerso     Initial creation
 * Jan 30, 2013 15719      jdynina      Allowed more than 128 char wx string 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class WeatherGridSlice extends AbstractGridSlice {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherGridSlice.class);

    @DynamicSerializeElement
    private Grid2DByte weatherGrid;

    private String cacheId;

    @DynamicSerializeElement
    private WeatherKey[] keys;

    /**
     * Constructor for serialization only.
     */
    public WeatherGridSlice() {
        super();
    }

    /**
     * Constructor with TimeRange, GFERecord, Grid2DByte, and a WeatherKey
     * array.
     * 
     * @param validTime
     * @param gfeRecord
     * @param grid
     * @param keys
     */
    public WeatherGridSlice(TimeRange validTime, GFERecord gfeRecord,
            Grid2DByte grid, WeatherKey[] keys) {
        super(validTime, gfeRecord);
        setWeatherGrid(grid);
        this.keys = keys;
    }

    /**
     * Constructor with TimeRange, GFERecord, Grid2DBWeatherKeyDiscreteKey
     * array.
     * 
     * @param validTime
     * @param gpi
     * @param history
     * @param grid
     * @param keys
     */
    public WeatherGridSlice(TimeRange validTime, GridParmInfo gpi,
            GridDataHistory[] history, Grid2DByte grid, WeatherKey[] keys) {
        super(validTime, gpi, history);
        setWeatherGrid(grid);
        this.keys = keys;
    }

    public WeatherGridSlice(TimeRange validTime, GridParmInfo gpi,
            List<GridDataHistory> history, Grid2DByte grid,
            List<WeatherKey> keys) {
        this(validTime, gpi, history
                .toArray(new GridDataHistory[history.size()]), grid, keys
                .toArray(new WeatherKey[keys.size()]));
    }

    /**
     * Copy constructor, defaults to no caching.
     * 
     * @param rhs
     *            the weather slice to be copied
     */
    public WeatherGridSlice(WeatherGridSlice rhs) {
        this(rhs, false);
    }

    /**
     * Copy constructor
     * 
     * @param rhs
     *            WeatherGridSlice to copy
     * @param useCache
     *            Whether or not to use cache initially. Useful when copying
     *            structure and will need to immediately modify, allowing for
     *            data to only be written to cache once.
     */
    public WeatherGridSlice(WeatherGridSlice rhs, boolean useCache) {
        super(rhs);

        this.useCache = useCache;
        Grid2DByte grid = rhs.getWeatherGrid().clone();
        setWeatherGrid(grid);

        this.keys = new WeatherKey[rhs.keys.length];
        System.arraycopy(rhs.keys, 0, this.keys, 0, rhs.keys.length);
    }

    @Override
    public void assign(IGridSlice gs) {
        if (!(gs instanceof WeatherGridSlice)) {
            throw new IllegalArgumentException(
                    "Attempted to assign WeatherGridSlice to non-WeatherGridSlice object");
        }

        super.assign(gs);

        WeatherGridSlice slice = (WeatherGridSlice) gs;
        Grid2DByte gsWeatherGrid = slice.getWeatherGrid();

        if (gsWeatherGrid != null) {
            Grid2DByte weatherGrid = getWeatherGrid();
            if (weatherGrid.getXdim() != gsWeatherGrid.getXdim()
                    || weatherGrid.getYdim() != gsWeatherGrid.getYdim()) {
                throw new IllegalArgumentException(
                        "Supplied grid is not of same dimension");
            }

            weatherGrid.assign(gsWeatherGrid);
            setWeatherGrid(weatherGrid);

            this.keys = new WeatherKey[slice.keys.length];
            System.arraycopy(slice.keys, 0, this.keys, 0, slice.keys.length);
        } else {
            setWeatherGrid(null);
            this.keys = new WeatherKey[0];
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
        if (!(rhs instanceof WeatherGridSlice)) {
            return false;
        }

        if (!super.equals(rhs)) {
            return false;
        }

        WeatherGridSlice slice = (WeatherGridSlice) rhs;
        Grid2DByte grid = getWeatherGrid();
        Grid2DByte rhsGrid = slice.getWeatherGrid();

        if (grid == null) {
            if (rhsGrid == null) {
                return true;
            }
            return false;
        }

        if (grid.getXdim() != rhsGrid.getXdim()
                || grid.getYdim() != rhsGrid.getYdim()) {
            return false;
        }

        byte[] thisData = grid.getBuffer().array();
        byte[] rhsData = rhsGrid.getBuffer().array();
        for (int i = 0; i < thisData.length; i++) {
            if (!this.keys[0xFF & thisData[i]]
                    .equals(slice.keys[0xFF & rhsData[i]])) {
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
        Grid2DByte weatherGrid = getWeatherGrid();
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
    // TODO: Rewrite for caching
    public boolean assign(WeatherKey aValue, Grid2DBit editArea) {
        if (!aValue.isValid()) {
            return false;
        }

        Grid2DByte weatherGrid = getWeatherGrid();
        Point gridSize = new Point(weatherGrid.getXdim(), weatherGrid.getYdim());
        if (editArea.getXdim() != gridSize.x
                || editArea.getYdim() != gridSize.y) {
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

        setWeatherGrid(weatherGrid);

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
    public boolean assign(WeatherGridSlice gs, Grid2DBit editArea) {
        Grid2DByte weatherGrid = getWeatherGrid();
        if (editArea.getXdim() != weatherGrid.getXdim()
                || editArea.getYdim() != weatherGrid.getYdim()) {
            return false;
        }

        Grid2DByte gsWeatherGrid = gs.getWeatherGrid();
        Point ll = new Point();
        Point ur = new Point();
        editArea.extremaOfSetBits(ll, ur);

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    // Get the WeatherKey from the source grid
                    byte dByte = gsWeatherGrid.get(i, j);
                    WeatherKey dKey = gs.keys[0xFF & dByte];
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

        setWeatherGrid(weatherGrid);

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

        Grid2DByte weatherGrid = getWeatherGrid();
        Arrays.fill(weatherGrid.getBuffer().array(), dByte);
        setWeatherGrid(weatherGrid);

        return true;
    }

    /**
     * Assigns the specified value to the GridSlice.
     * 
     * @param gs
     *            grid slice to assign from
     * @return result of assignment
     */
    public boolean assign(WeatherGridSlice gs) {
        super.assign(gs);
        Grid2DByte weatherGrid = gs.weatherGrid.clone();

        List<WeatherKey> currentKeys = new ArrayList<WeatherKey>(
                Arrays.asList(this.keys));
        byte[] data = weatherGrid.getBuffer().array();
        int thisB;
        for (int i = 0; i < data.length; i++) {
            thisB = 0xFF & data[i];
            byte keyIndex;
            if ((keyIndex = (byte) currentKeys.indexOf(gs.keys[thisB])) != -1) {
                data[i] = keyIndex;
            } else {
                data[i] = (byte) currentKeys.size();
                currentKeys.add(new WeatherKey(gs.keys[thisB]));
            }
        }

        setWeatherGrid(weatherGrid);
        this.keys = currentKeys.toArray(new WeatherKey[currentKeys.size()]);

        return true;
    }

    /**
     * Returns a Grid2DBit that corresponds to the gridcells that are equal to
     * the specified WeatherKey value.
     * 
     * @param value
     *            the WeatherKey to test for
     * @return a Grid2DBit with bits set that match the input WeatherKey
     */
    public Grid2DBit eq(WeatherKey value) {
        if (!value.isValid()) {
            throw new IllegalArgumentException("Supplied key is invalid");
        }

        Grid2DByte weatherGrid = getWeatherGrid();
        Point gridSize = new Point(weatherGrid.getXdim(), weatherGrid.getYdim());
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
        Grid2DByte weatherGrid = getWeatherGrid();
        Point gridSize = new Point(weatherGrid.getXdim(), weatherGrid.getYdim());
        Grid2DBit bits = new Grid2DBit(gridSize.x, gridSize.y);

        // Check for each value
        // Get the list of subkey permutations from the value given
        String siteId = getGridInfo().getGridLoc().getSiteId();
        List<WeatherSubKey> searchKeys = WeatherKey.descriptionSubKeys(siteId,
                value);

        // Get the byte values that correspond to the specified textStrings
        List<Byte> byteValues = new ArrayList<Byte>();

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

        if (byteValues.size() == 0) {
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

    public Grid2DBit almost(WeatherGridSlice gs, float fuzz) {
        Grid2DByte weatherGrid = getWeatherGrid();
        return new Grid2DBit(weatherGrid.getXdim(), weatherGrid.getYdim());
    }

    public Grid2DBit almost(WeatherGridSlice gs) {
        return almost(gs, 0);
    }

    /**
     * Returns a Grid2DBit whose bits are set where ever the specified GridSlice
     * is equal to the same value as this GridSlice.
     * 
     * @param gs
     * @return the resulting grid
     */
    public Grid2DBit eq(WeatherGridSlice gs) {
        Grid2DByte weatherGrid = getWeatherGrid();
        Grid2DBit bits = new Grid2DBit(weatherGrid.getXdim(),
                weatherGrid.getYdim());

        byte[] thisB = weatherGrid.getBuffer().array();
        byte[] rhsB = gs.getWeatherGrid().getBuffer().array();
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
     * @return the resulting grid
     */
    public Grid2DBit notEq(WeatherGridSlice gs) {
        Grid2DBit bits = eq(gs);
        bits.negate();
        return bits;
    }

    /**
     * Collapses the discrete key/grid to not contain extra key definitions.
     */
    @Override
    public void collapse() {
        Grid2DByte weatherGrid = getWeatherGrid();
        if (weatherGrid == null) {
            return;
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
                used[0xFF & weatherGrid.get(i, j)] = true;
            }
        } // indicate used

        // clear the invmapping if not used
        for (int i = 0; i < used.length; i++) {
            if (!used[i]) {
                invMapping[i] = -1;
            }
        }

        // eliminate duplicate keys
        int nk = 0;
        List<WeatherKey> tmpKeys = new ArrayList<WeatherKey>();
        for (int i = 0; i < used.length; i++) {
            if (used[i]) {
                tmpKeys.add(keys[i]);
                invMapping[i] = nk;
                for (int j = i + 1; j < used.length; j++) {
                    if (keys[i].equals(keys[j])) {
                        invMapping[j] = nk; // key index
                        used[j] = false; // to prevent reprocessing
                    }
                }
                nk++;
            }
        }
        WeatherKey[] newKeys = tmpKeys.toArray(new WeatherKey[tmpKeys.size()]);

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

        // store the grid
        setWeatherGrid(weatherGrid);
        keys = newKeys;
    }

    @Override
    public WeatherGridSlice clone() throws CloneNotSupportedException {
        TimeRange aValidTime = this.validTime.clone();
        GridParmInfo aGpi = this.gridParmInfo.clone();
        GridDataHistory[] aHistory = new GridDataHistory[this.gridDataHistory
                .size()];
        for (int i = 0; i < aHistory.length; i++) {
            aHistory[i] = this.gridDataHistory.get(i).clone();
        }

        Grid2DByte aGrid = getWeatherGrid();
        if (aGrid != null) {
            aGrid = aGrid.clone();
        }

        WeatherKey[] aKey = new WeatherKey[this.keys.length];
        for (int i = 0; i < aKey.length; i++) {
            aKey[i] = new WeatherKey(keys[i]);
        }
        return new WeatherGridSlice(aValidTime, aGpi, aHistory, aGrid, aKey);
    }

    @Override
    public String toString() {
        StringBuilder rVal = new StringBuilder(super.toString());
        rVal.append("Weather grid: ").append(getWeatherGrid()).append("\n");
        rVal.append("Weather keys: ").append(getKeys()).append("\n");
        return rVal.toString();
    }

    private String checkDims() {
        Grid2DByte weatherGrid = getWeatherGrid();
        if (weatherGrid == null) {
            return "Grid data not populated";
        }

        int x = weatherGrid.getXdim();
        int y = weatherGrid.getYdim();

        if (x != gridParmInfo.getGridLoc().getNx()
                || y != gridParmInfo.getGridLoc().getNy()) {
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
    public Grid2DByte getWeatherGrid() {
        if (useCache && cacheId != null) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                return (Grid2DByte) diskCache.getFromCache(cacheId);
            } catch (CacheException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to load data from GFE cache.", e);
            }
        }

        return this.weatherGrid;
    }

    /**
     * Return the discrete key
     * 
     * @return the discrete key
     */
    public WeatherKey[] getKeys() {
        return this.keys;
    }

    public void setWeatherGrid(Grid2DByte grid) {
        this.weatherGrid = grid;

        if (useCache) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                if (this.weatherGrid != null) {
                    if (cacheId != null) {
                        diskCache.addToCache(cacheId, this.weatherGrid);
                    } else {
                        cacheId = diskCache.addToCache(this.weatherGrid);
                    }
                } else if (cacheId != null) {
                    // scalarGrid is null, remove previous cache entry
                    diskCache.removeFromCache(cacheId);
                    cacheId = null;
                }

                this.weatherGrid = null;
            } catch (Exception e) {
                // failed to move to local cache, don't remove from memory
                statusHandler.handle(Priority.WARN,
                        "Failed to move data to local cache", e);
            }
        }
    }

    public void setKeys(WeatherKey[] keys) {
        this.keys = keys;
    }

    public void setKey(List<WeatherKey[]> key) {
        setKeys(key.toArray(new WeatherKey[] {}));
    }

    @Override
    public Object[] getNumPy() {
        Object[] numpy = new Object[2];
        numpy[0] = getWeatherGrid().getBuffer().array();
        List<String> keyList = new ArrayList<String>();
        for (WeatherKey k : keys) {
            keyList.add(k.toString());
        }
        String pyList = PyUtil.listToList(keyList);
        numpy[1] = pyList;
        return numpy;
    }

    @Override
    public int getNumpyX() {
        return getWeatherGrid().getXdim();
    }

    @Override
    public int getNumpyY() {
        return getWeatherGrid().getYdim();
    }

    @Override
    protected void moveDataToLocalCache() {
        setWeatherGrid(getWeatherGrid());
    }

    @Override
    protected void moveDataToMem() {
        if (cacheId != null) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                this.weatherGrid = (Grid2DByte) diskCache.getFromCache(cacheId);
                diskCache.removeFromCache(cacheId);
            } catch (CacheException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to load data from GFE cache.", e);
                return;
            }
        }

        cacheId = null;
    }

    public boolean isPopulated() {
        if (useCache) {
            return cacheId != null;
        }

        return weatherGrid != null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#finalize()
     */
    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        if (cacheId != null) {
            @SuppressWarnings("unchecked")
            ICache<IGrid2D> diskCache = CacheFactory.getInstance().getCache(
                    "GFE");
            diskCache.removeFromCache(cacheId);
        }
    }
}
