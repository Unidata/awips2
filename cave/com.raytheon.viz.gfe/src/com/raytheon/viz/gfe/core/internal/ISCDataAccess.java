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

package com.raytheon.viz.gfe.core.internal;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IISCDataAccess;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utility class to deal with getting ISC data from conventional data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/14/09      1995       bphillip    Initial release
 * 10/31/2013    2508       randerso    Change to use DiscreteGridSlice.getKeys()
 * 09/01/2014    3572       randerso    Removed ourSiteMap as it was unused and the only 
 *                                      thing that used Grid2DBoolean
 * 04/04/2016    5539       randerso    Fix unsigned byte issues
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ISCDataAccess implements IISCDataAccess {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ISCDataAccess.class);

    /** The Data Manager instance */
    private final DataManager dataMgr;

    private Map<String, Grid2D<String>> siteGridMap;

    /**
     * Constructs a new ISCDataAccess object
     * 
     * @param dataMgr
     *            The data manager
     */
    public ISCDataAccess(DataManager dataMgr) {
        this.dataMgr = dataMgr;
        this.createSiteMask();
    }

    // returns corresponding ISC gridid, parm, grid
    @Override
    public GridID getISCGridID(GridID id, boolean exactMatch) {

        Parm p = this.getISCParm(id.getParm());

        if (p != null) {
            GridID exact = new GridID(p, id.getDate());
            if (exactMatch) {
                return exact;
            }

            // not exact match needed
            if (exact.grid() != null) {
                return exact; // but there is an exact match
            }

            // get inventory over overlapping grids in isc from primary
            IGridData primaryGrid = id.grid();
            if (primaryGrid != null) {
                // TODO: Verify this method call
                TimeRange primaryTR = primaryGrid.getGridTime();
                IGridData[] overlap = p.getGridInventory(primaryTR);
                if (overlap.length == 0) {
                    return exact; // none found
                }
                int index = 0;
                long overlapAmount = primaryTR.intersection(
                        overlap[0].getGridTime()).getDuration();
                for (int i = 1; i < overlap.length; i++) {
                    long oAmt = primaryTR
                            .intersection(overlap[i].getGridTime())
                            .getDuration();
                    if (oAmt > overlapAmount) {
                        index = i;
                        overlapAmount = oAmt;
                    }
                }
                return new GridID(p, overlap[index].getGridTime().getStart());
            } else {
                return exact; // none found
            }
        } else {
            return null;
        }
    }

    @Override
    public ParmID getISCParmID(ParmID parmID) {
        return dataMgr.getParmManager().getISCParmID(parmID);
    }

    @Override
    public Parm getISCParm(Parm p) {

        if (p == null) {
            return null;
        }

        // check for isc parm, if so, return it directly
        if (p.isIscParm()) {
            return p;
        }

        // incoming parm must ba a parm in the mutable database
        if (!p.getParmID().getDbId()
                .equals(dataMgr.getParmManager().getMutableDatabase())) {
            return null;
        }

        // get the corresponding ISC ParmID
        ParmID iscParmID = getISCParmID(p.getParmID());
        if (!iscParmID.isValid()) {
            return null;
        }

        // isc parm must exist
        Parm iscP = dataMgr.getParmManager().getParm(iscParmID);
        if (iscP == null) { // create it as an undisplayable parm.
            iscP = dataMgr.getParmManager().addParm(iscParmID, false, false);
        }
        return iscP;
    }

    @Override
    public String getISCSite(Point loc, GridID gid) {
        String empty = "";
        String officeType = gid.getParm().getOfficeType();
        Grid2D<String> siteGridMap = this.siteGridMap.get(officeType);

        if (siteGridMap != null) {
            if (siteGridMap.isValid(loc.x, loc.y)) {
                return siteGridMap.get(loc.x, loc.y);
            } else {
                return empty;
            }

        } else {
            return empty;
        }
    }

    @Override
    public WxValue getDataPoint(GridID gridID, Coordinate worldLoc)
            throws GFEServerException {
        Coordinate gloc = new Coordinate();
        return getDataPoint(gridID, worldLoc, gloc);
    }

    @Override
    public WxValue getDataPoint(GridID gridID, Coordinate worldLoc,
            Coordinate gloc) throws GFEServerException {
        if (gridID.getParm() == null) {
            return WxValue.defaultValue(gridID.getParm());
        }

        // convert to grid coordinate
        GridLocation location = gridID.getParm().getGridInfo().getGridLoc();
        boolean inGrid = false;
        gloc = MapUtil.latLonToGridCoordinate(worldLoc,
                PixelOrientation.UPPER_RIGHT, location);
        inGrid = (gloc.x >= 0) && (gloc.x < location.getNx()) && (gloc.y >= 0)
                && (gloc.y < location.getNy());
        if (!inGrid) {
            return WxValue.defaultValue(gridID.getParm());
        }

        // point within our site domain
        if (getISCSite(new Point((int) gloc.x, (int) gloc.y), gridID).equals(
                dataMgr.getSiteID())) {
            IGridData grid = gridID.grid();
            if (grid != null) {
                return grid.getWxValue((int) gloc.x, (int) gloc.y);
            } else {
                return WxValue.defaultValue(gridID.getParm());
            }

        }

        // point outside our site domain
        else {
            GridID iGrid = getISCGridID(gridID, true);
            IGridData grid = iGrid.grid();
            if (grid == null) {
                return WxValue.defaultValue(gridID.getParm());
            }
            return grid.getWxValue((int) gloc.x, (int) gloc.y);
        }

    }

    @Override
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            ScalarGridSlice slice) {

        GridLocation gridLoc = gid.getParm().getGridInfo().getGridLoc();
        int nx = gridLoc.getNx();
        int ny = gridLoc.getNy();
        boolean iscOnly = gid.getParm().getParmState().isIscParm();

        // data check
        if (!gid.getParm().getGridInfo().getGridType().equals(GridType.SCALAR)) {
            statusHandler.handle(Priority.PROBLEM,
                    "getCompositeGrid called on non-scalar parm");
            slice.setScalarGrid(new Grid2DFloat());
            return new Grid2DBit();
        }

        ScalarGridData primary = (ScalarGridData) gid.grid();
        Grid2DBit ourSiteMask = null;
        if (primary == null) {
            slice.setScalarGrid(new Grid2DFloat(nx, ny, Float.NaN));
            primary = new ScalarGridData(gid.getParm(), slice);
            ourSiteMask = new Grid2DBit(nx, ny);
        } else {
            ourSiteMask = dataMgr.getRefManager().siteGridpoints(
                    Arrays.asList(dataMgr.getSiteID()), !iscOnly);
        }

        try {
            slice.setScalarGrid(primary.getScalarSlice().getScalarGrid()
                    .clone());
        } catch (CloneNotSupportedException e) {
            statusHandler.handle(Priority.PROBLEM, "Error cloning grid ", e);
            slice.setScalarGrid(new Grid2DFloat(nx, ny, Float.NaN));
        }

        // isc grid
        GridID iscGid = getISCGridID(gid, exactMatch);

        if (iscGid == null) {
            return ourSiteMask;
        }
        ScalarGridData iscGrid = (ScalarGridData) iscGid.grid();
        if (iscGrid == null) {
            return ourSiteMask;
        }

        // get mask based on history
        Grid2DBit siteMask = dataMgr.getRefManager().siteGridpoints(
                iscGrid.getHistorySites(), iscOnly);

        // blend the isc grid into the "scalarGrid" for those points set
        Grid2DFloat isc = iscGrid.getScalarSlice().getScalarGrid();
        slice.getScalarGrid().copyWithMask(isc, siteMask);

        return siteMask.or(ourSiteMask);
    }

    /**
     * Helper method added so the correct method gets called from python
     * 
     * @param gid
     * @param exactMatch
     * @param slice
     * @return the composite slice
     */
    public Grid2DBit getVectorCompositeGrid(GridID gid, boolean exactMatch,
            VectorGridSlice slice) {
        return getCompositeGrid(gid, exactMatch, slice);
    }

    @Override
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            VectorGridSlice slice) {

        GridLocation gridLoc = gid.getParm().getGridInfo().getGridLoc();
        int nx = gridLoc.getNx();
        int ny = gridLoc.getNy();
        boolean iscOnly = gid.getParm().getParmState().isIscParm();

        // data check
        if (!gid.getParm().getGridInfo().getGridType().equals(GridType.VECTOR)) {
            statusHandler.handle(Priority.PROBLEM,
                    "getCompositeGrid called on non-vector parm");
            slice.setMagGrid(new Grid2DFloat(nx, ny, Float.NaN));
            slice.setDirGrid(new Grid2DFloat(nx, ny, Float.NaN));
            return new Grid2DBit(nx, ny);
        }
        VectorGridData primary = (VectorGridData) gid.grid();
        Grid2DBit ourSiteMask = null;
        if (primary == null) {
            slice.setMagGrid(new Grid2DFloat(nx, ny, Float.NaN));
            slice.setDirGrid(new Grid2DFloat(nx, ny, Float.NaN));
            primary = new VectorGridData(gid.getParm(), slice);
            ourSiteMask = new Grid2DBit(nx, ny);
        } else {
            ourSiteMask = dataMgr.getRefManager().siteGridpoints(
                    Arrays.asList(dataMgr.getSiteID()), !iscOnly);
        }

        try {
            slice.setMagGrid(primary.getVectorSlice().getMagGrid().clone());
            slice.setDirGrid(primary.getVectorSlice().getDirGrid().clone());
        } catch (CloneNotSupportedException e) {
            statusHandler.handle(Priority.PROBLEM, "Error cloning grid ", e);
            slice.setMagGrid(new Grid2DFloat(nx, ny, Float.NaN));
            slice.setDirGrid(new Grid2DFloat(nx, ny, Float.NaN));
        }

        // isc grid
        GridID iscGid = getISCGridID(gid, exactMatch);
        if (iscGid == null) {
            return ourSiteMask;
        }
        IGridData iscGrid = iscGid.grid();
        if (iscGrid == null) {
            return ourSiteMask;
        }

        // get mask based on history
        Grid2DBit siteMask = dataMgr.getRefManager().siteGridpoints(
                iscGrid.getHistorySites(), iscOnly);

        // blend the isc grid into the "scalarGrid" for those points set
        Grid2DFloat iscMag = ((VectorGridSlice) iscGrid.getGridSlice())
                .getMagGrid();
        Grid2DFloat iscDir = ((VectorGridSlice) iscGrid.getGridSlice())
                .getDirGrid();

        slice.getMagGrid().copyWithMask(iscMag, siteMask);
        slice.getDirGrid().copyWithMask(iscDir, siteMask);

        return siteMask.or(ourSiteMask);

    }

    @Override
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            WeatherGridSlice slice) {

        GridLocation gridLoc = gid.getParm().getGridInfo().getGridLoc();
        int nx = gridLoc.getNx();
        int ny = gridLoc.getNy();
        boolean iscOnly = gid.getParm().getParmState().isIscParm();

        // data check
        if (!gid.getParm().getGridInfo().getGridType().equals(GridType.WEATHER)) {
            statusHandler.handle(Priority.PROBLEM,
                    "getCompositeGrid called on non-discrete parm");

            slice.setWeatherGrid(new Grid2DByte());
            slice.setKeys(new WeatherKey[0]);
            return new Grid2DBit(nx, ny);
        }

        WeatherGridData primary = (WeatherGridData) gid.grid();
        Grid2DBit ourSiteMask = null;
        if (primary == null) {
            slice.setWeatherGrid(new Grid2DByte(nx, ny));
            slice.setKeys(new WeatherKey[0]);
            primary = new WeatherGridData(gid.getParm(), slice);
            ourSiteMask = new Grid2DBit(nx, ny);
        } else {
            ourSiteMask = dataMgr.getRefManager().siteGridpoints(
                    Arrays.asList(dataMgr.getSiteID()), !iscOnly);
        }

        slice.setWeatherGrid(primary.getWeatherSlice().getWeatherGrid().clone());
        WeatherKey[] keys = new WeatherKey[primary.getWeatherSlice().getKeys().length];
        for (int i = 0; i < keys.length; i++) {
            keys[i] = new WeatherKey(primary.getWeatherSlice().getKeys()[i]);
        }

        slice.setKeys(keys);
        keys = null; // don't use this copy any more

        // isc grid
        GridID iscGid = getISCGridID(gid, exactMatch);
        if (iscGid == null) {
            return ourSiteMask;
        }
        WeatherGridData iscGridData = (WeatherGridData) iscGid.grid();
        if (iscGridData == null) {
            return ourSiteMask;
        }

        // get mask based on history
        Grid2DBit siteMask = dataMgr.getRefManager().siteGridpoints(
                iscGridData.getHistorySites(), iscOnly);

        // blend the isc grid into the grid/key for those points set
        Grid2DByte iscGrid = iscGridData.getWeatherSlice().getWeatherGrid();

        WeatherKey[] iscKey = iscGridData.getWeatherSlice().getKeys();
        if (iscOnly) {
            slice.setKeys(iscKey);
        }

        WeatherKey[] keyList = slice.getKeys();
        Map<WeatherKey, Integer> keyIndexMap = new HashMap<WeatherKey, Integer>(
                keyList.length);
        for (int i = 0; i < keyList.length; i++) {
            keyIndexMap.put(keyList[i], new Integer(i));
        }

        for (int j = 0; j < siteMask.getYdim(); j++) {
            for (int i = 0; i < siteMask.getXdim(); i++) {
                if (siteMask.getAsBoolean(i, j)) {
                    byte index = lookupKeyValue(keyIndexMap,
                            iscKey[0xFF & iscGrid.get(i, j)]);
                    slice.getWeatherGrid().set(i, j, index);
                }
            }
        }

        WeatherKey[] newKeyList = new WeatherKey[keyIndexMap.size()];
        for (Entry<WeatherKey, Integer> key : keyIndexMap.entrySet()) {
            newKeyList[key.getValue().intValue()] = key.getKey();
        }

        slice.setKeys(newKeyList);
        return siteMask.or(ourSiteMask);
    }

    @Override
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            DiscreteGridSlice slice) {

        GridLocation gridLoc = gid.getParm().getGridInfo().getGridLoc();
        int nx = gridLoc.getNx();
        int ny = gridLoc.getNy();
        boolean iscOnly = gid.getParm().getParmState().isIscParm();

        // data check
        if (!gid.getParm().getGridInfo().getGridType()
                .equals(GridType.DISCRETE)) {
            statusHandler.handle(Priority.PROBLEM,
                    "getCompositeGrid called on non-discrete parm");

            slice.setDiscreteGrid(new Grid2DByte());
            slice.setKeys(new DiscreteKey[0]);
            return new Grid2DBit();
        }

        DiscreteGridData primary = (DiscreteGridData) gid.grid();
        Grid2DBit ourSiteMask = null;
        if (primary == null) {
            slice.setDiscreteGrid(new Grid2DByte(nx, ny));
            slice.setKeys(new DiscreteKey[0]);
            primary = new DiscreteGridData(gid.getParm(), slice);
            ourSiteMask = new Grid2DBit(nx, ny);
        } else {
            ourSiteMask = dataMgr.getRefManager().siteGridpoints(
                    Arrays.asList(dataMgr.getSiteID()), !iscOnly);
        }

        // try {
        slice.setDiscreteGrid(primary.getDiscreteSlice().getDiscreteGrid()
                .clone());
        DiscreteKey[] keys = new DiscreteKey[primary.getDiscreteSlice()
                .getKeys().length];
        for (int i = 0; i < keys.length; i++) {
            keys[i] = new DiscreteKey(primary.getDiscreteSlice().getKeys()[i]);
        }

        slice.setKeys(keys);
        keys = null; // don't use this copy any more

        // isc grid
        GridID iscGid = getISCGridID(gid, exactMatch);
        if (iscGid == null) {
            return ourSiteMask;
        }
        DiscreteGridData iscGridData = (DiscreteGridData) iscGid.grid();
        if (iscGridData == null) {
            return ourSiteMask;
        }

        // get mask based on history
        Grid2DBit siteMask = dataMgr.getRefManager().siteGridpoints(
                iscGridData.getHistorySites(), iscOnly);

        // blend the isc grid into the grid/key for those points set
        Grid2DByte iscGrid = iscGridData.getDiscreteSlice().getDiscreteGrid();

        DiscreteKey[] iscKey = iscGridData.getDiscreteSlice().getKeys();

        DiscreteKey[] keyList = slice.getKeys();
        Map<DiscreteKey, Integer> keyIndexMap = new HashMap<DiscreteKey, Integer>(
                keyList.length);
        for (int i = 0; i < keyList.length; i++) {
            keyIndexMap.put(keyList[i], new Integer(i));
        }

        for (int j = 0; j < siteMask.getYdim(); j++) {
            for (int i = 0; i < siteMask.getXdim(); i++) {
                if (siteMask.getAsBoolean(i, j)) {
                    byte index = lookupKeyValue(keyIndexMap,
                            iscKey[0xFF & iscGrid.get(i, j)]);
                    slice.getDiscreteGrid().set(i, j, index);
                }
            }
        }

        DiscreteKey[] newKeyList = new DiscreteKey[keyIndexMap.size()];
        for (Entry<DiscreteKey, Integer> key : keyIndexMap.entrySet()) {
            newKeyList[key.getValue().intValue()] = key.getKey();
        }

        slice.setKeys(newKeyList);
        return siteMask.or(ourSiteMask);
    }

    /**
     * Utility to create the site mask. Used for initial and updates.
     */
    protected void createSiteMask() {
        GridLocation gloc = dataMgr.getParmManager().compositeGridLocation();

        siteGridMap = new HashMap<String, Grid2D<String>>();

        // get list of known sites from server -- ignore any errors
        List<String> knownSites = dataMgr.knownSites();

        // get list of available ISC_xxx edit areas, keep as list of sites
        List<ReferenceID> available = dataMgr.getRefManager()
                .getAvailableSets();
        List<String> iscEAs = new ArrayList<String>();

        for (int i = 0; i < available.size(); i++) {
            String name = available.get(i).getName();

            if ((name.length() >= 5) && name.startsWith("ISC_")
                    && (name.indexOf('_', 4) == -1)) {
                String site = name.substring(4);
                if (knownSites.contains(site)) {
                    iscEAs.add(site);
                }
            }
        }

        // process each known office type
        List<String> knownOfficeTypes = dataMgr.knownOfficeTypes();

        for (String officeType : knownOfficeTypes) {
            // create the site mask that identifies the site id for each grid
            // point.
            Grid2D<String> sites = new Grid2D<String>(gloc.gridSize().x,
                    gloc.gridSize().y);

            for (String iscea : iscEAs) {

                // match the office type
                if (!dataMgr.officeType(iscea).equals(officeType)) {
                    continue; // not interested in this office type now
                }

                // get the ref set
                ReferenceID refid = new ReferenceID("ISC_" + iscea);
                ReferenceData refDat = dataMgr.getRefManager()
                        .loadRefSet(refid);

                // convert to gridpoints
                Grid2DBit bits = refDat.getGrid();

                if (bits.isAnyBitsSet()) {

                    for (int y = 0; y < bits.getYdim(); y++) {
                        for (int x = 0; x < bits.getXdim(); x++) {
                            if (bits.getAsBoolean(x, y)) {
                                sites.set(x, y, iscea);
                            }
                        }
                    }
                }
            }

            // store result in maps
            this.siteGridMap.put(officeType, sites);
        }
    }

    /**
     * This function takes a key, DiscreteKey and determines the corresponding
     * byte value in the discrete key. Allocates a new entry if necessary.
     * 
     * @param keys
     *            The keyValues array too search
     * @param keyValue
     *            The key value to search for
     * @return The byte value of the key
     */
    protected byte lookupKeyValue(Map<DiscreteKey, Integer> keys,
            DiscreteKey keyValue) {
        // first check to see if it already is in the discrete key
        Integer idx = keys.get(keyValue);
        if (idx != null) {
            return idx.byteValue();
        }

        // not in weather key, must allocate a new entry
        Integer newIdx = new Integer(keys.size());
        keys.put(keyValue, newIdx);
        return newIdx.byteValue();
    }

    /**
     * This function takes a key, WeatherKey and determines the corresponding
     * byte value in the weather key. Allocates a new entry if necessary.
     * 
     * @param keys
     *            The keyValues array too search
     * @param keyValue
     *            The key value to search for
     * @return The byte value of the key
     */
    protected byte lookupKeyValue(Map<WeatherKey, Integer> keys,
            WeatherKey keyValue) {
        // first check to see if it already is in the discrete key
        Integer idx = keys.get(keyValue);
        if (idx != null) {
            return idx.byteValue();
        }

        // not in weather key, must allocate a new entry
        Integer newIdx = new Integer(keys.size());
        keys.put(keyValue, newIdx);
        return newIdx.byteValue();
    }
}
