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
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IISCDataAccess;
import com.raytheon.viz.gfe.core.griddata.DiscreteDataObject;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorDataObject;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherDataObject;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import org.locationtech.jts.geom.Coordinate;

/**
 * Utility class to deal with getting ISC data from conventional data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 14, 2009  1995     bphillip  Initial release
 * Oct 31, 2013  2508     randerso  Change to use DiscreteGridSlice.getKeys()
 * Sep 01, 2014  3572     randerso  Removed ourSiteMap as it was unused and the
 *                                  only thing that used Grid2DBoolean
 * Apr 04, 2016  5539     randerso  Fix unsigned byte issues
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Changes to use IDataObject. Replaced clone()
 *                                  with copy(). Code cleanup
 * Jan 29, 2018  7178     randerso  Fix regression from earlier changes
 * Feb 13, 2019  7732     randerso  Remove obsolete TODOs
 *
 * </pre>
 *
 * @author bphillip
 */
public class ISCDataAccess implements IISCDataAccess {

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
                // but there is an exact match
                return exact;
            }

            // get inventory over overlapping grids in isc from primary
            IGridData primaryGrid = id.grid();
            if (primaryGrid != null) {
                TimeRange primaryTR = primaryGrid.getGridTime();
                IGridData[] overlap = p.getGridInventory(primaryTR);
                if (overlap.length == 0) {
                    // none found
                    return exact;
                }
                int index = 0;
                long overlapAmount = primaryTR
                        .intersection(overlap[0].getGridTime()).getDuration();
                for (int i = 1; i < overlap.length; i++) {
                    long oAmt = primaryTR.intersection(overlap[i].getGridTime())
                            .getDuration();
                    if (oAmt > overlapAmount) {
                        index = i;
                        overlapAmount = oAmt;
                    }
                }
                return new GridID(p, overlap[index].getGridTime().getStart());
            } else {
                // none found
                return exact;
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
        if (iscP == null) {
            // create it as an undisplayable parm.
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
        if (getISCSite(new Point((int) gloc.x, (int) gloc.y), gridID)
                .equals(dataMgr.getSiteID())) {
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
    public Pair<Grid2DBit, IGridData> getCompositeGrid(GridID gid,
            boolean exactMatch) {

        GridLocation gridLoc = gid.getParm().getGridInfo().getGridLoc();
        int nx = gridLoc.getNx();
        int ny = gridLoc.getNy();
        boolean iscOnly = gid.getParm().getParmState().isIscParm();

        // get our gridData and mask
        IGridData ourGridData = gid.grid();
        Grid2DBit ourSiteMask = null;
        if (ourGridData != null) {
            ourSiteMask = dataMgr.getRefManager().siteGridpoints(
                    Arrays.asList(dataMgr.getSiteID()), !iscOnly);
        }

        // get isc gridData and mask
        GridID iscGid = getISCGridID(gid, exactMatch);
        IGridData iscGridData = null;
        if (iscGid != null) {
            iscGridData = iscGid.grid();
        }

        Grid2DBit iscSiteMask = null;
        if (iscGridData != null) {
            iscSiteMask = dataMgr.getRefManager()
                    .siteGridpoints(iscGridData.getHistorySites(), iscOnly);
        }

        // merge the masks
        Grid2DBit mask = ourSiteMask;
        if (mask == null) {
            mask = iscSiteMask;
        } else if (iscSiteMask != null) {
            mask.orEquals(iscSiteMask);
        }
        if (mask == null) {
            mask = new Grid2DBit(nx, ny);
        }

        IGridData compositeGridData = composite(ourGridData, iscGridData,
                iscSiteMask);

        return new Pair<>(mask, compositeGridData);
    }

    private IGridData composite(IGridData ourGridData, IGridData iscGridData,
            Grid2DBit iscSiteMask) {

        if (ourGridData == null && iscGridData != null) {
            return iscGridData.copy();
        } else if (ourGridData != null && iscGridData == null) {
            return ourGridData.copy();
        } else if (ourGridData == null && iscGridData == null) {
            return null;
        }

        IGridData composite = ourGridData.copy();

        if (composite instanceof VectorGridData) {
            VectorDataObject compositeDataObject = ((VectorGridData) composite)
                    .getDataObject();
            Grid2DFloat compositeMagGrid = compositeDataObject.getMagGrid();
            Grid2DFloat compositeDirGrid = compositeDataObject.getDirGrid();

            VectorDataObject iscDataObject = ((VectorGridData) iscGridData)
                    .getDataObject();
            Grid2DFloat iscMagGrid = iscDataObject.getMagGrid();
            Grid2DFloat iscDirGrid = iscDataObject.getDirGrid();

            compositeMagGrid.copyWithMask(iscMagGrid, iscSiteMask);
            compositeDirGrid.copyWithMask(iscDirGrid, iscSiteMask);

        } else if (composite instanceof ScalarGridData) {
            Grid2DFloat compositeGrid = ((ScalarGridData) composite)
                    .getDataObject().getScalarGrid();
            Grid2DFloat iscGrid = ((ScalarGridData) iscGridData).getDataObject()
                    .getScalarGrid();
            compositeGrid.copyWithMask(iscGrid, iscSiteMask);

        } else if (composite instanceof WeatherGridData) {
            WeatherDataObject compositeDataObject = ((WeatherGridData) composite)
                    .getDataObject();
            Grid2DByte compositeGrid = compositeDataObject.getWeatherGrid();
            WeatherKey[] compositeKeys = compositeDataObject.getKeys();

            WeatherDataObject iscDataObject = ((WeatherGridData) iscGridData)
                    .getDataObject();
            Grid2DByte iscGrid = iscDataObject.getWeatherGrid();
            WeatherKey[] iscKeys = iscDataObject.getKeys();

            Map<WeatherKey, Integer> keyIndexMap = new HashMap<>(
                    compositeKeys.length);
            for (int i = 0; i < compositeKeys.length; i++) {
                keyIndexMap.put(compositeKeys[i], new Integer(i));
            }

            for (int j = 0; j < iscSiteMask.getYdim(); j++) {
                for (int i = 0; i < iscSiteMask.getXdim(); i++) {
                    if (iscSiteMask.getAsBoolean(i, j)) {
                        byte index = lookupKeyValue(keyIndexMap,
                                iscKeys[0xFF & iscGrid.get(i, j)]);
                        compositeGrid.set(i, j, index);
                    }
                }
            }

            WeatherKey[] newKeyList = new WeatherKey[keyIndexMap.size()];
            for (Entry<WeatherKey, Integer> key : keyIndexMap.entrySet()) {
                newKeyList[key.getValue().intValue()] = key.getKey();
            }

            compositeDataObject.setKeys(newKeyList);

        } else if (composite instanceof DiscreteGridData) {
            DiscreteDataObject compositeDataObject = ((DiscreteGridData) composite)
                    .getDataObject();
            Grid2DByte compositeGrid = compositeDataObject.getDiscreteGrid();
            DiscreteKey[] compositeKeys = compositeDataObject.getKeys();

            DiscreteDataObject iscDataObject = ((DiscreteGridData) iscGridData)
                    .getDataObject();
            Grid2DByte iscGrid = iscDataObject.getDiscreteGrid();
            DiscreteKey[] iscKeys = iscDataObject.getKeys();

            Map<DiscreteKey, Integer> keyIndexMap = new HashMap<>(
                    compositeKeys.length);
            for (int i = 0; i < compositeKeys.length; i++) {
                keyIndexMap.put(compositeKeys[i], new Integer(i));
            }

            for (int j = 0; j < iscSiteMask.getYdim(); j++) {
                for (int i = 0; i < iscSiteMask.getXdim(); i++) {
                    if (iscSiteMask.getAsBoolean(i, j)) {
                        byte index = lookupKeyValue(keyIndexMap,
                                iscKeys[0xFF & iscGrid.get(i, j)]);
                        compositeGrid.set(i, j, index);
                    }
                }
            }

            DiscreteKey[] newKeyList = new DiscreteKey[keyIndexMap.size()];
            for (Entry<DiscreteKey, Integer> key : keyIndexMap.entrySet()) {
                newKeyList[key.getValue().intValue()] = key.getKey();
            }

            compositeDataObject.setKeys(newKeyList);

        }
        return composite;
    }

    /**
     * Utility to create the site mask. Used for initial and updates.
     */
    protected void createSiteMask() {
        GridLocation gloc = dataMgr.getParmManager().compositeGridLocation();

        siteGridMap = new HashMap<>();

        // get list of known sites from server -- ignore any errors
        List<String> knownSites = dataMgr.knownSites();

        // get list of available ISC_xxx edit areas, keep as list of sites
        List<ReferenceID> available = dataMgr.getRefManager()
                .getAvailableSets();
        List<String> iscEAs = new ArrayList<>();

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
            Grid2D<String> sites = new Grid2D<>(gloc.gridSize().x,
                    gloc.gridSize().y);

            for (String iscea : iscEAs) {

                // match the office type
                if (!dataMgr.officeType(iscea).equals(officeType)) {
                    // not interested in this office type now
                    continue;
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
