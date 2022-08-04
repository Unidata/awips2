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
package com.raytheon.uf.viz.volumebrowser.dataplugin.grid;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableSet;
import java.util.Set;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.BoundingBox;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataplugin.level.util.LevelUtilities;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.menus.vb.ViewMenu;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.volumebrowser.dataplugin.AbstractInventoryDataCatalog;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.grid.xml.FieldDisplayTypesFactory;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.raytheon.viz.volumebrowser.vbui.MenuItemManager;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.LineString;

/**
 * Implements the IDataCatalog interface for grid data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 27, 2009  2161     lvenable  Initial creation
 * Oct 21, 2009  1711     bsteffen  Updated Baseline and Points to use new
 *                                  ToolsDataManager
 * Nov 17, 2009  3120     rjpeter   Updated to use LevelMappingFactory.
 * Jul 31, 2012  875      rferrel   Now uses points.
 * May 30, 2013  2055     bsteffen  Remove modelName from sounding pointName.
 * Dec 06, 2013  2271     mpduff    Added check for null coordinate.
 * Jan 30, 2014  2725     ekladstr  updated exception handling during move of
 *                                  derived parameters to common
 * Mar 11, 2014  2718     randerso  Changes for GeoTools 10.5
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Aug 03, 2015  3861     bsteffen  Move resource creation to ProductCreators
 * Mar 03, 2016  5439     bsteffen  Access constants through GridConstants class
 * Apr 15, 2019  7480     bhurley   Improved NSHARP auto-update
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class GridDataCatalog extends AbstractInventoryDataCatalog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridDataCatalog.class);

    /**
     * Create the product entry in the grid data catalog.
     * 
     * Uses the gribModelDataMap to determine the integer representations of the
     * selData.
     * 
     * @param selData
     *            The selected data.
     */
    @Override
    public IDataCatalogEntry getCatalogEntry(SelectedData selData) {
        if (!isValidSelection(selData)) {
            return null;
        }
        GridDataCatalogEntry catalogEntry = new GridDataCatalogEntry(selData);

        catalogEntry.modelName = selData.getSourcesKey();
        catalogEntry.paramAbbreviation = selData.getFieldsKey();
        catalogEntry.selectedPlanesKey = selData.getPlanesKey();

        return catalogEntry;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.volumebrowser.datacatalog.AbstractDataCatalog#
     * addProductParameters
     * (com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry,
     * java.util.HashMap)
     */
    @Override
    public void addProductParameters(IDataCatalogEntry entry,
            HashMap<String, RequestConstraint> parameters) {

        GridDataCatalogEntry catalogEntry = (GridDataCatalogEntry) entry;

        ViewMenu viewSelection = catalogEntry.getDialogSettings()
                .getViewSelection();

        // sounding isn't restricted to a specific parameter or level
        switch (viewSelection) {
        case SOUNDING:
            RequestConstraint params = new RequestConstraint();
            params.setConstraintType(ConstraintType.IN);
            params.setConstraintValueList(soundingParams);
            parameters.put(GridConstants.DATASET_ID,
                    new RequestConstraint(catalogEntry.modelName));
            parameters.put(GridConstants.PARAMETER_ABBREVIATION, params);
            RequestConstraint levelConstraint = new RequestConstraint(
                    new String[] { "MB", "SFC", "FHAG" });
            parameters.put(GridConstants.MASTER_LEVEL_NAME, levelConstraint);
            parameters.put(GridConstants.LEVEL_TWO, new RequestConstraint(
                    Level.getInvalidLevelValueAsString()));
            break;
        case VARVSHGT:
        case TIMEHEIGHT:
        case CROSSSECTION:
            parameters.put(GridConstants.DATASET_ID,
                    new RequestConstraint(catalogEntry.modelName));
            parameters.put(GridConstants.PARAMETER_ABBREVIATION,
                    new RequestConstraint(catalogEntry.paramAbbreviation));
            parameters.put(GridConstants.MASTER_LEVEL_NAME,
                    new RequestConstraint(FieldDisplayTypesFactory.getInstance()
                            .getVolumePlaneType(
                                    catalogEntry.paramAbbreviation)));
            parameters.put(GridConstants.LEVEL_TWO, new RequestConstraint(
                    Level.getInvalidLevelValueAsString()));
            break;
        case PLANVIEW:
        case TIMESERIES:
        default:
            parameters.put(GridConstants.DATASET_ID,
                    new RequestConstraint(catalogEntry.modelName));
            parameters.put(GridConstants.PARAMETER_ABBREVIATION,
                    new RequestConstraint(catalogEntry.paramAbbreviation));
            if (catalogEntry.getDialogSettings()
                    .getSpaceTimeSelection() == SpaceTimeMenu.SPACE) {
                parameters.put(GridConstants.MASTER_LEVEL_NAME,
                        new RequestConstraint(catalogEntry.selectedPlanesKey
                                .replace("spatial-", "")));
                parameters.put(GridConstants.LEVEL_TWO, new RequestConstraint(
                        Level.getInvalidLevelValueAsString()));
            } else {
                // Get all possible levels for the selected levels
                List<Level> selectedLevels = Collections.emptyList();
                LevelMappingFactory lmf = LevelMappingFactory.getInstance(
                        LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE);
                selectedLevels = new ArrayList<>(lmf
                        .getLevelMappingForKey(catalogEntry.selectedPlanesKey)
                        .getLevels());

                RequestConstraint masterRC = new RequestConstraint(null,
                        ConstraintType.IN);
                RequestConstraint oneRC = new RequestConstraint(null,
                        ConstraintType.IN);
                RequestConstraint twoRC = new RequestConstraint(null,
                        ConstraintType.IN);
                for (Level level : selectedLevels) {
                    masterRC.addToConstraintValueList(
                            level.getMasterLevel().getName());
                    oneRC.addToConstraintValueList(
                            Double.toString(level.getLevelonevalue()));
                    twoRC.addToConstraintValueList(
                            Double.toString(level.getLeveltwovalue()));
                }
                parameters.put(GridConstants.MASTER_LEVEL_NAME, masterRC);
                parameters.put(GridConstants.LEVEL_ONE, oneRC);
                parameters.put(GridConstants.LEVEL_TWO, twoRC);
                // }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.volumebrowser.datacatalog.AbstractDataCatalog#
     * getDisplayUnit
     * (com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry,
     * com.raytheon.uf.viz.core.rsc.DisplayType)
     */
    @Override
    protected String getDisplayUnit(IDataCatalogEntry catalogEntry,
            DisplayType displayType) {

        String displayUnit = super.getDisplayUnit(catalogEntry, displayType);

        // if we fail to get a unit from the style rules use the parameter unit
        if (displayUnit == null) {

            GridDataCatalogEntry gribDataCatalogEntry = ((GridDataCatalogEntry) catalogEntry);
            return getInventory().getParameterUnit(
                    gribDataCatalogEntry.modelName,
                    gribDataCatalogEntry.paramAbbreviation);
        }

        return displayUnit;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.AbstractDataCatalog#getPlugins
     * ()
     */
    @Override
    protected String[] getPlugins() {
        return new String[] { GridConstants.GRID };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.AbstractInventoryDataCatalog
     * #get3DLevels()
     */
    @Override
    protected Collection<? extends Level> get3DLevels() {
        ArrayList<Level> all = new ArrayList<>();

        NavigableSet<Level> tilts = LevelUtilities
                .getOrderedSetOfStandardLevels("TILT");
        if (tilts != null) {
            all.addAll(tilts);
        }

        NavigableSet<Level> pres = LevelUtilities
                .getOrderedSetOfStandardLevels("MB");
        if (pres != null) {
            all.addAll(pres);
        }

        NavigableSet<Level> theta = LevelUtilities
                .getOrderedSetOfStandardLevels("K");
        if (theta != null) {
            all.addAll(theta);
        }

        return all;
    }

    @Override
    protected Collection<String> get3DSources(String[] planes) {
        List<String> lineLetters = new ArrayList<>();
        List<Double> lats = new ArrayList<>();
        List<Double> lons = new ArrayList<>();
        List<String> pointLetters = new ArrayList<>();

        ViewMenu viewSelection = VolumeBrowserAction.getVolumeBrowserDlg()
                .getDialogSettings().getViewSelection();
        if (viewSelection == ViewMenu.PLANVIEW) {
            // no-op
        } else if (viewSelection == ViewMenu.TIMESERIES) {
            pointLetters.add(VolumeBrowserAction.getVolumeBrowserDlg()
                    .getDialogSettings().getPointsSelection().getName());
        } else {
            if (planes == null) {
                return null;
            }
            for (String plane : planes) {
                if ("LATS".equals(plane) || "LONS".equals(plane)) {
                    return null;
                } else if (plane.startsWith("Lat")) {
                    lats.add(Double.parseDouble(plane.replace("Lat", "")));
                } else if (plane.startsWith("Lon")) {
                    lons.add(Double.parseDouble(plane.replace("Lon", "")));
                } else if (plane.startsWith("Line")) {
                    lineLetters.add(plane.replace("Line", ""));
                } else if (plane.startsWith("Point")) {
                    pointLetters.add(plane.replace("Point", ""));
                }
            }
        }
        if (pointLetters.isEmpty() && lineLetters.isEmpty() && lats.isEmpty()
                && lons.isEmpty()) {
            return null;
        }
        Set<String> fileredSources = new HashSet<>();
        for (String source : getSupportedSourcesInternal()) {
            try {
                Collection<GridCoverage> coverages = CoverageUtils.getInstance()
                        .getCoverages(source);
                if (coverages == null) {
                    fileredSources.add(source);
                    continue;
                }
                for (GridCoverage coverage : coverages) {
                    GridGeometry2D gridGeom = coverage.getGridGeometry();
                    MathTransform llToCRS = MapUtil.getTransformFromLatLon(
                            gridGeom.getCoordinateReferenceSystem());
                    Envelope2D env = gridGeom.getEnvelope2D();
                    for (String letter : pointLetters) {
                        Coordinate c = PointsDataManager.getInstance()
                                .getCoordinate(letter);
                        if (c == null) {
                            break;
                        }

                        DirectPosition2D dp = new DirectPosition2D(c.x, c.y);
                        llToCRS.transform(dp, dp);
                        if (env.contains(dp.x, dp.y)) {
                            fileredSources.add(source);
                            break;
                        }
                    }
                    for (String letter : lineLetters) {
                        LineString ls = ToolsDataManager.getInstance()
                                .getBaseline(letter);
                        Envelope2D lineEnv = null;
                        for (Coordinate c : ls.getCoordinates()) {
                            DirectPosition2D dp = new DirectPosition2D(c.x,
                                    c.y);
                            llToCRS.transform(dp, dp);
                            if (lineEnv == null) {
                                lineEnv = new Envelope2D(
                                        gridGeom.getCoordinateReferenceSystem(),
                                        dp.x, dp.y, 1, 1);
                            } else {
                                lineEnv.add(dp.x, dp.y);
                            }
                        }
                        if (lineEnv.intersects((BoundingBox) env)) {
                            fileredSources.add(source);
                            break;
                        }
                    }
                    ReferencedEnvelope rEnv = new ReferencedEnvelope(env,
                            gridGeom.getCoordinateReferenceSystem());
                    rEnv = rEnv.transform(MapUtil.getLatLonProjection(), true);
                    for (Double lat : lats) {
                        if ((rEnv.getMinY() < lat) && (rEnv.getMaxY() > lat)) {
                            fileredSources.add(source);
                            break;
                        }
                    }
                    for (Double lon : lons) {
                        if ((rEnv.getMinX() < lon) && (rEnv.getMaxX() > lon)) {
                            fileredSources.add(source);
                            break;
                        }
                    }
                }
            } catch (Exception e) {
                fileredSources.add(source);
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return fileredSources;
    }

    @Override
    protected Collection<String> get3DPlanes(Collection<String> sources) {
        ViewMenu viewSelection = VolumeBrowserAction.getVolumeBrowserDlg()
                .getDialogSettings().getViewSelection();
        if ((sources == null) || sources.isEmpty()
                || (viewSelection == ViewMenu.PLANVIEW)
                || (viewSelection == ViewMenu.TIMESERIES)) {
            Set<String> results = new HashSet<>();
            results.addAll(MenuItemManager.getInstance().getLatLonKeys());
            results.addAll(PointLineUtil.getPointLineKeys());
            return results;
        }
        ToolsDataManager tdm = ToolsDataManager.getInstance();
        PointsDataManager pdm = PointsDataManager.getInstance();
        Set<String> validPlanes = new HashSet<>(sources.size());
        for (String source : sources) {
            try {
                Collection<GridCoverage> coverages = CoverageUtils.getInstance()
                        .getCoverages(source);
                if (coverages == null) {
                    Set<String> results = new HashSet<>();
                    results.addAll(
                            MenuItemManager.getInstance().getLatLonKeys());
                    results.addAll(PointLineUtil.getPointLineKeys());
                    return results;
                }
                for (GridCoverage coverage : coverages) {
                    GridGeometry2D gridGeom = coverage.getGridGeometry();
                    MathTransform llToCRS = MapUtil.getTransformFromLatLon(
                            gridGeom.getCoordinateReferenceSystem());
                    Envelope2D env = gridGeom.getEnvelope2D();
                    for (String letter : pdm.getPointNames()) {
                        Coordinate c = pdm.getCoordinate(letter);
                        DirectPosition2D dp = new DirectPosition2D(c.x, c.y);
                        llToCRS.transform(dp, dp);
                        if (env.contains(dp.x, dp.y)) {
                            validPlanes.add("Point" + letter);
                        }
                    }
                    for (String letter : tdm.getBaselineNames()) {
                        LineString ls = tdm.getBaseline(letter);
                        Envelope2D lineEnv = null;
                        for (Coordinate c : ls.getCoordinates()) {
                            DirectPosition2D dp = new DirectPosition2D(c.x,
                                    c.y);
                            llToCRS.transform(dp, dp);
                            if (lineEnv == null) {
                                lineEnv = new Envelope2D(
                                        gridGeom.getCoordinateReferenceSystem(),
                                        dp.x, dp.y, 1, 1);
                            } else {
                                lineEnv.add(dp.x, dp.y);
                            }
                        }
                        if (lineEnv.intersects((BoundingBox) env)) {
                            validPlanes.add("Line" + letter);
                        }
                    }
                    ReferencedEnvelope rEnv = new ReferencedEnvelope(env,
                            gridGeom.getCoordinateReferenceSystem());
                    rEnv = rEnv.transform(MapUtil.getLatLonProjection(), true);
                    for (String llKey : MenuItemManager.getInstance()
                            .getLatLonKeys()) {
                        if (llKey.startsWith("Lat")) {
                            double lat = Double
                                    .parseDouble(llKey.replace("Lat", ""));
                            if ((rEnv.getMinY() < lat)
                                    && (rEnv.getMaxY() > lat)) {
                                validPlanes.add(llKey);
                            }
                        } else if (llKey.startsWith("Lon")) {
                            double lon = Double
                                    .parseDouble(llKey.replace("Lon", ""));
                            if ((rEnv.getMinX() < lon)
                                    && (rEnv.getMaxX() > lon)) {
                                validPlanes.add(llKey);
                            }
                        } else {
                            validPlanes.add(llKey);
                        }
                    }
                }
            } catch (Exception e) {
                validPlanes.add(source);
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return validPlanes;
    }
}
