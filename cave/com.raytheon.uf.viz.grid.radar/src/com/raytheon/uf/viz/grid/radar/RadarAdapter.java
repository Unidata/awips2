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
package com.raytheon.uf.viz.grid.radar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.request.GetRadarDataTreeRequest;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.OrLevelNode;
import com.raytheon.uf.common.derivparam.tree.StaticDataLevelNode;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.StereographicGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.ParameterNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.level.Level.LevelType;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.grid.data.TopoRequestableData;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

import si.uom.SI;

/**
 * Handles pulling/mapping of radar data to grib parameters.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Mar 23, 2010  4473     rjpeter    Initial creation
 * Feb 21, 2014  16744    dfriedman  Add getUpdateConstraints
 * Apr 01, 2014  17220    dfriedman  Handle uninitialized grid inventory
 * Sep 09, 2014  3356     njensen    Remove CommunicationException
 * Aug 15, 2017  6332     bsteffen   Move to viz.grid.radar plugin
 * Jan 24, 2018  6907     bsteffen   Increase grid resolution.
 * Apr 15, 2019  7596     lsingh     Upgraded javax.measure to JSR-363. Handled
 *                                   unit conversion.
 * Jun 07, 2021  8453     randerso   Improve resolution of radar grids
 * Jul 07, 2021  8576     randerso   Changed RadarAdapter to support multiple
 *                                   local radars as defined in radarsInUse.txt
 * Jul 22, 2021  8601     randerso   Increased default grid resolution. Made
 *                                   GRID_SIZE and GRID_SPACING customizable via
 *                                   environment variable settings.
 * Sep 28, 2022  8937     mapeters   Ensure coverages that we pass to
 *                                   CoverageUtils are in DB
 * May 07, 2024  2036516  bines      Get all radar from db instead of just
 *                                   the local radars in radarsInUse.txt
 * May 22, 2024  2037092  mapeters   Set up virtual volume derived parameters
 * Oct 11, 2024  2037939  mapeters   Add icao param to timeInvariantQuery() and
 *                                   move DB query logic into RadarUpdater
 *
 * </pre>
 *
 * @author rjpeter
 */
public class RadarAdapter {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarAdapter.class);

    public static final String RADAR_SOURCE = "radar";

    public static final String CUBE_MASTER_LEVEL_NAME = "TILT";

    public static final String PLUGIN_NAME_QUERY = "pluginName";

    public static final String ICAO_QUERY = "icao";

    public static final String PRODUCT_CODE_QUERY = "productCode";

    public static final String TILT_QUERY = "primaryElevationAngle";

    public static final String SCAN_TYPE_QUERY = "scanType";

    private static final String RADAR_ADAPTER_GRID_SPACING = "RADAR_ADAPTER_GRID_SPACING";

    private static final String RADAR_ADAPTER_GRID_SIZE = "RADAR_ADAPTER_GRID_SIZE";

    private static final int DEFAULT_GRID_SIZE = 1840;

    private static final int DEFAULT_GRID_SPACING = 250;

    /**
     * Number of x/y grid points in radar grid.
     */
    private static final int GRID_SIZE = getEnvSetting(RADAR_ADAPTER_GRID_SIZE,
            DEFAULT_GRID_SIZE);

    /**
     * Spacing of grid points in meters.
     */
    private static final int GRID_SPACING = getEnvSetting(
            RADAR_ADAPTER_GRID_SPACING, DEFAULT_GRID_SPACING);

    private static final RadarAdapter instance = new RadarAdapter();

    private Map<String, RadarStation> configuredRadars = new HashMap<>();

    private Map<String, GridCoverage> coverages = new HashMap<>();

    private static int getEnvSetting(String varName, int defaultValue) {
        int value = defaultValue;
        String envValue = System.getenv(varName);
        if (envValue != null) {
            try {
                value = Integer.parseInt(envValue);
            } catch (NumberFormatException e) {
                String msg = String.format(
                        "Environment variable %s contains an invalid integer value: '%s'. Using default value: %d",
                        varName, envValue, defaultValue);
                statusHandler.warn(msg, e);
            }
        }
        return value;
    }

    public static RadarAdapter getInstance() {
        return instance;
    }

    private RadarAdapter() {

    }

    private boolean checkConfiguredRadars() {
        Set<String> allRadarsRdaId = RadarUtil.getAllRadarsRdaId();
        boolean status = allRadarsRdaId.equals(configuredRadars.keySet());

        if (!status) {
            configuredRadars.clear();
            coverages.clear();
            Set<RadarStation> allRadarStations = RadarUtil.getAllRadars();

            for (RadarStation station : allRadarStations) {
                String icao = station.getRdaId().toLowerCase();

                if (station != null) {
                    RadarUpdater.getInstance().clearCache();
                    ProjectedCRS crs = RadarUtil.constructCRS(station.getLat(),
                            station.getLon());
                    StereographicGridCoverage coverage = new StereographicGridCoverage();
                    coverage.setNx(GRID_SIZE);
                    coverage.setNy(GRID_SIZE);
                    coverage.setDx(GRID_SPACING);
                    coverage.setDy(GRID_SPACING);
                    coverage.setLov(station.getLon());
                    coverage.setLad(station.getLat());
                    coverage.setSpacingUnit("m");
                    coverage.setName(
                            station.getRdaId() + " Generated Coverage");
                    try {
                        MathTransform toLatLon = MapUtil
                                .getTransformToLatLon(crs);
                        int minExtent = -1 * GRID_SPACING * GRID_SIZE / 2;
                        DirectPosition2D lowerLeft = new DirectPosition2D(
                                minExtent, minExtent);
                        toLatLon.transform(lowerLeft, lowerLeft);
                        coverage.setFirstGridPointCorner(Corner.LowerLeft);
                        coverage.setLa1(lowerLeft.getY());
                        coverage.setLo1(lowerLeft.getX());
                    } catch (Exception e) {
                        // shouldn't occur since parsing well known geometry
                        statusHandler
                                .debug("Failed to populate radar coverage.", e);
                    }
                    try {
                        coverage.initialize();
                    } catch (GridCoverageException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                    configuredRadars.put(icao, station);
                    GridCoverage dbCoverage = GridCoverageLookup.getInstance()
                            .getCoverage(coverage, true);
                    coverages.put(icao, dbCoverage);
                    CoverageUtils.getInstance()
                            .setCoverage(RADAR_SOURCE + "-" + icao, dbCoverage);
                }
            }
            status = allRadarsRdaId.equals(configuredRadars.keySet());
        }
        return status;
    }

    public void addRadarBaseTree(DataTree dataTree,
            Map<String, DerivParamDesc> derParLibrary) {
        if (dataTree == null) {
            return;
        }
        checkConfiguredRadars();

        if (configuredRadars.isEmpty()) {
            return;
        }

        DataTree radarTree = null;
        GetRadarDataTreeRequest request = new GetRadarDataTreeRequest();

        try {
            Object response = ThriftClient.sendRequest(request);
            if (response instanceof DataTree) {
                radarTree = (DataTree) response;
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve data tree for local radars", e);
        }

        String icao = null;
        RadarProductCodeMapping pCodeMapping = RadarProductCodeMapping
                .getInstance();

        for (Entry<String, SourceNode> entry : radarTree.getSourceNodes()
                .entrySet()) {
            icao = entry.getKey();
            SourceNode sNode = entry.getValue();

            String radarSource = RadarAsGridUtil.getModelNameForIcao(icao);
            SourceNode gridSourceNode = new SourceNode();
            gridSourceNode.setValue(radarSource);
            gridSourceNode.setDt(60);
            initTopoParam(gridSourceNode);
            dataTree.getSourceNodes().put(radarSource, gridSourceNode);

            initTopoParam(sNode);

            Set<String> paramAbbrevs = RadarAsGridUtil
                    .addVirtualVolumeParamAbbrevs(
                            pCodeMapping.getParameterAbbrevs());
            /*
             * Generate the projection information for the radar and set into
             * each of the nodes...
             */
            for (String paramAbbrev : paramAbbrevs) {
                List<Integer> productCodes = pCodeMapping
                        .getProductCodesForAbbrev(RadarAsGridUtil
                                .getStandardParamAbbrev(paramAbbrev));

                ParameterNode gridParameterNode = new ParameterNode();
                gridParameterNode.setValue(paramAbbrev);
                DerivParamDesc desc = derParLibrary.get(paramAbbrev);
                if (desc != null) {
                    gridParameterNode.setParameterName(desc.getName());
                    if (desc.getUnit() != null) {
                        gridParameterNode
                                .setParameterUnit(desc.getUnit().toString());
                    }
                }
                gridSourceNode.addChildNode(gridParameterNode);

                // grab the associated nodes for merging
                for (Integer pCode : productCodes) {
                    ParameterNode pCodeParamNode = sNode
                            .getChildNode(pCode.toString());

                    // should this go into derived parameters to lookup
                    // units/name?
                    if (pCodeParamNode != null) {
                        for (LevelNode pCodeLevelNode : pCodeParamNode
                                .getChildNodes().values()) {
                            Level l = pCodeLevelNode.getLevel();
                            LevelNode gridLevelNode = gridParameterNode
                                    .getChildNode(pCodeLevelNode.getValue());
                            if (gridLevelNode == null) {
                                DerivParamMethod method = new DerivParamMethod();
                                method.setName("Supplement");
                                gridLevelNode = new OrLevelNode(l, desc, method,
                                        radarSource,
                                        new ArrayList<AbstractRequestableNode>(),
                                        false);
                                gridParameterNode.addChildNode(gridLevelNode);
                            }

                            RadarRequestableLevelNode radarLevelNode = new RadarRequestableLevelNode(
                                    pCodeLevelNode, icao,
                                    Integer.parseInt(pCodeParamNode.getValue()),
                                    paramAbbrev,
                                    gridParameterNode.getParameterName());
                            ((OrLevelNode) gridLevelNode)
                                    .addNodeToOrList(radarLevelNode);
                        }
                    }
                }
            }
        }

        // create static pressure, height params at all tilts
    }

    /**
     * Add the Topo param to the given node
     *
     * @param modelNameNode
     * @param level
     */
    private void initTopoParam(SourceNode modelNameNode) {
        Level sfc = LevelFactory.getInstance().getLevel("SFC", 0.0);
        DerivParamDesc topo = new DerivParamDesc();
        topo.setAbbreviation("Topo");
        topo.setName("Topography");
        topo.setUnit(SI.METRE);

        ParameterNode topoParam = new ParameterNode();
        topoParam.setParameterName("Topography");
        topoParam.setParameterUnit("m");
        topoParam.setValue("Topo");

        String modelName = modelNameNode.getValue();
        TopoRequestableData topoData = new TopoRequestableData(modelName);
        String icao = modelName.startsWith("radar-") ? modelName.substring(6)
                : modelName;
        topoData.setSpace(getCoverage(icao));
        StaticDataLevelNode topoNode = new StaticDataLevelNode(sfc, topo,
                topoData, modelNameNode.getValue());
        topoNode.setLevel(sfc);
        topoParam.addChildNode(topoNode);
        modelNameNode.addChildNode(topoParam);
    }

    /**
     * @param icao
     *            radar station icao
     * @return all volume scan times for the radar station with the given icao
     * @throws VizException
     */
    public Set<DataTime> timeInvariantQuery(String icao) throws VizException {
        return RadarUpdater.getInstance().getStationTimes(icao);
    }

    public static int getGridSize() {
        return GRID_SIZE;
    }

    public static int getGridSpacing() {
        return GRID_SPACING;
    }

    public GridCoverage getCoverage(String icao) {
        return coverages.get(icao);
    }

    /**
     * @return the configuredRadar
     */
    public Set<String> getConfiguredRadars() {
        return configuredRadars.keySet();
    }

    public static ColorMapParameters getColorMap(RadarRecord radar)
            throws VizException {
        SingleLevel level = new SingleLevel(LevelType.TILT);
        level.setValue(radar.getPrimaryElevationAngle());
        ColorMapParameters rval;
        try {
            rval = ColorMapParameterFactory.build(null,
                    radar.getProductCode().toString(), radar.getUnitObject(),
                    level);
        } catch (StyleException e) {
            throw new VizException(e.getLocalizedMessage(), e);
        }

        rval.setColorMapMax(255);
        rval.setColorMapMin(0);
        rval.setDataMax(255);
        rval.setDataMin(0);

        return rval;
    }

    public Map<String, RequestConstraint> getUpdateConstraints() {
        if (configuredRadars.isEmpty()) {
            // Can happen if grid inventory has not been initialized
            return null;
        }
        RadarProductCodeMapping rpcMap = RadarProductCodeMapping.getInstance();
        HashSet<Integer> productCodes = new HashSet<>();
        for (String abbrev : rpcMap.getParameterAbbrevs()) {
            productCodes.addAll(rpcMap.getProductCodesForAbbrev(abbrev));
        }
        Map<String, RequestConstraint> rcMap = new HashMap<>();
        rcMap.put(RadarAdapter.PLUGIN_NAME_QUERY,
                new RequestConstraint(RADAR_SOURCE));
        rcMap.put(ICAO_QUERY, new RequestConstraint(configuredRadars.keySet()));
        rcMap.put(PRODUCT_CODE_QUERY,
                new RequestConstraint(
                        Arrays.toString(
                                new ArrayList<>(productCodes).toArray()),
                        RequestConstraint.ConstraintType.IN));
        return rcMap;
    }
}
