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
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.grid.data.TopoRequestableData;
import com.raytheon.viz.radar.util.StationUtils;

import si.uom.SI;

/**
 * Handles pulling/mapping of radar data to grib parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ------------------------------------
 * Mar 23, 2010  4473     rjpeter    Initial creation
 * Feb 21, 2014  16744    dfriedman  Add getUpdateConstraints
 * Apr 01, 2014  17220    dfriedman  Handle uninitialized grid inventory
 * Sep 09, 2014  3356     njensen    Remove CommunicationException
 * Aug 15, 2017  6332     bsteffen   Move to viz.grid.radar plugin
 * Jan 24, 2018  6907     bsteffen   Increase grid resolution.
 * Apr 15, 2019  7596     lsingh     Upgraded javax.measure to JSR-363. 
 *                                   Handled unit conversion.
 * 
 * </pre>
 * 
 * @author rjpeter
 */
public class RadarAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarAdapter.class);

    public static final String RADAR_SOURCE = "radar";

    public static final String CUBE_MASTER_LEVEL_NAME = "TILT";

    private static final String PLUGIN_NAME_QUERY = "pluginName";

    public static final String ICAO_QUERY = "icao";

    public static final String PRODUCT_CODE_QUERY = "productCode";

    public static final String TILT_QUERY = "primaryElevationAngle";

    private static final String RDA_ID_QUERY = "location.rdaId";

    /**
     * Number of x/y grid points in radar grid.
     */
    private static final int GRID_SIZE = 400;

    /**
     * Spacing of grid points in meters.
     */
    private static final int GRID_SPACING = 2000;

    private static final RadarAdapter instance;

    private RadarStation configuredRadar = null;

    private GridCoverage coverage = null;

    static {
        instance = new RadarAdapter();
    }

    public static RadarAdapter getInstance() {
        return instance;
    }

    private RadarAdapter() {

    }

    private boolean checkConfiguredRadar() {
        boolean status = false;
        RadarStation station = StationUtils.getInstance().getHomeRadarStation();
        if (station != null) {
            if (configuredRadar == null
                    || !configuredRadar.getRdaId().equals(station.getRdaId())) {
                configuredRadar = station;
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
                coverage.setName(station.getRdaId() + " Generated Coverage");
                try {
                    MathTransform toLatLon = MapUtil.getTransformToLatLon(crs);
                    int minExtent = -1 * GRID_SPACING * GRID_SIZE / 2;
                    DirectPosition2D lowerLeft = new DirectPosition2D(minExtent,
                            minExtent);
                    toLatLon.transform(lowerLeft, lowerLeft);
                    coverage.setFirstGridPointCorner(Corner.LowerLeft);
                    coverage.setLa1(lowerLeft.getY());
                    coverage.setLo1(lowerLeft.getX());
                } catch (Exception e) {
                    // shouldn't occur since parsing well known geometry
                    statusHandler.debug("Failed to populate radar coverage.",
                            e);
                }
                try {
                    coverage.initialize();
                } catch (GridCoverageException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    configuredRadar = null;
                    return status;
                }
                this.coverage = GridCoverageLookup.getInstance()
                        .getCoverage(coverage, true);
                CoverageUtils.getInstance().setCoverage(RADAR_SOURCE,
                        this.coverage);
                status = true;
            }
        } else {
            configuredRadar = null;
        }
        return status;
    }

    public void addRadarBaseTree(DataTree dataTree,
            Map<String, DerivParamDesc> derParLibrary) {
        if (dataTree == null) {
            return;
        }
        checkConfiguredRadar();

        if (configuredRadar == null) {
            return;
        }

        DataTree radarTree = null;
        GetRadarDataTreeRequest request = new GetRadarDataTreeRequest();
        request.setRdaId(configuredRadar.getRdaId());

        try {
            Object response = ThriftClient.sendRequest(request);
            if (response != null && response instanceof DataTree) {
                radarTree = (DataTree) response;
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve data tree for home radar", e);
        }

        String icao = null;
        RadarProductCodeMapping pCodeMapping = RadarProductCodeMapping
                .getInstance();
        SourceNode gridSourceNode = new SourceNode();
        gridSourceNode.setValue(RADAR_SOURCE);
        gridSourceNode.setDt(60);
        initTopoParam(gridSourceNode);
        dataTree.getSourceNodes().put(RADAR_SOURCE, gridSourceNode);

        // should never have more than one node...
        for (SourceNode sNode : radarTree.getSourceNodes().values()) {
            initTopoParam(sNode);
            if (icao == null) {
                icao = sNode.getValue();
            }

            Set<String> parameterAbbrevs = pCodeMapping.getParameterAbbrevs();
            // Generate the projection information for the radar and set into
            // each of the nodes...
            for (String paramAbbrev : parameterAbbrevs) {
                List<Integer> productCodes = pCodeMapping
                        .getProductCodesForAbbrev(paramAbbrev);
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
                                        RADAR_SOURCE,
                                        new ArrayList<AbstractRequestableNode>(
                                                productCodes.size()),
                                        false);
                                gridParameterNode.addChildNode(gridLevelNode);
                            }

                            Map<String, RequestConstraint> rcMap = new HashMap<>();
                            rcMap.put(PLUGIN_NAME_QUERY,
                                    new RequestConstraint(RADAR_SOURCE));
                            rcMap.put(ICAO_QUERY, new RequestConstraint(icao));
                            rcMap.put(PRODUCT_CODE_QUERY, new RequestConstraint(
                                    pCodeParamNode.getValue()));
                            rcMap.put(TILT_QUERY, new RequestConstraint(
                                    Double.toString(l.getLevelonevalue())));

                            RadarRequestableLevelNode radarLevelNode = new RadarRequestableLevelNode(
                                    pCodeLevelNode, rcMap, paramAbbrev,
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

        TopoRequestableData topoData = new TopoRequestableData(
                modelNameNode.getValue());
        topoData.setSpace(getCoverage());
        StaticDataLevelNode topoNode = new StaticDataLevelNode(sfc, topo,
                topoData, modelNameNode.getValue());
        topoNode.setLevel(sfc);
        topoParam.addChildNode(topoNode);
        modelNameNode.addChildNode(topoParam);
    }

    public Set<DataTime> timeInvariantQuery() throws VizException {
        Set<DataTime> lastTimeQuery = RadarUpdater.getInstance()
                .getGlobalTimes();
        if (lastTimeQuery != null) {
            return lastTimeQuery;
        }
        Set<DataTime> rval = null;
        if (configuredRadar != null) {
            Map<String, RequestConstraint> newQuery = new HashMap<>();
            newQuery.put(PLUGIN_NAME_QUERY,
                    new RequestConstraint(RADAR_SOURCE));
            newQuery.put(RDA_ID_QUERY,
                    new RequestConstraint(configuredRadar.getRdaId()));

            DataTime[] times = CatalogQuery.performTimeQuery(newQuery, false,
                    null);
            if (times != null) {
                rval = new HashSet<>();
                for (DataTime time : times) {
                    rval.add(time);
                }
            }
        }
        RadarUpdater.getInstance().setGlobalTimes(rval);
        return rval;
    }

    public static int getGridSize() {
        return GRID_SIZE;
    }

    public static int getGridSpacing() {
        return GRID_SPACING;
    }

    public GridCoverage getCoverage() {
        return coverage;
    }

    /**
     * @return the configuredRadar
     */
    public RadarStation getConfiguredRadar() {
        return configuredRadar;
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
        RadarStation radarStation = getConfiguredRadar();
        if (radarStation == null) {
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
        rcMap.put(ICAO_QUERY,
                new RequestConstraint(radarStation.getRdaId().toLowerCase()));
        rcMap.put(PRODUCT_CODE_QUERY,
                new RequestConstraint(
                        Arrays.toString(
                                new ArrayList<>(productCodes).toArray()),
                        RequestConstraint.ConstraintType.IN));
        return rcMap;
    }
}
