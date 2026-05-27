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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.GridMapKey;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.CubeLevel;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.grid.GridExtension;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.rsc.AbstractRadarResource;
import com.raytheon.viz.radar.rsc.image.RadarRadialResource;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 *
 * Hooks radar data and TILT levels into the grid plugin.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 15, 2017  6332     bsteffen  Initial creation
 * Jul 17, 2020  17574    smoorthy  get true elevation angle and send with
 *                                  TiltRequest
 * Mar 29, 2021  8374     randerso  Updated for changes to TiltRequest
 *                                  constructor
 * Jul 07, 2021  8576     randerso  Changed RadarAdapter to support multiple
 *                                  local radars as defined in radarsInUse.txt
 * Jul 26, 2021  8600     randerso  Include icao in update key
 * Sep 08, 2021  8652     njensen   Added check for RCP to resolvePluginStaticData()
 * Nov 14, 2022  8973     mapeters  Sync access to resources' radar records
 * May 22, 2024  2037092  mapeters  Minor cleanup in getUpdateKey()
 * Oct 14, 2024  2037939  mapeters  Update timeInvariantQuery for radar->radar-kxxx
 *                                  changes done under #8576, add custom
 *                                  TiltTemporalGridDataLevelNode
 *
 * </pre>
 *
 * @author bsteffen
 */
public class RadarGridExtension implements GridExtension {

    private static final String TILT = "TILT";

    @Override
    public void addToBaseTree(DataTree dataTree,
            Map<String, DerivParamDesc> derParLibrary) {
        RadarAdapter.getInstance().addRadarBaseTree(dataTree, derParLibrary);
    }

    @Override
    public Set<DataTime> timeInvariantQuery(
            Map<String, RequestConstraint> query) throws VizException {
        RequestConstraint datasetIdConstraint = query
                .get(GridConstants.DATASET_ID);
        if (datasetIdConstraint != null) {
            String icao = RadarAsGridUtil.getIcaoFromModelName(
                    datasetIdConstraint.getConstraintValue());
            if (icao != null) {
                return RadarAdapter.getInstance().timeInvariantQuery(icao);
            }
        }
        return Set.of();
    }

    @Override
    public String get3DMasterLevel(String model) {
        if (model.startsWith(RadarAdapter.RADAR_SOURCE)) {
            return RadarAdapter.CUBE_MASTER_LEVEL_NAME;
        }
        return null;
    }

    @Override
    public LevelNode getCubeNode(String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels) {
        if (modelName.startsWith(RadarAdapter.RADAR_SOURCE)) {
            return new RadarCubeLevelNode(cubeLevels, modelName);
        }
        return null;
    }

    @Override
    public Object resolvePluginSpecifiedField(SourceNode sourceNode,
            Level level, DerivParamMethod method, DerivParamField field) {
        String sourceValue = sourceNode.getValue();
        String fieldParamAbbrev = field.getParam();
        if (TILT.equals(level.getMasterLevel().getName())) {
            if (TILT.equals(fieldParamAbbrev)) {
                if (RadarAsGridUtil.isRadarModelName(sourceValue)) {
                    return new TiltTemporalGridDataLevelNode(sourceValue,
                            level);
                } else {
                    return new TiltStaticGridDataLevelNode(sourceValue, level);
                }
            } else if (RadarAsGridUtil.RCP.equals(fieldParamAbbrev)
                    && RadarAsGridUtil.isRadarModelName(sourceValue)) {
                return new RcpGridDataLevelNode(sourceValue, level);
            }
        }
        return null;
    }

    @Override
    public GridMapKey getUpdateKey(AbstractBaseDataNode node) {
        if (node instanceof RadarRequestableLevelNode) {
            RadarRequestableLevelNode rNode = (RadarRequestableLevelNode) node;
            Level level = rNode.getLevel();
            Map<String, Object> gribMap = new HashMap<>();
            gribMap.put(GridConstants.DATASET_ID,
                    RadarAsGridUtil.getModelNameForIcao(rNode.getIcao()));
            gribMap.put(GridConstants.PARAMETER_ABBREVIATION,
                    rNode.getParamAbbrev());
            gribMap.put(GridConstants.MASTER_LEVEL_NAME,
                    level.getMasterLevel().getName());
            gribMap.put(GridConstants.LEVEL_ONE, level.getLevelonevalue());
            gribMap.put(GridConstants.LEVEL_TWO, level.getLeveltwovalue());

            return new GridMapKey(gribMap);
        }
        return null;
    }

    @Override
    public IDataRecord[] loadCustomData(GridRecord record,
            IDescriptor descriptor) throws VizException {
        if (TILT.equals(record.getLevel().getMasterLevel().getName())) {
            Coordinate tiltLoc = findTiltLocation(descriptor.getResourceList());
            Double trueElevationAngle = getTrueElevationAngle(
                    descriptor.getResourceList(),
                    record.getLevel().getLevelonevalue());
            if (tiltLoc != null && trueElevationAngle != null) {
                TiltRequest request = new TiltRequest();
                request.setTiltLocation(tiltLoc);
                request.setTrueElevationAngle(trueElevationAngle);
                try {
                    return DataCubeContainer.getDataRecord(record, request,
                            null);
                } catch (DataCubeException e) {
                    throw new VizException(e);
                }
            }
        }
        return null;

    }

    private Coordinate findTiltLocation(ResourceList resourceList) {
        for (ResourcePair rp : resourceList) {
            AbstractResourceData resourceData = rp.getResourceData();
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource instanceof AbstractRadarResource) {
                return ((AbstractRadarResource<?>) resource).getRadarLocation();
            }
            if (resourceData instanceof IResourceGroup) {
                Coordinate tiltLoc = findTiltLocation(
                        ((IResourceGroup) resourceData).getResourceList());
                if (tiltLoc != null) {
                    return tiltLoc;
                }
            }
        }
        return null;
    }

    // e.g trim 1.30000012 to 1.3
    private double trimFloat(float angle) {

        String angleString = String.format("%.1f", angle);
        double doubleAngle = Double.parseDouble(angleString);
        return doubleAngle;
    }

    /**
     * Find the true elevation angle that corresponds to the given primary
     * elevation angle, based on what we have access to
     *
     * @param resourceList
     * @param primaryElevationAngle
     */
    private Double getTrueElevationAngle(ResourceList resourceList,
            double primaryElevationAngle) {

        for (ResourcePair rp : resourceList) {

            AbstractVizResource<?, ?> resource = rp.getResource();

            if (resource instanceof RadarRadialResource) {
                Double trueElevationAngle = ((AbstractRadarResource<?>) resource)
                        .processRadarRecords(radarRecords -> {
                            for (VizRadarRecord r : radarRecords.values()) {
                                if (r.getPrimaryElevationAngle() == primaryElevationAngle) {
                                    return trimFloat(r.getTrueElevationAngle());
                                }
                            }
                            return null;
                        });

                if (trueElevationAngle != null) {
                    return trueElevationAngle;
                }
            }
        }
        return null;
    }
}