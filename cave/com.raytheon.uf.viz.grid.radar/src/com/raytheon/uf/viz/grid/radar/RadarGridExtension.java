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

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.GridMapKey;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
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
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.grid.GridExtension;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.rsc.AbstractRadarResource;
import org.locationtech.jts.geom.Coordinate;
import com.raytheon.viz.radar.rsc.image.RadarRadialResource;

/**
 * 
 * Hooks radar data and TILT levels into the grid plugin.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 15, 2017  6332     bsteffen  Initial creation
 * Jul 17, 2020  17574    smoorthy  get true elevation angle and send with TiltRequest
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
        RequestConstraint sRC = query.get(GridConstants.DATASET_ID);
        if (sRC != null && sRC.evaluate(RadarAdapter.RADAR_SOURCE)) {
            return RadarAdapter.getInstance().timeInvariantQuery();
        } else {
            return Collections.emptySet();
        }
    }

    @Override
    public String get3DMasterLevel(String model) {
        if (model.equals(RadarAdapter.RADAR_SOURCE)) {
            return RadarAdapter.CUBE_MASTER_LEVEL_NAME;
        }
        return null;
    }

    @Override
    public LevelNode getCubeNode(String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels) {
        if (modelName.equals(RadarAdapter.RADAR_SOURCE)) {
            return new RadarCubeLevelNode(cubeLevels, modelName);
        }
        return null;
    }

    @Override
    public Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        String fieldParamAbbrev = field.getParam();
        if (level.getMasterLevel().getName().equals(fieldParamAbbrev)) {
            if (TILT.equals(fieldParamAbbrev)) {
                return new TiltGridDataLevelNode(sNode.getValue(),
                        fieldParamAbbrev, level);
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
            gribMap.put(GridConstants.DATASET_ID, RadarAdapter.RADAR_SOURCE);
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
            Double trueElevationAngle = getTrueElevationAngle(descriptor.getResourceList(), record.getDataTime(), record.getLevel().getLevelonevalue());
            if (tiltLoc != null && trueElevationAngle != null) {
                TiltRequest request = new TiltRequest();
                request.setType(Request.Type.ALL);
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

    //e.g trim 1.30000012 to 1.3
    private double trimFloat(float angle){

        String angleString = String.format("%.1f", angle);
        double doubleAngle = Double.parseDouble(angleString);
        return doubleAngle;
    }

    //find the corresponding true elevation angle based on what we have access to
    private Double getTrueElevationAngle(ResourceList resourceList, DataTime dt, double primaryElevationAngle){

        for (ResourcePair rp : resourceList) {

            AbstractResourceData resourceData = rp.getResourceData();
            AbstractVizResource<?, ?> resource = rp.getResource();

            if (resource instanceof RadarRadialResource){
                Map<DataTime,  VizRadarRecord> records = ((AbstractRadarResource<MapDescriptor>) resource).getRadarRecords();
                for (VizRadarRecord r: records.values()){
                    if (r.getPrimaryElevationAngle() == primaryElevationAngle){
                        return trimFloat(r.getTrueElevationAngle());
                    }
                 }
             }
         }
         return null;
    }
}