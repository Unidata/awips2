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
package com.raytheon.uf.viz.cloudheight.impl;

import java.awt.Point;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grib.util.GridModel;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.viz.core.map.GeoUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Grib model cloud height sounding implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ModelCloudHeightSourceImplementation extends
        AbstractCloudHeightSourceImpl {

    private Map<DataTime, VerticalSounding[]> soundingMap = new HashMap<DataTime, VerticalSounding[]>();

    private Job hdf5Job = null;

    public ModelCloudHeightSourceImplementation(SoundingSource source) {
        super(source);
    }

    /**
     * Create a sounding at the grid index, given the pdos to use
     * 
     * @param index
     * @param objects
     * @param derived
     * @return
     */
    private VerticalSounding createSounding(int index,
            List<PluginDataObject> objects) {

        Map<Double, SoundingLayer> layerMap = new HashMap<Double, SoundingLayer>();

        for (PluginDataObject pdo : objects) {
            float[] data = (float[]) pdo.getMessageData();
            if (data != null) {
                Double level = ((GribRecord) pdo).getModelInfo()
                        .getLevelOneValue();
                SoundingLayer layer = layerMap.get(level);
                if (layer == null) {
                    layer = new SoundingLayer();
                    layer.setPressure(level.floatValue());
                    layerMap.put(level, layer);
                }

                float val = data[index];
                String param = ((GribRecord) pdo).getModelInfo()
                        .getParameterAbbreviation();

                if ("T".equals(param)) {
                    layer.setTemperature(val);
                } else if ("GH".equals(param)) {
                    layer.setGeoHeight(val);
                } else if ("DpT".equals(param)) {
                    layer.setDewpoint(val);
                }
            }
        }

        VerticalSounding sounding = new VerticalSounding();
        sounding.setStationId(source.getName());
        sounding.setDataTime(objects.get(0).getDataTime());
        for (SoundingLayer layer : layerMap.values()) {
            sounding.addLayer(layer);
        }

        return sounding;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.cloudheight.AbstractCloudHeightSourceImpl#
     * constructMetadataMap()
     */
    @Override
    protected HashMap<String, RequestConstraint> constructMetadataMap() {
        HashMap<String, RequestConstraint> requestMap = new HashMap<String, RequestConstraint>();
        requestMap.put("pluginName", new RequestConstraint("grib"));
        requestMap.put("modelInfo.modelName",
                new RequestConstraint(source.getName()));

        RequestConstraint params = new RequestConstraint();
        params.setConstraintType(ConstraintType.IN);
        params.addToConstraintValueList("T");
        params.addToConstraintValueList("GH");
        params.addToConstraintValueList("DpT");
        requestMap.put("modelInfo.parameterAbbreviation", params);
        requestMap.put("modelInfo.level.masterLevel.name",
                new RequestConstraint("MB"));
        requestMap.put("modelInfo.level.leveltwovalue", new RequestConstraint(
                Level.getInvalidLevelValueAsString()));
        return requestMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.cloudheight.AbstractCloudHeightSourceImpl#
     * createSoundingInternal(com.vividsolutions.jts.geom.Coordinate,
     * com.raytheon.uf.common.time.DataTime)
     */
    @Override
    protected synchronized VerticalSounding createSoundingInternal(
            Coordinate latLon, final DataTime time,
            final PluginDataObject[] pdos) {
        GribRecord pdo = (GribRecord) pdos[0];

        final ISpatialObject spatial = pdo.getSpatialObject();

        Point p = null;
        try {
            p = PointUtil.determineIndex(latLon, spatial.getCrs(),
                    MapUtil.getGridGeometry(spatial));
        } catch (Exception e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error determining index into grib record", e);
        }

        if (p == null || p.y < 0 || p.y >= spatial.getNy() || p.x < 0
                || p.x >= spatial.getNx()) {
            return null;
        }

        int index = p.y * spatial.getNx() + p.x;

        VerticalSounding[] sounding = soundingMap.get(time);
        if (sounding == null) {
            if (hdf5Job == null) {
                hdf5Job = new Job("Requesting " + source.getName() + " Data") {
                    @Override
                    protected IStatus run(IProgressMonitor monitor) {
                        long t0 = System.currentTimeMillis();

                        try {
                            DataCubeContainer.getDataRecords(
                                    Arrays.asList(pdos), Request.ALL, null);
                        } catch (VizDataCubeException e) {
                            UFStatus.getHandler().handle(Priority.PROBLEM,
                                    "Error requesting model data for sounding",
                                    e);
                        }
                        for (PluginDataObject gr : pdos) {
                            IDataRecord[] drs = (IDataRecord[]) gr
                                    .getMessageData();
                            if (drs != null && drs.length > 0) {
                                gr.setMessageData(((FloatDataRecord) drs[0])
                                        .getFloatData());
                            }
                        }

                        VerticalSounding[] loadedSounding = new VerticalSounding[spatial
                                .getNx() * spatial.getNy()];
                        soundingMap.put(time, loadedSounding);
                        System.out
                                .println("Time to request grib sounding data: "
                                        + (System.currentTimeMillis() - t0)
                                        + "ms");
                        hdf5Job = null;
                        return Status.OK_STATUS;
                    }
                };
                hdf5Job.schedule();
                return CloudHeightAlgorithm.LOADING;
            } else {
                return CloudHeightAlgorithm.LOADING;
            }
        }

        VerticalSounding vs = sounding[index];
        if (vs == null) {
            vs = createSounding(index, Arrays.asList(pdos));
            SurfaceObsLocation loc = new SurfaceObsLocation();
            loc.setStationId(vs.getStationId());
            loc.setLatitude(latLon.y);
            loc.setLongitude(latLon.x);
            vs.setSpatialInfo(loc);
            vs.setName(GeoUtil.formatCoordinate(latLon));
            sounding[index] = vs;
        }
        return vs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.cloudheight.impl.AbstractCloudHeightSourceImpl#
     * getValidTimeInterval
     * (com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected long getValidTimeInterval() {
        GridModel gm = GribModelLookup.getInstance().getModelByName(
                source.getName());
        // TODO: Why is Laps null?
        int dt = gm != null ? gm.getDt() : 1;
        // Convert hours to millis, from A1 SatPVImageDepict.C
        return ((dt * 3600 * 1000L) * 3) / 2;
    }
}
