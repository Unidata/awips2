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
package com.raytheon.uf.viz.sounding.providers;

import java.awt.Point;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.core.map.GeoUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Grid sounding provider
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2013       2190 mschenke    Initial creation
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GridSoundingProvider extends
        AbstractVerticalSoundingProvider<PluginDataObject[]> {

    private static final String PARAM_TEMP = "T";

    private static final String PARAM_DEWPOINT = "DpT";

    private static final String PARAM_HEIGHT = "GH";

    private class GridSoundingJob extends Job {

        private final PluginDataObject[] records;

        private VerticalSounding[] soundings;

        /**
         * @param name
         */
        public GridSoundingJob(String jobName, PluginDataObject[] records) {
            super(jobName);
            this.records = records;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            long t0 = System.currentTimeMillis();

            try {
                DataCubeContainer.getDataRecords(Arrays.asList(records),
                        Request.ALL, null);
            } catch (DataCubeException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        "Error requesting model data for sounding", e);
            }

            Integer size = null;

            for (PluginDataObject gr : records) {
                Object messageData = gr.getMessageData();
                IDataRecord record = null;
                if (messageData instanceof IDataRecord[]) {
                    IDataRecord[] records = (IDataRecord[]) messageData;
                    if (records.length > 0) {
                        record = records[0];
                    }
                } else if (messageData instanceof IDataRecord) {
                    record = (IDataRecord) messageData;
                }
                if (record != null) {
                    float[] floats = ((FloatDataRecord) record).getFloatData();
                    if (size == null) {
                        size = floats.length;
                    } else if (size != floats.length) {
                        throw new IllegalStateException(
                                "Grid sounding records returned do not have same sized dimensions");
                    }
                    gr.setMessageData(floats);
                }
            }

            if (size != null) {
                this.soundings = new VerticalSounding[size];
            }
            System.out.println("Time to request grib sounding data: "
                    + (System.currentTimeMillis() - t0) + "ms");
            return Status.OK_STATUS;
        }

        public VerticalSounding[] getSoundings() {
            return soundings;
        }

    }

    private Map<DataTime, GridSoundingJob> soundings = new HashMap<DataTime, GridSoundingJob>();

    private String modelName = "Unknown";

    @Override
    protected void populateBaseConstraints(
            Map<String, RequestConstraint> constraints) {
        if (constraints.containsKey(GridConstants.DATASET_ID)) {
            modelName = constraints.get(GridConstants.DATASET_ID)
                    .getConstraintValue();
        }

        RequestConstraint params = new RequestConstraint();
        params.setConstraintType(ConstraintType.IN);
        params.addToConstraintValueList(PARAM_TEMP);
        params.addToConstraintValueList(PARAM_HEIGHT);
        params.addToConstraintValueList(PARAM_DEWPOINT);
        constraints.put(GridConstants.PARAMETER_ABBREVIATION, params);
        constraints.put(GridConstants.MASTER_LEVEL_NAME, new RequestConstraint(
                "MB"));
        constraints.put(GridConstants.LEVEL_TWO,
                new RequestConstraint(Level.getInvalidLevelValueAsString()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.sounding.providers.AbstractVerticalSoundingProvider
     * #queryForData(java.util.Map, com.raytheon.uf.common.time.DataTime,
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected PluginDataObject[] queryForData(
            Map<String, RequestConstraint> constraints, DataTime time,
            Coordinate location) {
        try {
            return DataCubeContainer.getData(constraints, time);
        } catch (DataCubeException e) {
            throw new RuntimeException("Error querying for sounding records: "
                    + constraints, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.sounding.providers.AbstractVerticalSoundingProvider
     * #createSounding(com.raytheon.uf.common.dataplugin.PluginDataObject[],
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected VerticalSounding createSounding(DataTime time,
            PluginDataObject[] records, Coordinate location) {
        GridRecord pdo = (GridRecord) records[0];

        ISpatialObject spatial = pdo.getSpatialObject();

        Point p = null;
        try {
            p = PointUtil.determineIndex(location, spatial.getCrs(),
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

        GridSoundingJob soundingJob = soundings.get(time);
        if (soundingJob == null) {
            soundingJob = new GridSoundingJob("Loading " + modelName + " Data",
                    records);
            soundings.put(time, soundingJob);
            soundingJob.schedule();
        }
        VerticalSounding vs = null;
        VerticalSounding[] sounding = soundingJob.getSoundings();
        if (sounding != null) {
            vs = sounding[index];
            if (vs == null) {
                vs = createSounding(index, records);
                SurfaceObsLocation loc = new SurfaceObsLocation();
                loc.setStationId(vs.getStationId());
                loc.setLatitude((float) location.y);
                loc.setLongitude((float) location.x);
                vs.setSpatialInfo(loc);
                vs.setName(GeoUtil.formatCoordinate(location));
                sounding[index] = vs;
            }
        }
        return vs;
    }

    /**
     * @param index
     * @param asList
     * @return
     */
    private VerticalSounding createSounding(int index,
            PluginDataObject[] records) {
        Map<Double, SoundingLayer> layerMap = new HashMap<Double, SoundingLayer>();

        for (PluginDataObject pdo : records) {
            float[] data = (float[]) pdo.getMessageData();
            if (data != null) {
                Double level = ((GridRecord) pdo).getLevel().getLevelonevalue();
                SoundingLayer layer = layerMap.get(level);
                if (layer == null) {
                    layer = new SoundingLayer();
                    layer.setPressure(level.floatValue());
                    layerMap.put(level, layer);
                }

                float val = data[index];
                String param = ((GridRecord) pdo).getParameter()
                        .getAbbreviation();

                if (PARAM_TEMP.equals(param)) {
                    layer.setTemperature(val);
                } else if (PARAM_HEIGHT.equals(param)) {
                    layer.setGeoHeight(val);
                } else if (PARAM_DEWPOINT.equals(param)) {
                    layer.setDewpoint(val);
                }
            }
        }

        VerticalSounding sounding = new VerticalSounding();
        sounding.setStationId(modelName);
        for (SoundingLayer layer : layerMap.values()) {
            sounding.addLayer(layer);
        }

        return sounding;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider#
     * getSoundingSource()
     */
    @Override
    public String getSoundingSource() {
        return GridConstants.PLUGIN_NAME;
    }

}
