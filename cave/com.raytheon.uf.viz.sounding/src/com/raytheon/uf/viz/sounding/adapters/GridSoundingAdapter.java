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
package com.raytheon.uf.viz.sounding.adapters;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.AbstractVerticalSoundingAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.topo.TopoQuery;
import com.raytheon.uf.viz.sounding.SoundingParams;
import com.raytheon.viz.core.map.GeoUtil;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Sounding adapter for grid data, used for cloud height sampling
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GridSoundingAdapter extends AbstractVerticalSoundingAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridSoundingAdapter.class);

    /** Interface for getting point information */
    private IPointSounding pointSounding;

    /** Map used for skewt sounding generation */
    private Map<DataTime, Map<Double, SoundingLayer>> soundingMap;

    /** The SurfaceObsLocation used for skewt */
    private SurfaceObsLocation location;

    /**
     * Skewt constructor
     * 
     * @param pointSounding
     *            Object which will provide information on point location and
     *            name
     */
    public GridSoundingAdapter(IPointSounding pointSounding) {
        this.pointSounding = pointSounding;
        soundingMap = new HashMap<DataTime, Map<Double, SoundingLayer>>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.sounding.adapters.AbstractVerticalSoundingAdapter
     * #createSoundings()
     */
    @Override
    public VerticalSounding[] createSoundings() {
        long t0 = System.currentTimeMillis();
        List<VerticalSounding> soundings = new ArrayList<VerticalSounding>();
        try {
            GridRecord sampleRecord = (GridRecord) objects[0];
            ISpatialObject spatial = sampleRecord.getSpatialObject();
            Point point = PointUtil.determineIndex(
                    pointSounding.getCoordinate(), spatial.getCrs(),
                    MapUtil.getGridGeometry(spatial));
            if (point.y < 0 || point.y >= spatial.getNy() || point.x < 0
                    || point.x >= spatial.getNx()) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Point is outside bounds of grid", new VizException(
                                "Point is outside bounds of grid"));
                return new VerticalSounding[0];
            }
            String name = String.format("%s pt%s %s",
                    sampleRecord.getDatasetId(), pointSounding.getPointName(),
                    GeoUtil.formatCoordinate(pointSounding.getCoordinate()));
            location = new SurfaceObsLocation(name);
            location.setGeometry(new GeometryFactory()
                    .createPoint(pointSounding.getCoordinate()));
            location.setLatitude(pointSounding.getCoordinate().y);
            location.setLongitude(pointSounding.getCoordinate().x);
            location.setElevation(new Double(TopoQuery.getInstance().getHeight(
                    pointSounding.getCoordinate())).intValue());

            // TODO: Request data for a single point for all records, which is
            // the fastest way to get data from the data cube.
            DataCubeContainer.getDataRecords(Arrays.asList(objects),
                    Request.buildPointRequest(point), null);
            for (PluginDataObject pdo : objects) {
                IDataRecord[] rec = (IDataRecord[]) pdo.getMessageData();
                if (rec != null && rec.length > 0) {
                    float[] data = (float[]) rec[0].getDataObject();
                    float val = data[0];
                    addToSoundingMap((GridRecord) pdo, val);
                }
            }
            for (DataTime time : soundingMap.keySet()) {
                VerticalSounding sounding = new VerticalSounding();
                sounding.setDataTime(time);
                sounding.setElevation(location.getElevation());
                sounding.setName(name);
                sounding.setObsTime(time.getValidTime());
                sounding.setSpatialInfo((SurfaceObsLocation) location.clone());
                sounding.setStationId(location.getStationId());
                Map<Double, SoundingLayer> layerMap = soundingMap.get(time);
                for (SoundingLayer layer : layerMap.values()) {
                    sounding.addLayer(layer);
                }

                SoundingParams params = new SoundingParams(sounding);
                sounding = params.getAnalysisData();
                sounding.checkSfcLayer();
                sounding.removeBelowSfcLayers();
                // Make sure that the surface layer is defined and all values
                // are set to a value other than MISSING before adding it to the
                // soundings list
                SoundingLayer abv = sounding.get(1);
                if (sounding.get(0).getTemperature() > SoundingLayer.NODATA
                        && abv.getTemperature() < SoundingLayer.NODATA) {
                    sounding.get(0).setTemperature(
                            (float) (Math.pow(sounding.get(0).getPressure()
                                    / abv.getPressure(), 0.286) * abv
                                    .getTemperature()));
                }
                if (sounding.get(0).getDewpoint() > SoundingLayer.NODATA
                        && abv.getDewpoint() < SoundingLayer.NODATA) {
                    sounding.get(0).setDewpoint(abv.getDewpoint());
                }
                if (sounding.get(0).getWindU() > SoundingLayer.NODATA
                        && abv.getWindU() < SoundingLayer.NODATA) {
                    sounding.get(0).setWindU(abv.getWindU());
                }
                if (sounding.get(0).getWindV() > SoundingLayer.NODATA
                        && abv.getWindV() < SoundingLayer.NODATA) {
                    sounding.get(0).setWindV(abv.getWindV());
                }
                params = new SoundingParams(sounding);
                sounding = params.getAnalysisData();
                sounding.checkSfcLayer();
                soundings.add(sounding);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating grib soundings", e);
        }
        System.out.println("Time to createSoundings = "
                + (System.currentTimeMillis() - t0));
        return soundings.toArray(new VerticalSounding[soundings.size()]);
    }

    /**
     * Add record to the skewt sounding map
     * 
     * @param record
     * @param dataVal
     * @throws VizException
     */
    private void addToSoundingMap(GridRecord record, float dataVal)
            throws VizException {
        DataTime dt = record.getDataTime();
        Map<Double, SoundingLayer> layerMap = soundingMap.get(dt);
        if (layerMap == null) {
            layerMap = new HashMap<Double, SoundingLayer>();
            soundingMap.put(dt, layerMap);
        }

        Double l1val = record.getLevel().getLevelonevalue();
        SoundingLayer layer = layerMap.get(l1val);
        if (layer == null) {
            layer = new SoundingLayer();
            layer.setPressure(l1val.floatValue());
            layerMap.put(l1val, layer);
        }
        String param = record.getParameter().getAbbreviation();

        if ("T".equals(param)) {
            layer.setTemperature(dataVal);
        } else if ("GH".equals(param)) {
            layer.setGeoHeight(dataVal);
        } else if ("uW".equals(param)) {
            layer.setWindU(dataVal);
        } else if ("vW".equals(param)) {
            layer.setWindV(dataVal);
        } else if ("DpT".equals(param)) {
            layer.setDewpoint(dataVal);
        }
    }

}
