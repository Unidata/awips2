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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.geometry.jts.JTS;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.dataplugin.bufrua.UAObsAdapter;
import com.raytheon.uf.common.dataplugin.bufrua.dao.BufrUAPointDataTransform;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.sounding.Activator;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * {@link IVerticalSoundingProvider} implementation using bufrua data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2013       2190 mschenke    Initial creation
 * Sep  9, 2013       2277 mschenke    Got rid of ScriptCreator references
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class BufruaSoundingProvider extends
        AbstractVerticalSoundingProvider<PluginDataObject[]> {

    private static final double MAX_MOUSE_DISTANCE_DEG = 5.0;

    private Map<DataTime, STRtree> searchMap = new HashMap<DataTime, STRtree>();

    @Override
    protected DataTime[] queryForSoundingTimes(
            Map<String, RequestConstraint> constraints) {
        TimeQueryRequest request = new TimeQueryRequest();
        request.setPluginName(UAObs.PLUGIN_NAME);
        request.setBinOffset(new BinOffset(3600, 3600));
        request.setQueryTerms(constraints);
        try {
            List<?> times = (List<?>) ThriftClient.sendRequest(request);
            return times.toArray(new DataTime[0]);
        } catch (VizException e) {
            throw new RuntimeException(
                    "Error querying for available sounding times", e);
        }
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
            constraints.put(PluginDataObject.DATATIME_ID,
                    new RequestConstraint(time.toString()));
            PointDataContainer pdc = DataCubeContainer.getPointData(
                    UAObs.PLUGIN_NAME, BufrUAPointDataTransform.MAN_PARAMS,
                    constraints);
            return BufrUAPointDataTransform.toUAObsRecords(pdc);
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
        STRtree searchTree = searchMap.get(time);
        if (searchTree == null) {
            long t0 = System.currentTimeMillis();
            searchTree = new STRtree();
            searchMap.put(time, searchTree);

            Map<String, List<PluginDataObject>> pdoListMap = new HashMap<String, List<PluginDataObject>>();
            for (PluginDataObject pdo : records) {
                UAObs obs = (UAObs) pdo;
                List<PluginDataObject> pdoList = pdoListMap.get(obs
                        .getStationId());
                if (pdoList == null) {
                    pdoList = new ArrayList<PluginDataObject>();
                    pdoListMap.put(obs.getStationId(), pdoList);
                }
                pdoList.add(pdo);
            }

            for (String key : pdoListMap.keySet()) {
                List<PluginDataObject> pdoList = pdoListMap.get(key);
                if (pdoList.size() > 0) {
                    UAObsAdapter adapter = new UAObsAdapter();
                    adapter.setObjects(pdoList
                            .toArray(new PluginDataObject[pdoList.size()]));
                    VerticalSounding[] soundings = adapter.createSoundings();
                    if (soundings.length > 0) {
                        VerticalSounding sounding = soundings[0];
                        // Clean up data:
                        List<SoundingLayer> badLayers = new ArrayList<SoundingLayer>();
                        for (SoundingLayer sl : sounding) {
                            if (isBad(sl.getPressure())
                                    || isBad(sl.getTemperature())
                                    || isBad(sl.getDewpoint())
                                    || isBad(sl.getGeoHeight())) {
                                badLayers.add(sl);
                            }
                        }

                        for (SoundingLayer sl : badLayers) {
                            sounding.removeLayer(sl);
                        }

                        sounding.setStationId(sounding.getName());
                        Coordinate soundingPoint = new Coordinate(
                                sounding.getLongitude(), sounding.getLatitude());
                        Coordinate p1 = new Coordinate(soundingPoint.x + 0.5,
                                soundingPoint.y + 0.5);
                        Coordinate p2 = new Coordinate(soundingPoint.x - 0.5,
                                soundingPoint.y - 0.5);
                        Envelope env = new Envelope(p1, p2);
                        searchTree.insert(env, sounding);
                    }
                }
            }
            System.out.println("time to populate RAOB Tree: "
                    + (System.currentTimeMillis() - t0) + "ms");
        }

        // Find the station to query
        // create envelope
        Coordinate p1 = new Coordinate(location.x + MAX_MOUSE_DISTANCE_DEG,
                location.y + MAX_MOUSE_DISTANCE_DEG);
        Coordinate p2 = new Coordinate(location.x - MAX_MOUSE_DISTANCE_DEG,
                location.y - MAX_MOUSE_DISTANCE_DEG);
        Envelope env = new Envelope(p1, p2);

        // Find the closest station and retrieve the sounding
        VerticalSounding closest = null;
        double closestDist = Double.MAX_VALUE;
        List<?> results = searchTree.query(env);
        for (Object obj : results) {
            VerticalSounding sounding = (VerticalSounding) obj;
            Coordinate c = sounding.getSpatialInfo().getLocation()
                    .getCoordinate();
            double distance = Double.MAX_VALUE;
            try {
                distance = JTS.orthodromicDistance(c, location,
                        MapUtil.LATLON_PROJECTION);
            } catch (TransformException e) {
                Activator.statusHandler.handle(Priority.INFO,
                        "Error computing distance", e);
                distance = c.distance(location);
            }
            if (distance < closestDist) {
                closestDist = distance;
                closest = sounding;
            }
        }
        return closest;
    }

    /**
     * @param sl
     * @return
     */
    private boolean isBad(float val) {
        return (val == SoundingLayer.MISSING || val == SoundingLayer.NODATA);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider#
     * getSoundingSource()
     */
    @Override
    public String getSoundingSource() {
        return "RAOB";
    }
}
