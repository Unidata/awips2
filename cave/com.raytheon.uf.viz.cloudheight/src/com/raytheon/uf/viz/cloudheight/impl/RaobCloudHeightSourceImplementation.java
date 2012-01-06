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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.dataplugin.bufrua.UAObsAdapter;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.cloudheight.data.CloudHeightData;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * Raob cloud height sounding implementation, requests soundings for all
 * stations
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

public class RaobCloudHeightSourceImplementation extends
        AbstractCloudHeightSourceImpl {

    private Map<DataTime, STRtree> searchMap = new HashMap<DataTime, STRtree>();

    /**
     * 
     */
    public RaobCloudHeightSourceImplementation(SoundingSource source) {
        super(source);
        resourceData.setBinOffset(new BinOffset(3600, 3600));
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
        requestMap.put("pluginName", new RequestConstraint("bufrua"));
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
    protected VerticalSounding createSoundingInternal(Coordinate latLon,
            DataTime time, PluginDataObject[] pdos) {
        CloudHeightData chd = CloudHeightData.getCloudHeightData();
        double maxMouseDistanceDeg = chd.getMaxMouseDistanceDeg();

        STRtree searchTree = searchMap.get(time);
        if (searchTree == null) {
            long t0 = System.currentTimeMillis();
            searchTree = new STRtree();
            searchMap.put(time, searchTree);

            Map<String, List<PluginDataObject>> pdoListMap = new HashMap<String, List<PluginDataObject>>();
            for (PluginDataObject pdo : pdos) {
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
        Coordinate p1 = new Coordinate(latLon.x + maxMouseDistanceDeg, latLon.y
                + maxMouseDistanceDeg);
        Coordinate p2 = new Coordinate(latLon.x - maxMouseDistanceDeg, latLon.y
                - maxMouseDistanceDeg);
        Envelope env = new Envelope(p1, p2);

        VerticalSounding closest = null;
        double closestDist = Double.MAX_VALUE;
        List<?> results = searchTree.query(env);
        for (Object obj : results) {
            VerticalSounding sounding = (VerticalSounding) obj;
            Coordinate c = new Coordinate(sounding.getLongitude(),
                    sounding.getLatitude());
            double distance = c.distance(latLon);
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
     * @see com.raytheon.uf.viz.cloudheight.impl.AbstractCloudHeightSourceImpl#
     * getValidTimeInterval()
     */
    @Override
    protected long getValidTimeInterval() {
        // 18 hours in millis, from A1 SatPVImageDepict.C
        return 18 * 3600 * 1000;
    }
}