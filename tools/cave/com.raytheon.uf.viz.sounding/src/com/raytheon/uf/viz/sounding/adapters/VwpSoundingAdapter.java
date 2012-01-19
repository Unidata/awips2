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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.AbstractVerticalSoundingAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.sounding.Activator;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Descriptio
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VwpSoundingAdapter extends AbstractVerticalSoundingAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(VwpSoundingAdapter.class);

    private static final String[] requiredParameters = new String[] { "P",
            "uW", "vW", "GH", "latitude", "longitude", "stationId" };

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.sounding.AbstractVerticalSoundingAdapter#createSoundings
     * ()
     */
    @Override
    public VerticalSounding[] createSoundings() {
        ArrayList<VerticalSounding> soundings = new ArrayList<VerticalSounding>();
        for (PluginDataObject obj : objects) {
            VerticalSounding sounding = createVerticalSounding(obj);
            if (sounding != null && sounding.size() >= 5) {
                soundings.add(sounding);
            }
        }
        return soundings.toArray(new VerticalSounding[soundings.size()]);
    }

    private VerticalSounding createVerticalSounding(PluginDataObject pdo) {
        VerticalSounding sounding = null;

        if (pdo != null) {
            Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
            rcMap.put("dataURI", new RequestConstraint("" + pdo.getDataURI()));
            PointDataContainer container;
            try {
                container = DataCubeContainer.getPointData(pdo.getPluginName(),
                        requiredParameters, rcMap);
            } catch (VizException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving sounding", e1);
                return sounding;
            }

            if (container == null || container.getAllocatedSz() == 0) {
                return sounding;
            }
            PointDataView pdv = container.readRandom(0);

            Number[] p = pdv.getNumberAllLevels("P");
            Number[] uWind = pdv.getNumberAllLevels("uW");
            Number[] vWind = pdv.getNumberAllLevels("vW");
            Number[] gh = pdv.getNumberAllLevels("GH");
            Number lat = pdv.getNumber("latitude");
            Number lon = pdv.getNumber("longitude");
            String stationId = pdv.getString("stationId");
            if (p != null && p.length != 0) {
                sounding = new VerticalSounding();
                sounding.setName("VWP " + stationId.toUpperCase());
                sounding.setObsTime(pdo.getDataTime().getRefTimeAsCalendar());
                sounding.setDataTime(pdo.getDataTime());
                SurfaceObsLocation obs = new SurfaceObsLocation(stationId);
                obs.setLatitude(lat.doubleValue());
                obs.setLongitude(lon.doubleValue());
                sounding.setSpatialInfo(obs);
                for (int i = 0; i < p.length; i++) {
                    Number level = p[i];
                    if (level != null && level.intValue() != -9999) {
                        SoundingLayer layer = new SoundingLayer();
                        Float g = gh[i].floatValue();
                        layer.setGeoHeight((g != -9999) ? g * 30.48f
                                : SoundingLayer.MISSING);
                        layer.setPressure((level.floatValue() != -9999) ? level
                                .floatValue() : SoundingLayer.MISSING);
                        Float s = uWind[i].floatValue();
                        layer
                                .setWindU((s != -9999) ? s
                                        : SoundingLayer.MISSING);

                        Float w = vWind[i].floatValue();
                        layer
                                .setWindV((w != -9999) ? w
                                        : SoundingLayer.MISSING);
                        // layer.setDewpoint(0);
                        // layer.setTemperature(0);
                        sounding.addLayer(layer);
                    }
                }
            }
        }
        return sounding;
    }
}
