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
import com.raytheon.uf.common.dataplugin.profiler.ProfilerObs;
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
 * Profiler sounding adapter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2009 3953       jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */
public class ProfilerSoundingAdapter extends AbstractVerticalSoundingAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ProfilerSoundingAdapter.class);

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
            ProfilerObs obs = (ProfilerObs) obj;
            VerticalSounding sounding = createVerticalSounding(obs);
            if (sounding != null) {
                soundings.add(sounding);
            }
        }
        return soundings.toArray(new VerticalSounding[soundings.size()]);
    }

    private VerticalSounding createVerticalSounding(ProfilerObs record) {
        VerticalSounding sounding = null;

        String[] requiredParameters = new String[] { "P", "height",
                "uComponent", "vComponent" };

        if (record != null) {

            Map<String, Number[]> numbers = getParameterNumberMap(
                    requiredParameters, record);
            if (numbers == null) {
                return sounding;
            }

            if (numbers.get("P") != null) {
                sounding = new VerticalSounding();
                SurfaceObsLocation location = record.getLocation();
                sounding.setSpatialInfo(location);

                sounding.setElevation(record.getElevation());
                sounding.setStationId(record.getStationId());
                sounding
                        .setObsTime(record.getDataTime().getRefTimeAsCalendar());
                sounding.setDataTime(record.getDataTime());

                sounding.setName(String.format("Profiler %.1f%c %.1f%c %s",
                        Math.abs(location.getLatitude()), (location
                                .getLatitude() >= 0 ? 'N' : 'S'), Math
                                .abs(location.getLongitude()), (location
                                .getLongitude() >= 0 ? 'E' : 'W'), record
                                .getStationId()));

                for (int i = 0; i < numbers.get("P").length; i++) {
                    Number level = numbers.get("P")[i];
                    if (level != null && level.intValue() != -9999) {
                        SoundingLayer layer = new SoundingLayer();

                        layer.setGeoHeight(getSoundingLayerValue(numbers
                                .get("height")[i]));
                        layer.setWindU(getSoundingLayerValue(numbers
                                .get("uComponent")[i]));
                        layer.setWindV(getSoundingLayerValue(numbers
                                .get("vComponent")[i]));
                        layer.setPressure(getSoundingLayerValue(numbers
                                .get("P")[i]));

                        sounding.addLayer(layer);
                    }
                } // for
            }
        }
        return sounding;
    }

    /**
     * @param h
     * @return
     */
    private float getSoundingLayerValue(Number number) {
        float value = number.floatValue();
        return (value != -9999) ? value : SoundingLayer.MISSING;
    }

    /**
     * @param requiredParameters2
     * @return
     */
    private Map<String, Number[]> getParameterNumberMap(
            String[] requiredParameters, ProfilerObs record) {

        Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
        rcMap.put("dataURI", new RequestConstraint("" + record.getDataURI()));
        PointDataContainer container;
        try {
            container = DataCubeContainer.getPointData(record.getPluginName(),
                    requiredParameters, rcMap);
        } catch (VizException e1) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving sounding", e1);
            return null;
        }

        if (container == null || container.getAllocatedSz() == 0) {
            return null;
        }
        PointDataView pdv = container.readRandom(0);

        Map<String, Number[]> parameterNumberMap = new HashMap<String, Number[]>();

        for (String parameter : requiredParameters) {
            parameterNumberMap
                    .put(parameter, pdv.getNumberAllLevels(parameter));
        }

        return parameterNumberMap;
    }
}
