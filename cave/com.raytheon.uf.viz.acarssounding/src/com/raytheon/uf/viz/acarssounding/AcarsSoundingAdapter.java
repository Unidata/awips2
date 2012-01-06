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
package com.raytheon.uf.viz.acarssounding;

import java.util.ArrayList;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.AbstractVerticalSoundingAdapter;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingLayer;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2008        1747 jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AcarsSoundingAdapter extends AbstractVerticalSoundingAdapter {

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
            ACARSSoundingRecord obs = (ACARSSoundingRecord) obj;
            VerticalSounding sounding = createVerticalSounding(obs);
            if (sounding != null) {
                soundings.add(sounding);
            }
        }
        return soundings.toArray(new VerticalSounding[soundings.size()]);
    }

    private VerticalSounding createVerticalSounding(ACARSSoundingRecord record) {
        VerticalSounding sounding = null;

        if (record != null) {
            sounding = new VerticalSounding();
            SurfaceObsLocation location = record.getLocation();
            sounding.setSpatialInfo(location);

            sounding.setElevation(location.getElevation());
            sounding.setStationId(location.getStationId());
            sounding.setObsTime(record.getDataTime().getRefTimeAsCalendar());
            sounding.setDataTime(record.getDataTime());

            sounding.setName(String.format("ACARS %.1f%c %.1f%c %s", Math
                    .abs(location.getLatitude()),
                    (location.getLatitude() >= 0 ? 'N' : 'S'), Math
                            .abs(location.getLongitude()), (location
                            .getLongitude() >= 0 ? 'E' : 'W'), record
                            .getLocation().getStationId()));
            for (ACARSSoundingLayer layer : record.getLevels()) {
                SoundingLayer sndLayer = new SoundingLayer();
                if (layer.getDwpt() != null) {
                    sndLayer.setDewpoint(layer.getDwpt().floatValue());
                }
                if (layer.getTemp() != null) {
                    sndLayer.setTemperature(layer.getTemp().floatValue());
                }
                if (layer.getWindDirection() != null) {

                    sndLayer.setWindDirection(layer.getWindDirection()
                            .floatValue());
                }
                if (layer.getWindSpeed() != null) {
                    sndLayer.setWindSpeed(layer.getWindSpeed().floatValue());
                }
                if (layer.getPressure() != null) {
                    sndLayer
                            .setPressure(layer.getPressure().floatValue() / 100);
                } else if (layer.getFlightLevel() != null) {
                    sndLayer.setPressure(Controller.ztopsa(layer
                            .getFlightLevel()));
                }
                if (layer.getFlightLevel() != null) {
                    sndLayer.setGeoHeight(layer.getFlightLevel().floatValue());
                }
                sounding.addLayer(sndLayer);
            }

        }
        return sounding;
    }

}