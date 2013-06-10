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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.poessounding.POESSounding;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.sounding.adapter.AbstractVerticalSoundingAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PointDataRequest;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 01, 2008 1747       jkorman     Initial creation
 * May 15, 2013 1869       bsteffen    Remove DataURI from goes/poes soundings.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class POESSoundingAdapter extends AbstractVerticalSoundingAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(POESSoundingAdapter.class);

    private static UnitConverter pascalsToHectoPascals = SI.PASCAL
            .getConverterTo(SI.HECTO(SI.PASCAL));

    private static final String[] requiredParameters = new String[] {
            "pressure", "temperature", "mixingRatio" };

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
            POESSounding obs = (POESSounding) obj;
            VerticalSounding sounding = createVerticalSounding(obs);
            if (sounding != null) {
                soundings.add(sounding);
            }
        }
        return soundings.toArray(new VerticalSounding[soundings.size()]);
    }

    private VerticalSounding createVerticalSounding(POESSounding obsData) {
        VerticalSounding sounding = null;

        if (obsData != null) {

            Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
            rcMap.put("location.stationId", new RequestConstraint(obsData
                    .getLocation().getStationId()));
            rcMap.put(
                    "dataTime.refTime",
                    new RequestConstraint(TimeUtil.formatToSqlTimestamp(obsData
                            .getDataTime().getRefTime())));
            PointDataContainer container;
            try {
                container = PointDataRequest.requestPointDataAllLevels(null,
                        obsData.getPluginName(), requiredParameters, null,
                        rcMap);
            } catch (VizException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving sounding", e1);
                return sounding;
            }

            if (container == null || container.getAllocatedSz() == 0) {
                return sounding;
            }
            PointDataView pdv = container.readRandom(0);

            Number[] p = pdv.getNumberAllLevels("pressure");
            Number[] temp = pdv.getNumberAllLevels("temperature");
            Number[] mr = pdv.getNumberAllLevels("mixingRatio");

            if (p != null) {
                sounding = new VerticalSounding();
                SurfaceObsLocation location = obsData.getLocation();
                sounding.setSpatialInfo(location);

                sounding.setElevation(obsData.getElevation());
                sounding.setStationId(obsData.getStationId());
                sounding.setObsTime(obsData.getDataTime()
                        .getRefTimeAsCalendar());
                sounding.setDataTime(obsData.getDataTime());

                sounding.setName(String.format("PoesBufr %.1f%c %.1f%c %s",
                        Math.abs(location.getLatitude()), (location
                                .getLatitude() >= 0 ? 'N' : 'S'), Math
                                .abs(location.getLongitude()), (location
                                .getLongitude() >= 0 ? 'E' : 'W'), obsData
                                .getStationId()));

                for (int i = 0; i < p.length; i++) {
                    Number level = p[i];
                    if (level != null && level.intValue() != -9999) {
                        SoundingLayer layer = new SoundingLayer();

                        Integer n = level.intValue();
                        layer
                                .setPressure((n != -9999) ? (float) pascalsToHectoPascals
                                        .convert(n.doubleValue())
                                        : SoundingLayer.MISSING);

                        float t = temp[i].floatValue();
                        layer.setTemperature((t != -9999) ? t
                                : SoundingLayer.MISSING);

                        // POES data doesn't contain dewpoint, but does have
                        // mixing ratio so
                        // we need to calculate the dewpoint from available
                        // data.
                        Double m = mr[i].doubleValue();
                        if ((n != -9999) && (t != -9999) && (m != -9999)) {

                            double dewPoint = WxMath.tempAtMixingRatio(
                                    pascalsToHectoPascals.convert(n
                                            .doubleValue()), m * 1000);

                            layer.setDewpoint((float) dewPoint);
                        } else {
                            layer.setDewpoint(SoundingLayer.MISSING);
                        }
                        sounding.addLayer(layer);
                    }
                } // for
            }
        }
        return sounding;
    }

}
