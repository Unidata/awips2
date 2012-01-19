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

import com.raytheon.edex.plugin.modelsounding.common.SoundingSite;
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

public class BufrSoundingAdapter extends AbstractVerticalSoundingAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(BufrSoundingAdapter.class);

    private static final String[] requiredParameters = new String[] { "P", "T",
            "DpT", "wSp", "WD" };

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
            SoundingSite obs = (SoundingSite) obj;
            VerticalSounding sounding = createVerticalSounding(obs);
            if (sounding != null) {
                soundings.add(sounding);
            }
        }
        return soundings.toArray(new VerticalSounding[soundings.size()]);
    }

    private VerticalSounding createVerticalSounding(SoundingSite sndngSite) {
        VerticalSounding sounding = null;

        if (sndngSite != null) {

            Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
            rcMap.put("dataURI", new RequestConstraint(""
                    + sndngSite.getDataURI()));
            PointDataContainer container;
            try {
                container = DataCubeContainer.getPointData(sndngSite
                        .getPluginName(), requiredParameters, rcMap);
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
            Number[] temp = pdv.getNumberAllLevels("T");
            Number[] dpt = pdv.getNumberAllLevels("DpT");
            Number[] windSpd = pdv.getNumberAllLevels("wSp");
            Number[] windDir = pdv.getNumberAllLevels("WD");

            if (p != null && p.length != 0) {
                sounding = new VerticalSounding();
                SurfaceObsLocation location = sndngSite.getLocation();
                sounding.setSpatialInfo(location);

                sounding.setElevation(sndngSite.getElevation());
                sounding.setStationId(sndngSite.getStationId());
                sounding.setObsTime(sndngSite.getDataTime()
                        .getRefTimeAsCalendar());
                sounding.setDataTime(sndngSite.getDataTime());

                String type = sndngSite.getReportType();

                type = ("ETA".equals(type)) ? "NAM" : "GFS";

                sounding.setDisplayFormat(String.format("%sBUFR{$pointId$}%s ",
                        type, sndngSite.getStationId()));
                
                sounding.setName(sndngSite.getStationId());
                
                for (int i = 0; i < p.length; i++) {
                    Number level = p[i];
                    if (level != null && level.intValue() != -9999) {
                        SoundingLayer layer = new SoundingLayer();

                        layer.setPressure((level.floatValue() != -9999) ? level
                                .floatValue() : SoundingLayer.MISSING);
                        float t = temp[i].floatValue();
                        layer.setTemperature((t != -9999) ? t
                                : SoundingLayer.MISSING);

                        Float d = dpt[i].floatValue();
                        layer.setDewpoint((d != -9999) ? d
                                : SoundingLayer.MISSING);

                        Float s = windSpd[i].floatValue();
                        layer.setWindSpeed((s != -9999) ? s
                                : SoundingLayer.MISSING);

                        Float w = windDir[i].floatValue();
                        layer.setWindDirection((w != -9999) ? w
                                : SoundingLayer.MISSING);

                        sounding.addLayer(layer);
                    }
                } // for
            }
        }
        return sounding;
    }
}
