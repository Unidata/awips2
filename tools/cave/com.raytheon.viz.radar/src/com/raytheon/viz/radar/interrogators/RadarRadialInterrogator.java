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
package com.raytheon.viz.radar.interrogators;

import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataInterrogator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.topo.ITopoQuery;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.core.topo.TopoQuery;
import com.raytheon.viz.radar.Activator;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarRadialInterrogator extends RadarDefaultInterrogator implements
        IRadarInterrogator {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RadarRadialInterrogator.class);

    protected RadarDataInterrogator interrogator = new RadarDataInterrogator(
            null);

    private boolean useTopo = true;

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            Map<String, String> dataMap) {
        int dataValue = 0;
        // Unit Converters
        UnitConverter feetToMeters = NonSI.FOOT.getConverterTo(SI.METRE);
        UnitConverter metersToFeet = SI.METRE.getConverterTo(NonSI.FOOT);
        UnitConverter metersToNm = SI.METRE.getConverterTo(NonSI.NAUTICAL_MILE);

        // Get value for Radial
        dataValue = interrogator.getDataValue(radarRecord, latLon);

        // Find MSL and AGL
        double range = interrogator.getLastRangeInterrogated();
        double azimuth = interrogator.getLastAzimuthInterrogated();
        double radarElevation = feetToMeters
                .convert(radarRecord.getElevation());
        // Set the values in the map
        dataMap.put("Azimuth", String.format("%d", (int) (azimuth)));
        dataMap.put("Range",
                String.format("%dnm", Math.round(metersToNm.convert(range))));
        dataMap.put("Elevation", String.format("%.2f", radarElevation));
        dataMap.put("Latitude", radarRecord.getLatitude().toString());
        dataMap.put("Longitude", radarRecord.getLongitude().toString());

        double tiltAngle = Math.toRadians(radarRecord.getTrueElevationAngle());
        if (tiltAngle > 0.0) {
            double msl = radarElevation + tiltAngle * range + range * range
                    / 1.7e7;
            double topoHeight = Double.NaN;
            if (useTopo) {
                ITopoQuery topoQuery = TopoQuery.getInstance();
                if (topoQuery != null) {
                    topoHeight = topoQuery.getHeight(latLon);
                } else {
                    useTopo = false;
                    statusHandler.handle(Priority.PROBLEM,
                            "Topo Error: Radar AGL sampling has been disabled");
                }
            }
            dataMap.put("MSL",
                    String.format("%.0fft", metersToFeet.convert(msl)));
            dataMap.put("msl", "" + msl);

            if (!Double.isNaN(topoHeight)) {
                double agl = msl - topoHeight;
                dataMap.put("AGL",
                        String.format("%.0fft", metersToFeet.convert(agl)));
            } else {
                dataMap.put("AGL", "???ft");
            }
        }
        return dataValue;
    }
}
