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
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The interrogator for precip radar products
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarPrecipInterrogator extends RadarRadialInterrogator {
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.RadarRadialInterrogator#addParameters
     * (com.raytheon.uf.common.dataplugin.radar.RadarRecord,
     * com.vividsolutions.jts.geom.Coordinate, java.util.Map)
     */
    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            Map<String, String> dataMap) {
        int dataValue = 0;
        // Unit Converters
        UnitConverter feetToMeters = NonSI.FOOT.getConverterTo(SI.METRE);
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

        return dataValue;
    }
}
