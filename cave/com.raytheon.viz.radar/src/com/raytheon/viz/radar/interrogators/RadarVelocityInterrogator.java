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
import javax.measure.unit.SI;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataInterrogator;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2010            mnash     Initial creation
 * 05/02/2013   DR 14587   D. Friedman Store base velocity in map.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarVelocityInterrogator extends RadarRadialInterrogator
        implements IRadarInterrogator {
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.RadarDefaultInterrogator#computeValues
     * (java.util.Map, int)
     */
    @Override
    public void computeValues(RadarRecord radarRecord,
            Map<String, String> dataMap, int dataValue,
            ColorMapParameters params) {
        double deltaAngle = 0.0f;
        int dataValue2 = 0;
        if (radarRecord.getAngleData() == null) {
            return;
        }

        int lastRadialIndex = interrogator.getLastRadialIndexInterrogated();

        if (lastRadialIndex < 0) {
            return;
        }

        float radialCenterAngle = radarRecord.getRadialAzimuth(lastRadialIndex);

        int radial1 = (lastRadialIndex + 1) % radarRecord.getNumRadials();
        float angle1 = radarRecord.getRadialAzimuth(radial1);

        int radial2 = (lastRadialIndex > 0 ? (lastRadialIndex - 1)
                : radarRecord.getNumRadials() - 1);
        float angle2 = radarRecord.getRadialAzimuth(radial2);

        double lastAzimuth = interrogator.getLastAzimuthInterrogated();
        int lastBin = interrogator.getLastBinInterrogated();

        double dataAngle2 = 0;
        if (Math.abs(angle1 - lastAzimuth) < Math.abs(radialCenterAngle
                - lastAzimuth)) {
            dataAngle2 = angle1 - radialCenterAngle;
            dataValue2 = radarRecord.getRawDataValue(radial1, lastBin) & 0xFF;
        } else {
            dataAngle2 = angle2 - radialCenterAngle;
            if (radial1 * radarRecord.getNumBins() + lastBin > radarRecord
                    .getRawData().length) {
                dataValue2 = 0;
            } else {
                dataValue2 = radarRecord.getRawDataValue(radial2, lastBin) & 0xFF;
            }
        }

        double velocity1 = Double.NaN;
        double velocity2 = Double.NaN;
        UnitConverter cvt = UnitConverter.IDENTITY;
        if (params != null) {
            params.getDataUnit().getConverterTo(SI.METERS_PER_SECOND);
        }
        if (radarRecord.getNumLevels() <= 16) {
            Object th1 = radarRecord.getDecodedThreshold(dataValue);
            Object th2 = radarRecord.getDecodedThreshold(dataValue2);
            if ((th1 instanceof Float) && (th2 instanceof Float)) {
                velocity1 = cvt.convert(dataValue);
                velocity2 = cvt.convert(dataValue2);
            }

        } else {
            velocity1 = cvt.convert(dataValue);
            velocity2 = cvt.convert(dataValue2);
        }
        double deltaVelocity = velocity2 - velocity1;
        if (!Double.isNaN(deltaVelocity)) {
            deltaAngle = Math.toRadians(dataAngle2 * 2)
                    * interrogator.getLastRangeInterrogated();

            double shear = deltaVelocity / deltaAngle;

            dataMap.put("Shear", String.format("%.4f/s", shear));
        }
    }

    @Override
    public Map<String, String> sample(RadarRecord radarRecord,
            Coordinate latLon, ColorMapParameters params) {
        Map<String, String> map = super.sample(radarRecord, latLon, params);
        int dataValue = interrogator.getDataValue(latLon,
                RadarDataInterrogator.DataType.BASE_VELOCITY);
        addValueToMap(dataValue, "baseVelocity-", radarRecord, params, map);
        return map;
    }
}
