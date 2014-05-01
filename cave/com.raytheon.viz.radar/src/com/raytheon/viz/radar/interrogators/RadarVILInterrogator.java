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

import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * Use DigitalVILUnit object for conversion, this class will be removed in the
 * future.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@Deprecated
public class RadarVILInterrogator extends RadarRasterInterrogator implements
        IRadarInterrogator {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.RadarDefaultInterrogator#decodeValues
     * (int, java.util.Map)
     */
    @Override
    public String decodeValues(int dataValue, String baseName, Map<String, String> dataMap,
            RadarRecord radarRecord, ColorMapParameters params) {
        double decoded[] = new double[] { 0, 0, 0, 0, 0 };
        // TODO still doesn't return correct value
        double dispValue = 0;
        for (int i = 0; i < decoded.length; i++) {
            if (i == 2) {
                decoded[i] = radarRecord.getThreshold(i);
                continue;
            }
            decoded[i] = getVILValue(radarRecord.getThreshold(i));
        }
        if (dataValue < decoded[2]) {
            dispValue = (dataValue - decoded[1]) / decoded[0];
        } else if (dataValue >= decoded[2]) {
            dispValue = Math.exp((dataValue - decoded[4]) / decoded[3]);
        }
        return String.format("%.4f %s", dispValue, UnitFormat.getUCUMInstance()
                .format(params.getDisplayUnit()));
    }

    private float getVILValue(short x) {
        int s = (x >> 15) & 0x1;
        int e = (x >> 10) & 0x1f;
        int f = x & 0x3ff;

        float value;
        if (e == 0) {
            value = (float) (Math.pow(-1, s) * 2 * (f / Math.pow(2, 10)));
        } else {
            value = (float) ((Math.pow(-1, s) * Math.pow(2, e - 16) * (1 + f
                    / Math.pow(2, 10))));
        }
        return value;
    }
}
