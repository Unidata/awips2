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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarEETInterrogator extends RadarRadialInterrogator implements
        IRadarInterrogator {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.RadarDefaultInterrogator#decodeValues
     * (int, java.util.Map, com.raytheon.uf.common.dataplugin.radar.RadarRecord,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public String decodeValues(int dataValue, String baseName, Map<String, String> dataMap,
            RadarRecord radarRecord, ColorMapParameters params) {
        dataValue -= 2;
        String d = "";
        if (dataValue >= 70 && dataValue < 130) {
            d = "NO DATA";
        } else if (dataValue >= 130) {
            dataValue -= 130;
            d = "~" + String.valueOf(dataValue);
        } else {
            d = String.valueOf(dataValue);
        }
        dataMap.put(baseName + "numericValue", d);
        return String.format("%s %s", d,
                UnitFormat.getUCUMInstance().format(params.getDisplayUnit()));
    }
}
