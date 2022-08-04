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

import java.util.Set;

import javax.measure.Quantity;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;

import tec.uom.se.format.SimpleUnitFormat;
import tec.uom.se.quantity.Quantities;

/**
 * Interrogator for Radar EET products.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 05, 2010            mnash       Initial creation
 * Sep 13, 2016 3239       nabowle     Use the Interrogatable API.
 *
 * </pre>
 *
 * @author mnash
 */

public class RadarEETInterrogator extends RadarRadialInterrogator
        implements IRadarInterrogator {

    @Override
    public void decodeValues(int dataValue,
            InterrogationKey<Quantity<?>> valueKey,
            InterrogationKey<String> stringKey, InterrogateMap dataMap,
            RadarRecord radarRecord, ColorMapParameters params,
            Set<InterrogationKey<?>> keys) {
        dataValue -= 2;
        String d = "";
        if (dataValue >= 70 && dataValue < 130) {
            d = "NO DATA";
        } else if (dataValue >= 130) {
            dataValue -= 130;
            d = "~" + String.valueOf(dataValue);
            addValueToMap(dataMap, keys, valueKey,
                    Quantities.getQuantity(dataValue, params.getDisplayUnit()));
        } else {
            d = String.valueOf(dataValue);
            addValueToMap(dataMap, keys, valueKey,
                    Quantities.getQuantity(dataValue, params.getDisplayUnit()));
        }

        addValueToMap(dataMap, keys, stringKey, String.format("%s %s", d,
                SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(params.getDisplayUnit())));
    }
}
