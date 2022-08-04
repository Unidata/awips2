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

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.viz.radar.util.GraphicDataUtil;
import org.locationtech.jts.geom.Coordinate;

/**
 * Interrogator for Graphic radar products.
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

public class RadarGraphicInterrogator extends RadarDefaultInterrogator
        implements IRadarInterrogator {

    @Override
    public InterrogateMap sample(RadarRecord radarRecord, Coordinate latLon,
            ColorMapParameters params, Set<InterrogationKey<?>> keys) {
        InterrogateMap dataMap = new InterrogateMap();
        sample(radarRecord, latLon, keys, dataMap);
        return dataMap;
    }

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {
        if (keys.contains(RadarDefaultInterrogator.VALUE_STRING)) {
            dataMap.put(RadarDefaultInterrogator.VALUE_STRING,
                    GraphicDataUtil.getGraphicDataValue(radarRecord, latLon));
        }
        return 0;
    }
}
