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

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import org.locationtech.jts.geom.Coordinate;

/**
 * Interrogator for radar cross section products
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

public class RadarXsectInterrogator extends RadarDefaultInterrogator
        implements IRadarInterrogator {

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {
        // Check to make sure we are in range
        if (latLon.x < 0 || latLon.x > 1 || latLon.y < 0 || latLon.y > 1) {
            return 0;
        }
        int x = (int) (radarRecord.getNumBins() * latLon.x);
        int y = (int) (radarRecord.getNumRadials() * latLon.y);
        return radarRecord.getRawDataValue(y, x);
    }
}
