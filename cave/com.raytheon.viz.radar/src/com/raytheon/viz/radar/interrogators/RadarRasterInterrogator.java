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
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarRasterInterrogator extends RadarDefaultInterrogator implements
        IRadarInterrogator {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.IRadarInterrogator#addParameters
     * (com.raytheon.uf.common.dataplugin.radar.RadarRecord,
     * com.vividsolutions.jts.geom.Coordinate, java.util.Map)
     */
    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            Map<String, String> dataMap) {
        return new RadarDataInterrogator(radarRecord).getDataValue(latLon);
    }
}
