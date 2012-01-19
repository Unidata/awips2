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

import java.util.HashMap;
import java.util.Map;

import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.viz.radar.util.GraphicDataUtil;
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

public class RadarGraphicInterrogator extends RadarDefaultInterrogator
        implements IRadarInterrogator {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.RadarDefaultInterrogator#sample(
     * com.raytheon.uf.common.dataplugin.radar.RadarRecord,
     * com.vividsolutions.jts.geom.Coordinate,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public Map<String, String> sample(RadarRecord radarRecord,
            Coordinate latLon, ColorMapParameters params) {

        Map<String, String> dataMap = new HashMap<String, String>();
        if (latLon == null) {
            return null;
        }
        double[] input = { latLon.x, latLon.y }; // rr
        double[] output = new double[2]; // rr
        try {
            MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(
                    radarRecord.getCRS());

            mt.transform(input, 0, output, 0, 1);
            dataMap.put("crsLocation", output == null ? "-1,-1" : output[0]
                    + "," + output[1]);
        } catch (Exception e) {
            return null;
        }

        dataMap.put("ICAO", radarRecord.getIcao());
        dataMap.put("Mnemonic", radarRecord.getMnemonic());

        addParameters(radarRecord, latLon, dataMap);
        return dataMap;
    }

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
        dataMap.put("Value",
                GraphicDataUtil.getGraphicDataValue(radarRecord, latLon));
        return 0;
    }
}
