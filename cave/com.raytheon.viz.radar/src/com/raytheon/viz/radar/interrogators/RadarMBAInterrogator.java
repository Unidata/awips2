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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.MBAPacket.MBAAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.MBAPacket.MBACategory;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Interrogator class for Radar GFM sampling.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/06/2014   DCS 16776  zwang       Initial creation
 * 
 * </pre>
 * 
 * @author zwang
 * @version 1.0
 */

public class RadarMBAInterrogator extends RadarGraphicInterrogator implements
        IRadarInterrogator {

    public RadarMBAInterrogator() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.IRadarInterrogator#sample(com.raytheon
     * .edex.plugin.radar.RadarRecord, com.vividsolutions.jts.geom.Coordinate,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public Map<String, String> sample(RadarRecord record, Coordinate latLon,
            ColorMapParameters params) {
        Map<String, String> dataMap = new HashMap<String, String>();
        if (latLon == null) {
            return null;
        }
        double[] input = { latLon.x, latLon.y }; // rr
        double[] output = new double[2]; // rr
        try {
            MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(
                    record.getCRS());

            mt.transform(input, 0, output, 0, 1);
            dataMap.put("crsLocation", output == null ? "-1,-1" : output[0]
                    + "," + output[1]);
        } catch (Exception e) {
            return null;
        }

        dataMap.put("ICAO", record.getIcao());
        dataMap.put("Mnemonic", record.getMnemonic());
        addParameters(record, latLon, dataMap);
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
        dataMap.put("Value", getDataValues(radarRecord, latLon));
        return 0;
    }

    private String getDataValues(RadarRecord radarRecord, Coordinate latLon) {
        StringBuffer rval = new StringBuffer();

        Coordinate c1 = new Coordinate(latLon.x + .025, latLon.y + .025);
        Coordinate c2 = new Coordinate(latLon.x - .025, latLon.y - .025);
        Envelope env = new Envelope(c1, c2);

        UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
                .getConverterTo(NonSI.KNOT);
        
        // Determine if the feature should be sampled
        RadarDisplayControls currentSettings = RadarDisplayManager
                .getInstance().getCurrentSettings();
        
        if (radarRecord.getProductCode() == 196) {
        	
            for (RadarDataKey key : radarRecord.getSymbologyData().keySet()) {
                
                Coordinate currStorm = new Coordinate(key.getLon(),
                        key.getLat());
                
                if (env.contains(currStorm)) {
                    // Get the data for the select feature
                    RadarDataPoint currPoint = radarRecord.getSymbologyData()
                            .get(key);
                    AreaComponent currFeature;
                    HashMap<Integer, HashMap<Integer, GenericDataComponent>> currPointData = currPoint
                            .getDisplayGenericPointData();

                    for (Integer type : currPointData.keySet()) {
                        for (GenericDataComponent currComp : currPointData.get(
                                type).values()) {
                            currFeature = (AreaComponent) currComp;

                            // Category: CATEGORY
                            String category = currFeature
                                    .getValue(MBAAttributeIDs.CATEGORY
                                            .toString());
                            
                            // if MBA is filtered out by category, do not sample
                            int catValue = category.equals("") ? 0 : Integer
                                    .parseInt(category);
                            
                            // By default, do not show MBA Wind Shear
                            int minCat = 1;
                            if (currentSettings.isMbaShowWindShear())
                                minCat = 0;
                            
                            if (catValue >= minCat) {
                                
                                // Microburst strength: DELTAV
                                String strength = currFeature
                                        .getValue(MBAAttributeIDs.DELTAV.toString());
                                if ((strength != null) && (strength.length() > 0)) {
                                    double strengthValue = metersPerSecondToKnots
                                            .convert(new Double(strength));
                                    strength = String.format("%dkts", (int) strengthValue);
                                }

                                // Maximum wind speed: MAXWINDSPEED
                                String maxSpeed = currFeature
                                        .getValue(MBAAttributeIDs.MAXWINDSPEED
                                                .toString());
                                if ((maxSpeed != null) && (maxSpeed.length() > 0)) {
                                    double spdValue = metersPerSecondToKnots
                                            .convert(new Double(maxSpeed));
                                    maxSpeed = String.format("%dkts", (int) spdValue);
                                }
                                
                                // Maximum shear: MAXSHEAR
                                String maxShear = currFeature
                                        .getValue(MBAAttributeIDs.MAXSHEAR
                                                .toString());
                                if ((maxShear != null) && (maxShear.length() > 0)) {
                                    double shearValue = new Double(maxShear);
                                    maxShear = String.format("%.4f/s", shearValue);
                                }
                                
                                rval.append(MBACategory.getCatName(catValue));
                                rval.append(" " + maxShear);
                                rval.append(" maxV " + maxSpeed);
                                rval.append(" deltaV " + strength);

                            }
                        }
                    }
                }
            }
        }
        return rval.toString();
    }
    
}
