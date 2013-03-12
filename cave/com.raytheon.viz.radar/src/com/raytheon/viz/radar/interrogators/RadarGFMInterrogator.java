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

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket.GFMAttributeIDs;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
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
 * 03/04/2013   DCS51      zwang       Initial creation
 * 
 * </pre>
 * 
 * @author zwang
 * @version 1.0
 */

public class RadarGFMInterrogator extends RadarGraphicInterrogator implements
        IRadarInterrogator {

    public RadarGFMInterrogator() {
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

        NumberFormat formatter = new DecimalFormat("###");
        
        String windX, windY;
        double pU = 0.0;
        double pV = 0.0;
        double pSpd = 0.0;
        double pDir = 0.0;
        double wX = 0.0;
        double wY = 0.0;
        
        if (radarRecord.getProductCode() == 140) {
        	
            for (RadarDataKey key : radarRecord.getSymbologyData().keySet()) {
                
            	// Get the data for the select feature
                RadarDataPoint currPoint = radarRecord.getSymbologyData()
                        .get(key);
                
                AreaComponent currFeature;
                HashMap<Integer, HashMap<Integer, GenericDataComponent>> currPointData = currPoint
                        .getDisplayGenericPointData();
                        
                for (Integer type : currPointData.keySet()) {
                    for (GenericDataComponent currComp : currPointData
                         .get(type).values()) {
                        currFeature = (AreaComponent) currComp;
        
                        int numParam = currFeature.getParameters().size();
                        
                        if (numParam == 11) {
                        	windX = currFeature.getValue(GFMPacket.GFMAttributeIDs.WINDBEHINDX.getName());
                            if ((windX != null) && (windX.length() > 0)) {
                               wX = Float.parseFloat(windX);
                            }
                            windY = currFeature.getValue(GFMPacket.GFMAttributeIDs.WINDBEHINDY.getName());
                            if ((windY != null) && (windY.length() > 0)) {
                               wY = Float.parseFloat(windY);
                            }
                            
                            // windX/windY unit is 1km, convert to 1/4km
                            Coordinate currStorm = radarRecord
                                       .convertStormLatLon(wX * 4.0, wY * 4.0);
                            
                            if (env.contains(currStorm) && currPoint.isVisible()) {

                            	// propU
                                String pUStr = currFeature
                                       .getValue(GFMAttributeIDs.PROPU.toString());
                                if ((pUStr != null) && (pUStr.length() > 0)) {
                                   pU = metersPerSecondToKnots.convert(new Double(pUStr));
                                }
                                        
                                // propV
                                String pVStr = currFeature
                                       .getValue(GFMAttributeIDs.PROPV.toString());
                                if ((pVStr != null) && (pVStr.length() > 0)) {
                                   pV = metersPerSecondToKnots.convert(new Double(pVStr));
                                }
                                pSpd = getSpeed(pU, pV);
                                pDir = getDir(pU, pV);
                                        
                                // avg_speed
                                String spdStr = currFeature
                                       .getValue(GFMAttributeIDs.AVG_SPEED.toString());
                                if ((spdStr != null) && (spdStr.length() > 0)) {
                                   double spd = metersPerSecondToKnots
                                          .convert(new Double(spdStr));
                                   spdStr = formatter.format(spd);
                                }
                                        
                                // avg_direction
                                String dirStr = currFeature
                                       .getValue(GFMAttributeIDs.AVG_DIRECTION.toString());

                                if ((dirStr != null) && (dirStr.length() > 0)) {
                                   double dir = new Double(dirStr);
                                   dirStr = formatter.format(dir);
                                }
                                        
                                // wsHarzard
                                String wsStr = currFeature
                                       .getValue(GFMAttributeIDs.WSHAZARD.toString());
                                if ((wsStr != null) && (wsStr.length() > 0)) {
                                   double ws = metersPerSecondToKnots
                                          .convert(new Double(wsStr));
                                   wsStr = formatter.format(ws);
                                }
                                        
                                rval.append("Movement " + formatter.format(pSpd) + "kts@" 
                                		+ formatter.format(pDir) + "\n");
                                rval.append("Wind Behind " + spdStr + "kts@" + dirStr + "\n");
                                rval.append("Wind Shear Hazard " + wsStr + "kts ");
                            
                            }
                            break;
                        }
                    }    
                }
                
            }
        }

        return rval.toString();
    }

    // Get wind speed from (u,v)
    private double getSpeed(double u, double v) {
    	double spd = 0.0;
    	spd = Math.sqrt(u * u + v * v);
    	
    	return spd;
    }
    
    // Get wind direction from (u,v)
    private double getDir(double u, double v) {
    	double R_EQ_ZERO = 1.0E-10;
    	double DEG_HALF_PI = 180.0;
    	
    	double spd = 0.0;
    	double dir = 0.0;
    	spd = Math.sqrt(u * u + v * v);
    	
    	if (spd < R_EQ_ZERO) {
            dir = 0.0;
        } else {
            dir = (double)(Math.toDegrees(Math.atan2(u, v)) + DEG_HALF_PI + 1.0E-3);  
        }
    	   	
    	return dir;
    }
}
