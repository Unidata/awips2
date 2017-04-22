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
package com.raytheon.uf.viz.monitor.data;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;
import com.raytheon.uf.common.monitor.data.ObConst.VarName;

/**
 * This class holds a collection of SAFESEAS' monitor thresholds. Two methods
 * are provided to retrieve the specified monitor threshold, one for
 * single-valued thresholds and another for dual-valued thresholds.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2009 2047       grichard    Initial creation.
 * Dec 24, 2009 3424       zhao        added getDualValuedThresholdMap and getSingleValuedThresholdMap
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class MonitorThresholdsSet {

    // The monitor threshold map of maps for single-valued monitor thresholds
    private Map<ObConst.VarName, Map<ObConst.ThreatLevel, Float>> monThreshMap = new HashMap<ObConst.VarName, Map<ObConst.ThreatLevel, Float>>();

    // The monitor threshold map of maps for dual-valued monitor thresholds
    private Map<ObConst.VarName, Map<ObConst.ThreatLevel, Float[]>> monDualThreshMap = new HashMap<ObConst.VarName, Map<ObConst.ThreatLevel, Float[]>>();

    // The monitor threshold for single-valued monitor thresholds
    private Map<ObConst.ThreatLevel, Float> monThresh;

    // The monitor threshold for dual-valued monitor thresholds
    private Map<ObConst.ThreatLevel, Float[]> monDualThresh;

    // Constructor -- takes "red" and "yellow" threshold data classes as
    // arguments
    public MonitorThresholdsSet(final SafeSeasThresholdStruct redThresh,
            final SafeSeasThresholdStruct yellowThresh) {

        // Wind Speed
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.windSpeed));
        monThresh
                .put(ThreatLevel.YELLOW, Float.valueOf(yellowThresh.windSpeed));
        monThreshMap.put(VarName.WIND_SPEED, monThresh);

        // Max Wind Speed
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.maxWindSpeed));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.maxWindSpeed));
        monThreshMap.put(VarName.MAX_WIND_SPEED, monThresh);

        // Gust Speed
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.gustSpeed));
        monThresh
                .put(ThreatLevel.YELLOW, Float.valueOf(yellowThresh.gustSpeed));
        monThreshMap.put(VarName.GUST_SPEED, monThresh);

        // Wave Height
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.waveHeight));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.waveHeight));
        monThreshMap.put(VarName.WAVE_HEIGHT, monThresh);

        // Visibility in statute miles
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.visibility));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.visibility));
        monThreshMap.put(VarName.VISIBILITY, monThresh);

        // Primary swell height
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.primSwellHt));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.primSwellHt));
        monThreshMap.put(VarName.PRIM_SWELL_HT, monThresh);

        // Primary swell period
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.primSwellPd));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.primSwellPd));
        monThreshMap.put(VarName.PRIM_SWELL_PD, monThresh);

        // Secondary swell height
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.secSwellHt));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.secSwellHt));
        monThreshMap.put(VarName.SEC_SWELL_HT, monThresh);

        // Secondary swell period
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.secSwellPd));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.secSwellPd));
        monThreshMap.put(VarName.SEC_SWELL_PD, monThresh);

        // Temperature
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.temperature));
        monThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.temperature));
        monThreshMap.put(VarName.TEMPERATURE, monThresh);

        // Snow Depth
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.snowDepth));
        monThresh
                .put(ThreatLevel.YELLOW, Float.valueOf(yellowThresh.snowDepth));
        monThreshMap.put(VarName.SNOW_DEPTH, monThresh);

        // Wind Chill
        monThresh = new HashMap<ObConst.ThreatLevel, Float>();
        monThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.windchill));
        monThresh
                .put(ThreatLevel.YELLOW, Float.valueOf(yellowThresh.windchill));
        monThreshMap.put(VarName.WIND_CHILL, monThresh);

        // Primary swell direction
        monDualThresh = new HashMap<ObConst.ThreatLevel, Float[]>();
        Float[] prf = { Float.valueOf(redThresh.primSwellDir1),
                Float.valueOf(redThresh.primSwellDir2) };
        monDualThresh.put(ThreatLevel.RED, prf);
        Float[] pyf = { Float.valueOf(yellowThresh.primSwellDir1),
                Float.valueOf(yellowThresh.primSwellDir2) };
        monDualThresh.put(ThreatLevel.YELLOW, pyf);
        monDualThreshMap.put(VarName.PRIM_SWELL_DIR, monDualThresh);

        // Secondary swell direction
        monDualThresh = new HashMap<ObConst.ThreatLevel, Float[]>();
        Float[] srf = { Float.valueOf(redThresh.secSwellDir1),
                Float.valueOf(redThresh.secSwellDir2) };
        monDualThresh.put(ThreatLevel.RED, srf);
        Float[] syf = { Float.valueOf(yellowThresh.secSwellDir1),
                Float.valueOf(yellowThresh.secSwellDir2) };
        monDualThresh.put(ThreatLevel.YELLOW, syf);
        monDualThreshMap.put(VarName.SEC_SWELL_DIR, monDualThresh);

    }

    /**
     * Method that obtains the monitor threshold
     * 
     * @param varName
     *            -- the variable name
     * @param threatLevel
     *            -- the threat level
     * @return the monitor threshold value
     */
    public float getThreshold(ObConst.VarName varName,
            ObConst.ThreatLevel threatLevel) {
        Map<ObConst.ThreatLevel, Float> monThresh;
        monThresh = monThreshMap.get(varName);
        return monThresh.get(threatLevel);
    }

    /**
     * Method that obtains the monitor threshold
     * 
     * @param varName
     *            -- the variable name
     * @param threatLevel
     *            -- the threat level
     * @return the monitor threshold values
     */
    public Float[] getThresholds(ObConst.VarName varName,
            ObConst.ThreatLevel threatLevel) {
        Map<ObConst.ThreatLevel, Float[]> monDualThresh;
        monDualThresh = monDualThreshMap.get(varName);
        return monDualThresh.get(threatLevel);
    }
    
    /**
     * [Dec 24, 2009, zhao]
     * @param varName enumerated-type variable name
     * @return single-valued threshold map, or null if the map 
     * contains no mapping for the key
     */
    public Map<ObConst.ThreatLevel,Float> getSingleValuedThresholdMap(ObConst.VarName varName) {
    	return monThreshMap.get(varName);
    }
    
    /**
     * [Dec 24, 2009, zhao]
     * @param varName enumerated-type variable name
     * @return duel-valued threshold map, or null if the map 
     * contains no mapping for the key 
     */
    public Map<ObConst.ThreatLevel,Float[]> getDualValuedThresholdMap(ObConst.VarName varName) {
    	return monDualThreshMap.get(varName);
    }
    
}
