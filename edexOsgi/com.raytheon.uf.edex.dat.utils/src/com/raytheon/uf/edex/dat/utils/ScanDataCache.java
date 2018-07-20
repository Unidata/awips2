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
package com.raytheon.uf.edex.dat.utils;

import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.scan.data.LightningData;
import com.raytheon.uf.common.dataplugin.scan.data.ModelData;
import com.raytheon.uf.common.dataplugin.scan.data.RadarData;
import com.raytheon.uf.common.dataplugin.scan.data.SoundingData;

/**
 * SCAN Cache object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/02/2011        dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ScanDataCache {

    /** Singleton instance of this class */
    private static ScanDataCache instance = null;

    /** cache that holds grib records used by SCAN **/
    private ModelData md = new ModelData();

    /** cache that holds lightning records used by SCAN **/
    private LightningData ld = new LightningData();

    /** keeper of Sounding info used for scan **/
    private HashMap<String, SoundingData> sd = new HashMap<String, SoundingData>();

    /** keeper of Radar info used for scan **/
    private HashMap<String, RadarData> rd = new HashMap<String, RadarData>();

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static ScanDataCache getInstance() {
        if (instance == null) {
            instance = new ScanDataCache();
        }
        return instance;
    }

    public ModelData getModelData() {
        return md;
    }

    public synchronized RadarData getRadarData(String icao) {
        RadarData data = null;
        if (rd.containsKey(icao)) {
            // System.out.println("Accessing Radar cache:-----------" + icao);
            data = rd.get(icao);
        } else {
            // System.out.println("Creating Radar cache:-----------" + icao);
            data = new RadarData();
            rd.put(icao, data);
        }
        return data;
    }

    public void setModelData(ModelData md) {
        this.md = md;
    }

    public synchronized void setRadarData(String icao, RadarData data) {
        rd.put(icao, data);
    }

    public LightningData getLigtningData() {
        return ld;
    }

    public void setLigtningData(LightningData ld) {
        this.ld = ld;
    }

    public synchronized void setSoundingData(String icao, SoundingData data) {
        sd.put(icao, data);
    }

    public synchronized SoundingData getSoundingData(String icao) {
        SoundingData sounding = null;
        if (sd.containsKey(icao)) {
            // System.out.println("Accessing Sounding cache..." + icao);
            sounding = sd.get(icao);
        } else {
            // System.out.println("Creating Sounding cache..." + icao);
            sounding = new SoundingData();
            sd.put(icao, sounding);
        }
        return sounding;
    }

}
