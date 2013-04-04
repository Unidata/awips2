package com.raytheon.uf.common.dataplugin.scan.data;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.Transient;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.time.DataTime;

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

/**
 * 
 * EnvironmentalData, Used to hold data derived from the VerticalSoundings and
 * models they don't change very often and are held here for convenience.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/11/2009   2307      dhladky    Initial Creation.
 * 02/01/13     1569      D. Hladky   removed XML where not needed
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
public class SoundingData implements ISerializableObject {

    @DynamicSerializeElement
    public DataTime time = null;

    /** sounding freeze level **/
    @DynamicSerializeElement
    public float frzLevel = 0.0f;

    /** sounding 100 - 500 mb thickness **/
    @DynamicSerializeElement
    public float thick1000500 = 0.0f;

    /** wind speed at 700 mb **/
    @DynamicSerializeElement
    public float spd700 = 0.0f;

    /** U wind at 500 mb **/
    @DynamicSerializeElement
    public float u500 = 0.0f;

    /** Sounding total totals **/
    @DynamicSerializeElement
    public float totalTotals = 0.0f;

    @Transient
    private final Map<String, VerticalSounding> soundingMap;

    /**
     * Public sounding constructor
     */
    public SoundingData() {
        soundingMap = new HashMap<String, VerticalSounding>();
    }

    /**
     * Check for type
     * 
     * @param type
     * @return
     */
    public boolean isType(String type) {
        boolean key = false;
        if (soundingMap.keySet().contains(type)) {
            key = true;
        }
        return key;
    }

    /**
     * Setter for Sounding Record
     * 
     * @param prodType
     *            -- product type
     * @param rr
     *            -- vertical sounding record
     */
    public void setSoundingRecord(String prodType, VerticalSounding vs) {
        soundingMap.put(prodType, vs);
    }

    /**
     * Getter for Sounding Record
     * 
     * @param prodType
     *            -- product type
     * @return a vertical sounding record
     */
    public VerticalSounding getSoundingRecord(String prodType) {
        return soundingMap.get(prodType);
    }

    public float getFrzLevel() {
        return frzLevel;
    }

    public void setFrzLevel(float frzLevel) {
        this.frzLevel = frzLevel;
    }

    public float getThick1000500() {
        return thick1000500;
    }

    public void setThick1000500(float thick1000500) {
        this.thick1000500 = thick1000500;
    }

    public float getSpd700() {
        return spd700;
    }

    public void setSpd700(float spd700) {
        this.spd700 = spd700;
    }

    public float getU500() {
        return u500;
    }

    public void setU500(float u500) {
        this.u500 = u500;
    }

    public float getTotalTotals() {
        return totalTotals;
    }

    public void setTotalTotals(float totalTotals) {
        this.totalTotals = totalTotals;
    }

    public void setTime(DataTime time) {
        this.time = time;
    }

    public DataTime getTime() {
        return time;
    }

    /**
     * process a sounding for out environmental values
     * 
     * @param sp
     */

    /*
     * private void processSoundingObject(VerticalSounding vs) { SoundingParams
     * sp = new SoundingParams(vs);
     * 
     * if (sp.frzgLvlZ() != SoundingLayer.MISSING) { setFrzLevel(sp.frzgLvlZ());
     * } if (sp.Totals() != SoundingLayer.MISSING) {
     * setTotalTotals(sp.Totals()); } SoundingLayer layer =
     * vs.getLayerNearest(700); if (layer != null) {
     * setSpd700(layer.getWindSpeed()); }
     * 
     * layer = vs.getLayerNearest(500.0f); if (layer != null) { SoundingLayer sl
     * = layer; setU500(sl.getWindU()); } SoundingLayer layer2 =
     * vs.getLayerNearest(1000); if (layer != null && layer2 != null) {
     * setThick1000500(layer.getGeoHeight() - layer2.getGeoHeight()); }
     * 
     * }
     */

    /**
     * Set environmental data
     */
    public void setEnvironmentalData(VerticalSounding vs) {
        // FIXME Get these values from MODEL DATA not from UA!
        setFrzLevel(vs.firstFreezingLevel());
        setThick1000500(5750); // units: meters
        setSpd700(vs.getWindSpeed700());
        setU500(vs.getWindUComp500());
        setTotalTotals(vs.totalTotals());

    }

    /**
     * Override
     */
    @Override
    public String toString() {
        StringBuffer buff = new StringBuffer();
        buff.append("---------Environmental Data----------");
        buff.append("freeze_level: " + frzLevel + "\n");
        buff.append("thick1000_500 : " + thick1000500 + "\n");
        buff.append("spd700: " + spd700 + "\n");
        buff.append("u500: " + u500 + "\n");
        buff.append("totalTotals: " + totalTotals + "\n");
        return buff.toString();
    }
}
