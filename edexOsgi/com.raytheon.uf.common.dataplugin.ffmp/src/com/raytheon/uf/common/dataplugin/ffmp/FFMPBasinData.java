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
package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * FFMPBasinData Container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/22/09      2152       D. Hladky   Initial release
 * 01/27/13      1478       D. Hladky   Added support for write of aggregate record cache
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@DynamicSerialize
public class FFMPBasinData implements ISerializableObject {

    /**
	 * 
	 */
    private static final long serialVersionUID = 8162247989509750715L;

    public static final double GUIDANCE_MISSING = -999999.0;

    // defaults
    @DynamicSerializeElement
    private String hucLevel;

    @DynamicSerializeElement
    private HashMap<Long, FFMPBasin> basins = new HashMap<Long, FFMPBasin>();

    /**
     * Public one arg constructor
     * 
     * @param huc_level
     */
    public FFMPBasinData(String hucLevel) {
        setHucLevel(hucLevel);
    }

    /**
     * No arg hibernate constructor
     */
    public FFMPBasinData() {

    }

    /**
     * get the basin map
     * 
     * @return
     */
    public HashMap<Long, FFMPBasin> getBasins() {
        return basins;
    }

    /**
     * Sets the basin map
     * 
     * @param basins
     */
    public void setBasins(HashMap<Long, FFMPBasin> basins) {
        this.basins = basins;
    }

    /**
     * Gets the HUC Level for this map
     * 
     * @return
     */
    public String getHucLevel() {
        return hucLevel;
    }

    /**
     * Sets the HUC Level for this map
     */
    public void setHucLevel(String hucLevel) {
        this.hucLevel = hucLevel;
    }

    /**
     * Add to the hash
     * 
     * @param key
     * @param basin
     */
    public void put(Long key, FFMPBasin basin) {
        basins.put(key, basin);
    }

    /**
     * Gets the FFMPBasin from the hash
     * 
     * @param key
     * @return
     */
    public FFMPBasin get(Long key) {
        return basins.get(key);
    }

    /**
     * Gets a list of pfaf ids from the keys
     * 
     * @return
     */
    public ArrayList<Long> getPfafIds() {
        ArrayList<Long> pfafs = new ArrayList<Long>(basins.keySet());
        return pfafs;
    }

    /**
     * Extracts the average value for an aggregation of basins
     * 
     * @param pfaf_ids
     * @return
     */
    public float getAverageValue(ArrayList<Long> pfaf_ids, Date beforeDate,
            Date afterDate) {

        float tvalue = 0.0f;
        int i = 0;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                tvalue += basin.getValue(beforeDate, afterDate);
                i++;
            }
        }
        tvalue = tvalue / i;

        return tvalue;
    }

    /**
     * Extracts the average value for an aggregation of basins with areal
     * weighting
     * 
     * @param pfaf_ids
     * @return
     */
    public float getAverageValue(ArrayList<Long> pfaf_ids,
            ArrayList<Double> areas) {

        float tvalue = 0.0f;
        float tarea = 0.0f;
        int i = 0;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                if (basin.getValue() != FFMPUtils.MISSING) {
                    tvalue += (basin.getValue() * areas.get(i));
                    tarea += areas.get(i);
                }
            }
        }

        if (tvalue < 0.0f) {
            tvalue = FFMPUtils.MISSING;
        } else {
            tvalue = tvalue / tarea;
        }

        return tvalue;
    }

    /**
     * Extracts the average value for an aggregation of basins to a given time
     * 
     * @param pfaf_ids
     * @return
     */
    public float getAccumAverageValue(ArrayList<Long> pfaf_ids,
            Date beforeDate, Date afterDate, long expirationTime, boolean rate) {

        float tvalue = 0.0f;
        int i = 0;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                tvalue += basin.getAccumValue(beforeDate, afterDate,
                        expirationTime, rate);
                i++;
            }
        }
        tvalue = tvalue / i;

        return tvalue;
    }

    /**
     * Extracts the Max value for a range of times in an aggregation of basins
     * 
     * @param pfaf_ids
     * @param hour
     * @return
     */
    public float getMaxValue(ArrayList<Long> pfaf_ids, Date beforeDate,
            Date afterDate) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                Float value = basin.getValue(beforeDate, afterDate);
                if (value > tvalue) {
                    tvalue = value;
                }
            }
        }
        return tvalue;
    }
    
    /**
     * Used for mosaic sources
     * @param pfaf_ids
     * @param date
     * @param expiration
     * @return
     */
    public float getAverageMaxValue(ArrayList<Long> pfaf_ids, Date date,
            long expiration) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                Float value = basin.getAverageValue(date, expiration);
                if (value > tvalue) {
                    tvalue = value;
                }
            }
        }
        return tvalue;
    }
    
    /**
     * Used for mosaic sources
     * @param pfaf_ids
     * @param date
     * @param expiration
     * @return
     */
    public float getAverageMaxValue(ArrayList<Long> pfaf_ids, Date afterDate,
            Date beforeDate) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                Float value = basin.getAverageValue(afterDate, beforeDate);
                if (value > tvalue) {
                    tvalue = value;
                }
            }
        }
        return tvalue;
    }

    /**
     * Extracts the Max value for a specific time in an aggregation of basins
     * 
     * @param pfaf_ids
     * @param hour
     * @return
     */
    public float getMaxValue(ArrayList<Long> pfaf_ids, Date date) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                Float value = basin.getValue(date);
                if (value > tvalue) {
                    tvalue = value;
                }
            }
        }
        return tvalue;
    }

    /**
     * Average Guidance
     * 
     * @param pfaf_ids
     * @param sourceName
     * @return
     */
    public float getAverageGuidanceValue(ArrayList<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, float guidance,
            ArrayList<Long> forcedPfafs, long expiration) {

        float tvalue = 0.0f;
        float value;
        int i = 0;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);

            if (basin == null) {
                return guidance;
            }

            if (basin != null) {
                FFMPGuidanceBasin fgb = (FFMPGuidanceBasin) basin;
                if (forcedPfafs.contains(pfaf)) {
                    if (interpolation.isInterpolate()) {
                        value = fgb.getInterpolatedValue(
                                interpolation.getSource1(),
                                interpolation.getSource2(),
                                interpolation.getInterpolationOffset(),
                                interpolation, expiration);
                    } else {
                        value = fgb.getValue(interpolation.getStandardSource(),
                                interpolation, expiration);
                    }
                    // ignore missing values
                    if (value < -999) {
                        continue;
                    }
                    tvalue += value;
                    i++;
                } else {
                    if (interpolation.isInterpolate()) {
                        float nvalue;
                        if (new Float(guidance).isNaN()) {
                            nvalue = fgb.getInterpolatedValue(
                                    interpolation.getSource1(),
                                    interpolation.getSource2(),
                                    interpolation.getInterpolationOffset(),
                                    interpolation, expiration);
                        } else {
                            nvalue = guidance;
                        }

                        if (nvalue > 0.0f) {
                            tvalue += nvalue;
                        }
                        i++;
                    } else {
                        if (new Float(guidance).isNaN()) {
                            value = fgb.getValue(
                                    interpolation.getStandardSource(),
                                    interpolation, expiration);
                        } else {
                            value = guidance;
                        }
                        if (value > 0.0f) {
                            tvalue += value;
                        }
                        i++;
                    }
                }
            }
        }

        if (tvalue == 0.0f) {
            return Float.NaN;
        }

        return tvalue / i;
    }

    /**
     * Extracts the Max value for a guidance aggregation of basins THIS IS
     * BACKWARDS FROM LOGIC THOUGH!!!!, lower GUIDANCE values are actually worse
     * 
     * 
     * @param pfaf_ids
     * @param sourceName
     * @return
     */
    public float getMaxGuidanceValue(ArrayList<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, long expiration,
            long parentPfaf) {
        float tvalue = Float.NaN;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                FFMPGuidanceBasin fgb = (FFMPGuidanceBasin) basin;
                fgb.setCountyFips(parentPfaf);
                if (interpolation.isInterpolate()) {
                    float nvalue = fgb.getInterpolatedValue(
                            interpolation.getSource1(),
                            interpolation.getSource2(),
                            interpolation.getInterpolationOffset(),
                            interpolation, expiration);
                    // ignore missing values
                    if (nvalue < -999) {
                        continue;
                    }
                    if (((nvalue < tvalue) && (nvalue > 0.0f))
                            || Float.isNaN(tvalue)) {
                        tvalue = nvalue;
                    }
                } else {
                    float value = fgb.getValue(
                            interpolation.getStandardSource(), interpolation,
                            expiration);
                    // ignore missing values
                    if (value < -999) {
                        continue;
                    }
                    if (((value < tvalue) && (value > 0.0f))
                            || Float.isNaN(tvalue)) {
                        tvalue = value;
                    }
                }
            }
        }

        if (tvalue == 0.0f) {
            tvalue = Float.NaN;
        }

        return tvalue;
    }

    /**
     * Extracts the Accumulated Max value for an aggregation of basins
     * 
     * @param pfaf_ids
     * @param beforeDate
     * @param afterDate
     * @return
     */
    public float getAccumMaxValue(ArrayList<Long> pfaf_ids, Date beforeDate,
            Date afterDate, long expirationTime, boolean rate) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {

            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
          
                float val = basin.getAccumValue(afterDate, beforeDate,
                        expirationTime, rate);
             
                if (val > tvalue) {
                    tvalue = val;
                }
            }
        }
        
        return tvalue;
    }

    /**
     * used for max ratio and diff calcs
     * 
     * @param pfaf_ids
     * @param interpolation
     * @return
     */
    public ArrayList<Float> getGuidanceValues(ArrayList<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, long expiration) {
        ArrayList<Float> values = new ArrayList<Float>();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                FFMPGuidanceBasin fgb = (FFMPGuidanceBasin) basin;
                if (interpolation.isInterpolate()) {
                    values.add(fgb.getInterpolatedValue(
                            interpolation.getSource1(),
                            interpolation.getSource2(),
                            interpolation.getInterpolationOffset(),
                            interpolation, expiration));
                } else {
                    values.add(fgb.getValue(interpolation.getStandardSource(),
                            interpolation, expiration));
                }
            }
        }
        return values;
    }

    /**
     * used for max ratio and diff calcs
     * 
     * @param pfaf_ids
     * @param beforeDate
     * @param afterDate
     * @param rate
     * @return
     */
    public ArrayList<Float> getAccumValues(ArrayList<Long> pfaf_ids,
            Date beforeDate, Date afterDate, long expirationTime, boolean rate) {
        ArrayList<Float> values = new ArrayList<Float>();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = basins.get(pfaf);
            if (basin != null) {
                values.add(basin.getAccumValue(beforeDate, afterDate,
                        expirationTime, rate));
            }
        }
        return values;
    }

    /**
     * Purge out old data
     * 
     * @param date
     */
    public void purgeData(Date date) {
        for (FFMPBasin basin : basins.values()) {
            basin.purgeData(date);
        }
    }
    
    /**
     * populates data from the cache
     * 
     * @param times
     */
    public void populate(List<Long> times) {
        for (FFMPBasin basin : basins.values()) {
            basin.populate(times);
        }
    }

    /**
     * populates the serialized array/objects
     */
    public void setCache() {
        for (FFMPBasin basin : basins.values()) {
            basin.setCache();
        }
    }

}
