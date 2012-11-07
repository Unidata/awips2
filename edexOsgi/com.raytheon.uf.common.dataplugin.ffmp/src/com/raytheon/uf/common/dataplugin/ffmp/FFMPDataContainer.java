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
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * FFTI Data Container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/31/11     5489     D. Hladky   Initial release
 * 07/31/12     578      D.Hladky    finished it
 * 09/27/12		DR 15471  G.Zhang	 Fixed ConcurrentModificationException
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPDataContainer {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataContainer.class);

    private final ConcurrentHashMap<String, FFMPBasinData> basinDataMap = new ConcurrentHashMap<String, FFMPBasinData>();// DR

    private String sourceName = null;

    private String filePath = null;

    public FFMPDataContainer() {
        // public unused constructor
    }

    public FFMPDataContainer(String sourceName) {
        this.sourceName = sourceName;
        basinDataMap.put("ALL", new FFMPBasinData("ALL"));
        // System.out.println("Creating source: " + sourceName);
    }

    public FFMPDataContainer(String sourceName, ArrayList<String> hucs) {
        // System.out.println("Creating source with hucs: " + sourceName);
        this.sourceName = sourceName;
        for (String huc : hucs) {
            basinDataMap.put(huc, new FFMPBasinData(huc));
        }
    }

    /**
     * Adds to the cache
     * 
     * @param date
     * @param newBasinData
     */
    public void addFFMPEntry(Date date, SourceXML source,
            FFMPBasinData newBasinData, String huc, String siteKey) {

        boolean guid = false;

        if (source.getSourceType().equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
            guid = true;
        }

        FFMPBasinData currBasinData = getBasinData(huc);

        synchronized (currBasinData) {

            if (currBasinData == null) {
                setBasinData(huc, newBasinData);
            } else {

                for (Long key : newBasinData.getBasins().keySet()) {

                    if (guid) {

                        FFMPGuidanceBasin basin = null;

                        if (currBasinData.get(key) instanceof FFMPGuidanceBasin) {
                            basin = (FFMPGuidanceBasin) currBasinData.get(key);
                        }

                        if (basin == null) {

                            FFMPBasin newbasin = newBasinData.get(key);
                            basin = new FFMPGuidanceBasin(key,
                                    newbasin.getAggregated());

                            if (newbasin instanceof FFMPGuidanceBasin) {

                                Float val = ((FFMPGuidanceBasin) newbasin)
                                        .getValue(date, source.getSourceName());
                                basin.setValue(source.getSourceName(), date,
                                        val);
                            } else {
                                Float val = newbasin.getValue(date);
                                basin.setValue(source.getSourceName(), date,
                                        val);
                            }

                            //currBasinData.put(key, basin);
                            syncPut(currBasinData, key, basin);
                        } else {

                            FFMPBasin newbasin = newBasinData.get(key);

                            if (newbasin instanceof FFMPGuidanceBasin) {

                                FFMPGuidanceBasin newGbasin = (FFMPGuidanceBasin) newBasinData
                                        .get(key);
                                Float basinVal = basin.getValue(date,
                                        source.getSourceName());
                                Float newBasinVal = newGbasin.getValue(date,
                                        source.getSourceName());

                                if (basinVal != null && basinVal >= 0.0f
                                        && !basinVal.isNaN()
                                        && basinVal != FFMPUtils.MISSING) {

                                    if (newBasinVal != null
                                            && newBasinVal >= 0.0f
                                            && !newBasinVal.isNaN()
                                            && newBasinVal != FFMPUtils.MISSING) {

                                        float val = (float) ((basinVal + newBasinVal) / 2.0);
                                        basin.setValue(source.getSourceName(),
                                                date, val);
                                    }
                                } else {

                                    if (newBasinVal.isNaN()) {
                                        newBasinVal = FFMPUtils.MISSING;
                                    }

                                    basin.setValue(source.getSourceName(),
                                            date, newBasinVal);
                                }

                            } else {

                                Float basinVal = basin.getValue(date,
                                        source.getSourceName());
                                Float newBasinVal = newbasin.getValue();

                                if (basinVal != null && basinVal >= 0.0f
                                        && !basinVal.isNaN()
                                        && basinVal != FFMPUtils.MISSING) {

                                    if (newBasinVal != null
                                            && newBasinVal >= 0.0f
                                            && !newBasinVal.isNaN()
                                            && newBasinVal != FFMPUtils.MISSING) {

                                        float val = (float) ((basinVal + newBasinVal) / 2.0);
                                        basin.setValue(source.getSourceName(),
                                                date, val);
                                    }
                                } else {

                                    if (newBasinVal.isNaN()) {
                                        newBasinVal = FFMPUtils.MISSING;
                                    }

                                    basin.setValue(source.getSourceName(),
                                            date, newBasinVal);
                                }
                            }
                        }

                    } else {

                        FFMPBasin basin = currBasinData.get(key);
                        FFMPBasin newbasin = newBasinData.get(key);
                        Float val = 0.0f;

                        if (basin == null) {

                            basin = new FFMPBasin(key, newbasin.getAggregated());
                            val = newbasin.getValue(date);

                            if (val.isNaN()) {
                                val = 0.0f;
                            }

                            basin.setValue(date, val);
                            //currBasinData.put(key, basin);
                            syncPut(currBasinData, key, basin);
                        } else {

                            if (basin.getValue(date) != null
                                    && !basin.getValue(date).isNaN()
                                    && basin.getValue(date) != 0.0) {
                                if (newbasin.getValue(date) != null
                                        && !newbasin.getValue(date).isNaN()
                                        && newbasin.getValue(date) != 0.0) {

                                    val = (float) ((basin.getValue(date) + newbasin
                                            .getValue(date)) / 2);
                                }

                            } else {
                                val = newbasin.getValue(date);

                                if (val.isNaN()) {
                                    val = 0.0f;
                                }
                            }

                            basin.setValue(date, val);
                        }
                    }
                }
            }
        }
    }

    /**
     * check for the key
     * 
     * @param date
     * @return
     */
    public boolean containsKey(Date date) {
        boolean contains = false;

        if (getBasinData("ALL") != null) {

            HashMap<Long, FFMPBasin> basins = getBasinData("ALL").getBasins();

            synchronized (basins) {
                for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                    FFMPBasin basin = entry.getValue();
                    contains = basin.getValues().containsKey(date);
                    if (contains == true) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * check for the key
     * 
     * @param date
     * @return
     */
    public boolean containsKey(String sourceName) {
        boolean contains = false;
        HashMap<Long, FFMPBasin> basins = getBasinData("ALL").getBasins();

        synchronized (basins) {
            for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                FFMPBasin basin = entry.getValue();
                if (basin instanceof FFMPGuidanceBasin) {
                    contains = ((FFMPGuidanceBasin) basin)
                            .containsKey(sourceName);
                    if (contains == true) {
                        // System.out.println("Contains Key: " + sourceName);
                        return true;
                    }
                }
            }
        }

        // System.out.println("No Key: " + sourceName);
        return false;
    }

    /**
     * Get the one you are looking for
     * 
     * @return
     */
    public FFMPBasinData getBasinData(String huc) {
        if (basinDataMap.containsKey(huc)) {
            return basinDataMap.get(huc);
        } else {
            return null;
        }
    }

    public String getFilePath() {
        return filePath;
    }

    public Set<String> getKeys() {
        return basinDataMap.keySet();
    }

    /**
     * Get the maximum value in the monitored area.
     * 
     * @param pfafs
     * @param backDate
     * @param currDate
     * @param expirationTime
     * @param rate
     * @return
     */
    public double getMaxValue(ArrayList<Long> pfafs, Date backDate,
            Date currDate, long expirationTime, boolean rate) {

        double val = getBasinData("ALL").getAccumMaxValue(pfafs, currDate,
                backDate, expirationTime, rate);

        return val;
    }

    /**
     * check for the newest key
     * 
     * @return
     */
    public Date getNewest() {
        try {

            HashMap<Long, FFMPBasin> basins = getBasinData("ALL").getBasins();

            synchronized (basins) {
                for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                    FFMPBasin basin = entry.getValue();
                    if (basin instanceof FFMPGuidanceBasin) {
                        ((FFMPGuidanceBasin) basin).getGuidValues().lastKey();
                    } else {
                        return basin.getValues().lastKey();
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.debug("No new times available..." + getSourceName());
            return null;
        }

        return null;
    }

    /**
     * check for the oldest key
     * 
     * @return
     */
    public Date getOldest() {
        try {
            HashMap<Long, FFMPBasin> basins = getBasinData("ALL").getBasins();

            synchronized (basins) {
                for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                    FFMPBasin basin = entry.getValue();
                    if (basin instanceof FFMPGuidanceBasin) {
                        ((FFMPGuidanceBasin) basin).getGuidValues().firstKey();
                    } else {
                        return basin.getValues().firstKey();
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.debug("No old times available..." + getSourceName());
            return null;
        }
        return null;
    }

    /**
     * Gets the list of ordered time keys
     * 
     * @param barrierTime
     * @return
     */
    public ArrayList<Date> getOrderedTimes(Date barrierTime) {
        ArrayList<Date> orderedTimes = new ArrayList<Date>();
        try {
            HashMap<Long, FFMPBasin> basins = getBasinData("ALL").getBasins();

            synchronized (basins) {
                for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                    FFMPBasin basin = entry.getValue();
                    for (Date time : basin.getValues().descendingKeySet()) {
                        if (time.after(barrierTime)) {
                            orderedTimes.add(time);
                        }
                    }

                    Collections.reverse(orderedTimes);

                    return orderedTimes;
                }
            }
        } catch (Exception e) {
            statusHandler.debug("No ordered times available..."
                    + getSourceName());
            return null;
        }

        return null;
    }

    public String getSourceName() {
        return sourceName;
    }

    /*
     * clean up old junk
     */
    public void purge(Date backDate) {
        for (String huc : basinDataMap.keySet()) {
            getBasinData(huc).purgeData(backDate);
        }
    }

    /**
     * maybe this will work
     * 
     * @param basins
     * @param hucName
     */
    public void setBasinBuddyData(FFMPBasinData basins, String hucName) {

        for (Entry<Long, FFMPBasin> entry : basins.getBasins().entrySet()) {
            FFMPBasin basin = getBasinData(hucName).get(entry.getKey());
            if (basin != null) {
                if (basin instanceof FFMPGuidanceBasin) {
                    FFMPGuidanceBasin gbasin = (FFMPGuidanceBasin) basin;
                    gbasin.getGuidValues().putAll(
                            ((FFMPGuidanceBasin) entry.getValue())
                                    .getGuidValues());
                } else {
                    basin.getValues().putAll(entry.getValue().getValues());
                }
            } else {
            	syncPut(getBasinData(hucName), entry.getKey(), entry.getValue());
                //getBasinData(hucName).put(entry.getKey(), entry.getValue());
            }
        }
    }

    /**
     * Add a brand new one for new source, or initialization
     * 
     * @param basins
     */
    public void setBasinData(String huc, FFMPBasinData fftiData) {
        basinDataMap.put(huc, fftiData);
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    /**
     * check for size
     * 
     * @param date
     * @return
     */
    public int size() {

        HashMap<Long, FFMPBasin> basins = getBasinData("ALL").getBasins();

        synchronized (basins) {
            for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                FFMPBasin basin = entry.getValue();
                if (basin instanceof FFMPGuidanceBasin) {
                    return ((FFMPGuidanceBasin) basin).getGuidValues().size();
                } else {
                    return basin.getValues().size();
                }
            }
        }
        return 0;
    }
    
    /**
     * DR 15471 lock put() to avoid ConcurrentModificationException     
     */    
    
    private void syncPut(FFMPBasinData fbd, Long key, FFMPBasin value){
    	if(fbd==null || key==null) 
    		return;
    	
    	HashMap<Long,FFMPBasin> basins = fbd.getBasins();
    	if(basins == null)
    		return;
    	
    	synchronized (basins) {
           basins.put(key, value);
        }
    }
}
