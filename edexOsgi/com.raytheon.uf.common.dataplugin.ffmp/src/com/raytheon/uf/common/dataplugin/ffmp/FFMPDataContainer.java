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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * FFMP Data Container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/31/11     5489        D. Hladky   Initial release
 * 07/31/12     578         D.Hladky    finished it
 * 09/27/12     DR 15471    G.Zhang     Fixed ConcurrentModificationException
 * 01/27/13     1478        D. Hladky   Re-worked to help with memory size and NAS read write stress
 * Apr 16, 2013 1912        bsteffen    Initial bulk hdf5 access for ffmp
 * 07/03/13     2131        D. Hladky   Fixed null pointers thrown by new container creation.
 * Jul 15, 2013 2184        dhladky     Remove all HUC's for storage except ALL
 * Aug 14, 2018 6720        njensen     Use simplified enums
 * 
 * </pre>
 * 
 * @author dhladky
 */

public class FFMPDataContainer {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataContainer.class);

    private FFMPBasinData basins;

    private String sourceName = null;

    private boolean isPurged = false;

    /**
     * Constructor
     */
    public FFMPDataContainer() {

    }

    /**
     * Constructor
     * 
     * @param sourceName
     */
    public FFMPDataContainer(String sourceName) {
        this.sourceName = sourceName;
        basins = new FFMPBasinData();
    }

    /**
     * new container first time read in from cache
     * 
     * @param sourceName
     * @param record
     */
    public FFMPDataContainer(String sourceName, FFMPAggregateRecord record) {
        this.sourceName = sourceName;

        FFMPBasinData basinData = record.getBasins();
        if (basinData != null) {
            basinData.populate(record.getTimes());
            basins = basinData;

        }
    }

    /**
     * Adds to the cache
     * 
     * @param date
     * @param source
     * @param newBasinData
     */
    public void addFFMPEntry(Date date, SourceXML source,
            FFMPBasinData newBasinData) {
        boolean guid = source.isGuidance();

        FFMPBasinData currBasinData = getBasinData();
        if (currBasinData == null) {
            setBasinData(newBasinData);
        } else {
            synchronized (currBasinData) {
                for (Long key : newBasinData.getBasins().keySet()) {
                    if (guid) {
                        FFMPGuidanceBasin basin = null;
                        if (currBasinData
                                .get(key) instanceof FFMPGuidanceBasin) {
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

                                        float val = (float) ((basinVal
                                                + newBasinVal) / 2.0);
                                        basin.setValue(source.getSourceName(),
                                                date, val);
                                    }
                                } else {
                                    if (newBasinVal.isNaN()) {
                                        newBasinVal = FFMPUtils.MISSING;
                                    }
                                    basin.setValue(source.getSourceName(), date,
                                            newBasinVal);
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

                                        float val = (float) ((basinVal
                                                + newBasinVal) / 2.0);
                                        basin.setValue(source.getSourceName(),
                                                date, val);
                                    }
                                } else {
                                    if (newBasinVal.isNaN()) {
                                        newBasinVal = FFMPUtils.MISSING;
                                    }

                                    basin.setValue(source.getSourceName(), date,
                                            newBasinVal);
                                }
                            }
                        }
                    } else {
                        FFMPBasin basin = currBasinData.get(key);
                        FFMPBasin newbasin = newBasinData.get(key);
                        Float val = 0.0f;

                        if (basin == null) {

                            basin = new FFMPBasin(key,
                                    newbasin.getAggregated());
                            val = newbasin.getValue(date);

                            if (val.isNaN()) {
                                val = 0.0f;
                            }

                            basin.setValue(date, val);
                            // currBasinData.put(key, basin);
                            syncPut(currBasinData, key, basin);
                        } else {
                            if (basin.getValue(date) != null
                                    && !basin.getValue(date).isNaN()
                                    && basin.getValue(date) != 0.0) {
                                if (newbasin.getValue(date) != null
                                        && !newbasin.getValue(date).isNaN()
                                        && newbasin.getValue(date) != 0.0) {

                                    val = (float) ((basin.getValue(date)
                                            + newbasin.getValue(date)) / 2);
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

        if (getBasinData() != null) {
            Map<Long, FFMPBasin> basins = getBasinData().getBasins();

            synchronized (basins) {
                for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                    FFMPBasin basin = entry.getValue();
                    contains = basin.getValues().containsKey(date);
                    if (contains) {
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
     * @param sourceName
     * @return
     */
    public boolean containsKey(String sourceName) {
        boolean contains = false;
        Map<Long, FFMPBasin> basins = getBasinData().getBasins();

        synchronized (basins) {
            for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                FFMPBasin basin = entry.getValue();
                if (basin instanceof FFMPGuidanceBasin) {
                    contains = ((FFMPGuidanceBasin) basin)
                            .containsKey(sourceName);
                    if (contains) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Get the one you are looking for
     * 
     * @return
     */
    public FFMPBasinData getBasinData() {
        if (basins != null) {
            return basins;
        } else {
            return null;
        }
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
    public double getMaxValue(List<Long> pfafs, Date backDate, Date currDate,
            long expirationTime, boolean rate) {
        double val = getBasinData().getAccumMaxValue(pfafs, currDate, backDate,
                expirationTime, rate);

        return val;
    }

    /**
     * check for the newest key
     * 
     * @return
     */
    public Date getNewest() {
        try {
            Map<Long, FFMPBasin> basins = getBasinData().getBasins();

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
            statusHandler.debug("No new times available..." + getSourceName(),
                    e);
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
            Map<Long, FFMPBasin> basins = getBasinData().getBasins();

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
            statusHandler.debug("No old times available..." + getSourceName(),
                    e);
        }

        return null;
    }

    /**
     * Gets the list of ordered time keys
     * 
     * @param barrierTime
     * @return
     */
    public List<Date> getOrderedTimes(Date barrierTime) {
        List<Date> orderedTimes = new ArrayList<>();
        try {
            Map<Long, FFMPBasin> basins = getBasinData().getBasins();

            synchronized (basins) {
                for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                    FFMPBasin basin = entry.getValue();
                    for (Date time : basin.getValues().descendingKeySet()) {
                        if (time.after(barrierTime)) {
                            orderedTimes.add(time);
                        }
                    }

                    return orderedTimes;
                }
            }
        } catch (Exception e) {
            statusHandler
                    .debug("No ordered times available..." + getSourceName(), e);
        }

        return null;
    }

    /**
     * Gets the list of times for serialization
     * 
     * @return
     */
    public List<Long> getOrderedTimes() {
        List<Long> orderedTimes = new ArrayList<>();
        try {
            Map<Long, FFMPBasin> basins = getBasinData().getBasins();

            synchronized (basins) {
                for (Entry<Long, FFMPBasin> entry : basins.entrySet()) {
                    FFMPBasin basin = entry.getValue();
                    for (Date time : basin.getValues().descendingKeySet()) {
                        orderedTimes.add(time.getTime());
                    }

                    return orderedTimes;
                }
            }
        } catch (Exception e) {
            statusHandler.debug(
                    "No ordered times available..." + getSourceName(), e);
        }

        return null;
    }

    /**
     * Gets the source name for this Data Container
     * 
     * @return
     */
    public String getSourceName() {
        return sourceName;
    }

    /**
     * Clean up old junk
     * 
     * @param backDate
     */
    public void purge(Date backDate) {
        statusHandler.handle(Priority.INFO, "Purging " + getSourceName()
                + " Container back to: " + backDate);
        getBasinData().purgeData(backDate);
        setPurged(true);
    }

    /**
     * Sets the Cache data for this container
     * 
     * @param cacheRecord
     */
    public void setAggregateData(FFMPAggregateRecord cacheRecord) {

        // create a record from the cache record
        FFMPRecord record = new FFMPRecord(cacheRecord);

        FFMPBasinData dentry = record.getBasinData();

        for (Entry<Long, FFMPBasin> entry : dentry.getBasins().entrySet()) {
            FFMPBasin basin = entry.getValue();
            if (basin != null) {
                if (basin instanceof FFMPGuidanceBasin) {
                    FFMPGuidanceBasin gbasin = (FFMPGuidanceBasin) basin;
                    gbasin.getGuidValues()
                            .putAll(((FFMPGuidanceBasin) entry.getValue())
                                    .getGuidValues());
                } else {
                    basin.getValues().putAll(entry.getValue().getValues());
                }
            } else {
                syncPut(getBasinData(), entry.getKey(), entry.getValue());
            }
        }

    }

    /**
     * Add a brand new one for new source, or initialization
     * 
     * @param basins
     */
    public void setBasinData(FFMPBasinData basins) {
        this.basins = basins;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    /**
     * check for size
     * 
     * @return
     */
    public int size() {
        Map<Long, FFMPBasin> basins = getBasinData().getBasins();

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
    private void syncPut(FFMPBasinData fbd, Long key, FFMPBasin value) {
        if (fbd == null || key == null) {
            return;
        }

        Map<Long, FFMPBasin> basins = fbd.getBasins();
        if (basins == null) {
            return;
        }

        synchronized (basins) {
            basins.put(key, value);
        }
    }

    /**
     * Sets whether this container has been purged or not
     * 
     * @param isPurged
     */
    public void setPurged(boolean isPurged) {
        this.isPurged = isPurged;
    }

    /**
     * Has this container been purged?
     * 
     * @return
     */
    public boolean isPurged() {
        return isPurged;
    }

    @Override
    public String toString() {
        return "FFMPDataContainer [basins=" + basins + ", sourceName="
                + sourceName + ", isPurged=" + isPurged + "]";
    }

}
