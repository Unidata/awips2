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

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataRecordLoader.LoadTask;
import com.raytheon.uf.common.dataplugin.ffmp.collections.BasinMapFactory;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * 01/27/13      1569       D. Hladky   Added support for write of aggregate record cache
 * 04/16/13      1912       bsteffen    Initial bulk hdf5 access for ffmp
 * 05/09/13      1919       mpduff      Use parent pfaf instead of lookupId.
 * 07/09/13      2152       njensen     Ensure purgeData() does not load data
 * Jul 15, 2013 2184        dhladky     Remove all HUC's for storage except ALL
 * 07/16/13      2197       njensen     Added hasAnyBasins() and moved getBasins() calls out of loops
 * Jul 31, 2013  2242       bsteffen    Optimize FFMP NavigableMap memory.
 * Aug 08, 2015  4722       dhladky     Dynamic serialize imp not needed.
 * Aug 31, 2015  4780       dhladky     Corrected guidance basin mosaic averaging logic.
 * Oct 26, 2015  5056       dhladky     Simplified guidance interpolator.
 * Jul 02, 2018  6641       njensen     Use List interface instead of ArrayList
 * Aug 14, 2018  6720       njensen     Use simplified enums
 * Mar 13, 2020  21883      dhaines     Added check for matching lengths of basin and value arrays in
 *                                      process method
 * 
 * </pre>
 * 
 * @author dhladky
 */
@DynamicSerialize
public class FFMPBasinData {

    public static final double GUIDANCE_MISSING = -999999.0;

    // defaults
    
    /** @deprecated this doesn't appear to be used by anything */
    @DynamicSerializeElement
    @Deprecated
    private String hucLevel;

    @DynamicSerializeElement
    private Map<Long, FFMPBasin> basins = new HashMap<>();

    /**
     * Pending load tasks that need to be run to fully populate basins
     */
    private final List<LoadTask> tasks = new ArrayList<>();

    /**
     * Cache of basins in order for easy population from Load Tasks.
     */
    private final Map<String, FFMPBasin[]> orderedBasinsCache = new HashMap<>();

    /**
     * Shared factory for efficient storage of data in basins.
     */
    private BasinMapFactory<Date> mapFactory = null;

    private IUFStatusHandler statusHandler = UFStatus.getHandler();
    
    /**
     * Public one arg constructor
     * 
     * @param hucLevel
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
    public Map<Long, FFMPBasin> getBasins() {
        if (!tasks.isEmpty()) {
            loadNow();
        }
        return basins;
    }

    /**
     * Sets the basin map
     * 
     * @param basins
     */
    public void setBasins(Map<Long, FFMPBasin> basins) {
        if (!tasks.isEmpty()) {
            synchronized (tasks) {
                tasks.clear();
                orderedBasinsCache.clear();
            }
        }
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
        getBasins().put(key, basin);
    }

    /**
     * Gets the FFMPBasin from the hash
     * 
     * @param key
     * @return
     */
    public FFMPBasin get(Long key) {
        return getBasins().get(key);
    }

    /**
     * Extracts the average value for an aggregation of basins
     * 
     * @param pfaf_ids
     * @param beforeDate
     * @param afterDate
     * @return
     */
    public float getAverageValue(List<Long> pfaf_ids, Date beforeDate,
            Date afterDate) {
        float tvalue = 0.0f;
        int i = 0;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
            if (basin != null) {
                tvalue += basin.getValue(beforeDate, afterDate);
                i++;
            }
        }
        tvalue = tvalue / i;

        return tvalue;
    }

    /**
     * Extracts the average value for an aggregation of basins
     * 
     * @param pfaf_ids
     * @param date
     * @return
     */
    public float getAverageValue(List<Long> pfaf_ids, Date date) {
        float tvalue = 0.0f;
        int i = 0;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
            if (basin != null) {
                tvalue += basin.getValue(date);
                i++;
            }
        }
        tvalue = tvalue / i;

        return tvalue;
    }

    /**
     * Extracts the average value for an aggregation of basins
     * 
     * @param pfaf_ids
     * @param date
     * @param expirationTime
     * @return
     */
    public float getAverageValue(List<Long> pfaf_ids, Date date,
            long expirationTime) {

        float tvalue = 0.0f;
        int i = 0;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);
            if (basin != null) {
                tvalue += basin.getAverageValue(date, expirationTime);
                i++;
            }
        }
        tvalue = tvalue / i;

        return tvalue;
    }

    /**
     * Extracts the average value for an aggregation of basins to a given time
     * 
     * @param pfaf_ids
     * @return
     */
    public float getAccumAverageValue(List<Long> pfaf_ids, Date beforeDate,
            Date afterDate, long expirationTime, boolean rate) {
        float tvalue = 0.0f;
        int i = 0;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
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
     * @param beforeDate
     * @param afterDate
     * @return
     */
    public float getMaxValue(List<Long> pfaf_ids, Date beforeDate,
            Date afterDate) {
        float tvalue = 0.0f;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
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
     * 
     * @param pfaf_ids
     * @param date
     * @param expiration
     * @return
     */
    public float getAverageMaxValue(List<Long> pfaf_ids, Date date,
            long expiration) {
        float tvalue = 0.0f;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
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
     * 
     * @param pfaf_ids
     * @param afterDate
     * @param beforeDate
     * @return
     */
    public float getAverageMaxValue(List<Long> pfaf_ids, Date afterDate,
            Date beforeDate) {
        float tvalue = 0.0f;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
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
     * @param date
     * @return
     */
    public float getMaxValue(List<Long> pfaf_ids, Date date) {
        float tvalue = 0.0f;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
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
     * @param interpolation
     * @param guidance
     * @param forcedPfafs
     * @param expiration
     * @return
     */
    public float getAverageGuidanceValue(List<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, float guidance,
            List<Long> forcedPfafs, long expiration) {
        float tvalue = 0.0f;
        float value;
        int i = 0;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);

            if (basin == null) {
                return guidance;
            }

            FFMPGuidanceBasin fgb = (FFMPGuidanceBasin) basin;
            if (forcedPfafs.contains(pfaf)) {
                if (interpolation.isInterpolate()) {
                    value = fgb.getInterpolatedValue(interpolation, expiration);
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
                        nvalue = fgb.getInterpolatedValue(interpolation,
                                expiration);
                    } else {
                        nvalue = guidance;
                    }

                    if (nvalue > 0.0f) {
                        tvalue += nvalue;
                    }
                    i++;
                } else {
                    if (new Float(guidance).isNaN()) {
                        value = fgb.getValue(interpolation.getStandardSource(),
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
     * @param interpolation
     * @param expiration
     * @param parentPfaf
     * @return
     */
    public float getMaxGuidanceValue(List<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, long expiration,
            long parentPfaf) {
        float tvalue = Float.NaN;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
            if (basin != null) {
                FFMPGuidanceBasin fgb = (FFMPGuidanceBasin) basin;
                fgb.setCountyFips(parentPfaf);
                if (interpolation.isInterpolate()) {
                    float nvalue = fgb.getInterpolatedValue(interpolation,
                            expiration);
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
     * @param expirationTime
     * @param rate
     * @return
     */
    public float getAccumMaxValue(List<Long> pfaf_ids, Date beforeDate,
            Date afterDate, long expirationTime, boolean rate) {
        float tvalue = 0.0f;
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
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
    public List<Float> getGuidanceValues(List<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, long expiration) {
        List<Float> values = new ArrayList<>();
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
            if (basin != null) {
                FFMPGuidanceBasin fgb = (FFMPGuidanceBasin) basin;
                if (interpolation.isInterpolate()) {
                    values.add(fgb.getInterpolatedValue(interpolation,
                            expiration));
                } else {
                    values.add(fgb.getValue(interpolation.getStandardSource(),
                            interpolation, expiration));
                }
            }
        }
        return values;
    }

    /**
     * Gets the average guidance value for an aggregate basin
     * 
     * @param pfaf_ids
     * @param interpolation
     * @param expiration
     * @return
     */
    public Float getAverageGuidanceValue(List<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, long expiration) {
        float tvalue = 0.0f;
        int i = 0;

        List<Float> vals = getGuidanceValues(pfaf_ids, interpolation,
                expiration);
        if (vals != null) {
            for (Float val : vals) {
                tvalue += val;
                i++;
            }
        } else {
            return null;
        }

        return tvalue / i;
    }

    /**
     * used for max ratio and diff calcs
     * 
     * @param pfaf_ids
     * @param beforeDate
     * @param afterDate
     * @param expirationTime
     * @param rate
     * @return
     */
    public List<Float> getAccumValues(List<Long> pfaf_ids, Date beforeDate,
            Date afterDate, long expirationTime, boolean rate) {
        List<Float> values = new ArrayList<>();
        Map<Long, FFMPBasin> localBasins = getBasins();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = localBasins.get(pfaf);
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
        // remove old tasks before calling getBasins() since that may
        // cause them to run
        if (!tasks.isEmpty()) {
            synchronized (tasks) {
                Iterator<LoadTask> itr = tasks.iterator();
                {
                    while (itr.hasNext()) {
                        LoadTask task = itr.next();
                        if (task instanceof LoadMapTask) {
                            LoadMapTask mtask = (LoadMapTask) task;
                            if (mtask.date.before(date)) {
                                itr.remove();
                            }
                        }
                    }
                }
                if (tasks.isEmpty()) {
                    orderedBasinsCache.clear();
                }
            }
        }
        for (FFMPBasin basin : basins.values()) {
            basin.purgeData(date);
        }
    }

    /**
     * deserialize data from the aggregate record
     * 
     * @param times
     */
    public void populate(List<Long> times) {
        if (mapFactory == null) {
            mapFactory = new BasinMapFactory<>(Collections.reverseOrder(),
                    getBasins().size());
        }

        long[] timesArr = new long[times.size()];
        for (int i = 0; i < timesArr.length; i += 1) {
            timesArr[i] = times.get(i);
        }
        for (FFMPBasin basin : getBasins().values()) {
            basin.deserialize(timesArr, mapFactory);
        }
    }

    /**
     * populates the serialized array/objects
     */
    public void serialize() {
        for (FFMPBasin basin : getBasins().values()) {
            basin.serialize();
        }
    }

    /**
     * Add basins some basins from a datastoreFile. The basins will not be
     * loaded immediately, they will be loaded when they are needed.
     * 
     * @param datastoreFile
     *            - the file containing data.
     * @param uri
     *            - datauri of record to load
     * @param siteKey
     *            - siteKey to load
     * @param cwa
     *            - cwa to load
     * @param huc
     *            - huc to load
     * @param sourceName
     *            - the sourceName for the data.
     * @param date
     *            - the date of the data.
     * @param orderedPfafs
     *            - a collection of Longs which is in the same order as the data
     *            in the dataStore.
     * @param aggregate
     */
    public void addBasins(File datastoreFile, String uri, String siteKey,
            String cwa, String huc, String sourceName, Date date,
            Collection<Long> orderedPfafs, boolean aggregate) {
        SourceXML source = FFMPSourceConfigurationManager.getInstance()
                .getSource(sourceName);
        boolean guidance = source.isGuidance();
        String basinsKey = siteKey + ' ' + cwa + ' ' + huc;
        String datasetGroupPath = uri + DataStoreFactory.DEF_SEPARATOR + cwa
                + DataStoreFactory.DEF_SEPARATOR + huc;

        synchronized (tasks) {
            FFMPBasin[] basins = this.orderedBasinsCache.get(basinsKey);
            if (basins == null) {
                basins = new FFMPBasin[orderedPfafs.size()];
                int j = 0;
                for (Long pfaf : orderedPfafs) {
                    FFMPBasin basin = this.basins.get(pfaf);
                    if (basin == null) {
                        if (guidance) {
                            basin = new FFMPGuidanceBasin(pfaf, aggregate);
                        } else {
                            if (mapFactory == null) {
                                mapFactory = new BasinMapFactory<>(
                                        Collections.reverseOrder(),
                                        orderedPfafs.size());
                            }
                            basin = new FFMPBasin(pfaf, aggregate, mapFactory);
                        }
                        this.basins.put(pfaf, basin);
                    }
                    basins[j] = basin;
                    j++;
                }
                this.orderedBasinsCache.put(basinsKey, basins);
            }
            if (guidance) {
                tasks.add(new LoadGuidanceMapTask(datastoreFile,
                        datasetGroupPath, basins, date, sourceName));
            } else {
                tasks.add(new LoadMapTask(datastoreFile, datasetGroupPath,
                        basins, date));
            }
        }
    }

    /**
     * Add virtual basins from a datastoreFile. The basins will not be loaded
     * immediately, they will be loaded when they are needed.
     * 
     * @param datastoreFile
     *            - the file containing data.
     * @param uri
     *            - datauri of record to load
     * @param dataKey
     *            - dataKey to load
     * @param cwa
     *            - cwa to load
     * @param date
     *            - the date of the data.
     * @param orderedMetadata
     *            - a collection of FFMPVirtualGageBasinMetaData which is in the
     *            same order as the data in the dataStore.
     */
    public void addVirtualBasins(File datastoreFile, String uri, String dataKey,
            String cwa, Date date,
            Collection<FFMPVirtualGageBasinMetaData> orderedMetadata) {
        String basinsKey = dataKey + ' ' + cwa;
        String datasetGroupPath = uri + DataStoreFactory.DEF_SEPARATOR + cwa
                + DataStoreFactory.DEF_SEPARATOR + FFMPRecord.ALL;
        synchronized (tasks) {
            FFMPBasin[] basins = this.orderedBasinsCache.get(basinsKey);
            if (basins == null) {
                basins = new FFMPBasin[orderedMetadata.size()];
                int j = 0;
                for (FFMPVirtualGageBasinMetaData fvgbmd : orderedMetadata) {
                    FFMPBasin basin = this.basins.get(fvgbmd.getLookupId());
                    if (basin == null) {
                        basin = new FFMPVirtualGageBasin(fvgbmd.getLid(),
                                fvgbmd.getParentPfaf(), false);
                        this.basins.put(fvgbmd.getLookupId(), basin);
                    }
                    basins[j] = basin;
                    j++;
                }
                this.orderedBasinsCache.put(basinsKey, basins);
            }
            tasks.add(new LoadVirtualMapTask(datastoreFile, datasetGroupPath,
                    basins, date));
        }
    }

    /**
     * Returns whether or not any basins have been put in the basins map. Faster
     * than calling getBasins().size() or getBasins().isEmpty().
     * 
     * @return
     */
    public boolean hasAnyBasins() {
        return !basins.isEmpty();
    }

    /**
     * Load now.
     */
    public void loadNow() {
        synchronized (tasks) {
            if (!tasks.isEmpty()) {
                FFMPDataRecordLoader.loadRecords(tasks);
                tasks.clear();
                orderedBasinsCache.clear();
            }
        }
    }

    /**
     * Base task for loading data from a dataRecord into FFMPBasins
     */
    private class LoadMapTask extends LoadTask {

        protected final FFMPBasin[] basins;

        protected final Date date;

        public LoadMapTask(File datastoreFile, String datasetGroupPath,
                FFMPBasin[] basins, Date date) {
            super(datastoreFile, datasetGroupPath);
            this.basins = basins;
            this.date = date;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void process(FloatDataRecord record) {
            float[] values = record.getFloatData();
            
            /** Added check for matching lengths of basin and value arrays in
             *  process method - they can be mismatched in cases where a site's
             *  basin shapefile is updated and and they don't purge and
             *  re-ingest the ARI data files, in which case the basin table
             *  failed to render. This change allows the table to render even
             *  in this case. **/
            if (values.length != basins.length) {
                StringBuilder msg = new StringBuilder("The number of basins and"
                        + " values for group: " + record.getGroup()
                        + " do not match. The source of this group will be unavailable"
                        + " to FFMP for this session.");
                
                if (record.getGroup().contains("ARI")) {
                    msg.append(" Please purge and reingest the ARI data"
                            + " if you want to use this source.");
                } else {
                    msg.append(" If you require use of this source, please open"
                            + " a troubleticket through NCF for assistance.");
                }
                
                statusHandler.warn(msg.toString());
                return;
            }
            
            for (int j = 0; j < values.length; j++) {
                applyValue(basins[j], values[j]);
            } 
        }

        /**
         * Apply the value to the basin/
         */
        protected void applyValue(FFMPBasin basin, float value) {
            if (basin.contains(date)) {
                float curval = basin.getValue(date);
                // These are QPF and QPE so, 0.0 is a valid amount
                if (curval >= 0.0f && value >= 0.0f) {
                    basin.setValue(date, (curval + value) / 2);
                } else if (value >= 0.0f) {
                    basin.setValue(date, value);
                }
                // do not overwrite original value
            } else {
                // no value at time exists, write regardless
                basin.setValue(date, value);
            }
        }
    }

    /**
     * Task for loading data from a dataRecord into FFMPGuidanceBasins
     */
    private class LoadGuidanceMapTask extends LoadMapTask {

        private final String sourceName;

        public LoadGuidanceMapTask(File datastoreFile, String datasetGroupPath,
                FFMPBasin[] basins, Date date, String sourceName) {
            super(datastoreFile, datasetGroupPath, basins, date);
            this.sourceName = sourceName;
        }

        @Override
        protected void applyValue(FFMPBasin basin, float value) {

            FFMPGuidanceBasin gBasin = (FFMPGuidanceBasin) basin;
            Float curval = gBasin.getValue(date, sourceName);

            if (curval != FFMPUtils.MISSING && !curval.isNaN()) {
                // average of original and new value
                if (curval > 0.0f && value > 0.0f) {
                    gBasin.setValue(sourceName, date, (curval + value) / 2);
                } else {
                    // curval zero, overwrite original value
                    if (curval == 0.0f) {
                        gBasin.setValue(sourceName, date, value);
                    }
                }
            } else {
                // curval NaN or MISSING, Overwrite original value
                gBasin.setValue(sourceName, date, value);
            }
        }

    }

    /**
     * Task for loading data from a dataRecord into FFMPVirtualGageBasins
     */
    private class LoadVirtualMapTask extends LoadMapTask {

        public LoadVirtualMapTask(File datastoreFile, String datasetGroupPath,
                FFMPBasin[] basins, Date date) {
            super(datastoreFile, datasetGroupPath, basins, date);
        }

        @Override
        protected void applyValue(FFMPBasin basin, float value) {
            basin.setValue(date, value);
        }

    }

}

