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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataRecordLoader.LoadTask;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.SourceXML;
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
 * 01/27/13      1569       D. Hladky   Added support for write of aggregate record cache
 * 04/16/13      1912       bsteffen    Initial bulk hdf5 access for ffmp
 * 05/09/13      1919       mpduff      Use parent pfaf instead of lookupId.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@DynamicSerialize
public class FFMPBasinData implements ISerializableObject {
    private static final long serialVersionUID = 8162247989509750715L;

    public static final double GUIDANCE_MISSING = -999999.0;

    // defaults
    @DynamicSerializeElement
    private String hucLevel;

    @DynamicSerializeElement
    private Map<Long, FFMPBasin> basins = new HashMap<Long, FFMPBasin>();

    /**
     * Pending load tasks that need to be run to fully populate basins
     */
    private final List<LoadTask> tasks = new ArrayList<LoadTask>();

    /**
     * Cache of basins in order for easy population from Load Tasks.
     */
    private final Map<String, FFMPBasin[]> orderedBasinsCache = new HashMap<String, FFMPBasin[]>();

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
     * Gets a list of pfaf ids from the keys
     * 
     * @return
     */
    public List<Long> getPfafIds() {
        return new ArrayList<Long>(getBasins().keySet());
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
            FFMPBasin basin = getBasins().get(pfaf);
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
            FFMPBasin basin = getBasins().get(pfaf);
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
            FFMPBasin basin = getBasins().get(pfaf);
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
            FFMPBasin basin = getBasins().get(pfaf);
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
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);
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
     * @param date
     * @param expiration
     * @return
     */
    public float getAverageMaxValue(ArrayList<Long> pfaf_ids, Date afterDate,
            Date beforeDate) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);
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
    public float getMaxValue(List<Long> pfaf_ids, Date date) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);
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
    public float getAverageGuidanceValue(List<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, float guidance,
            List<Long> forcedPfafs, long expiration) {

        float tvalue = 0.0f;
        float value;
        int i = 0;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);

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
    public float getMaxGuidanceValue(List<Long> pfaf_ids,
            FFMPGuidanceInterpolation interpolation, long expiration,
            long parentPfaf) {
        float tvalue = Float.NaN;
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);
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
    public float getAccumMaxValue(List<Long> pfaf_ids, Date beforeDate,
            Date afterDate, long expirationTime, boolean rate) {

        float tvalue = 0.0f;
        for (Long pfaf : pfaf_ids) {

            FFMPBasin basin = getBasins().get(pfaf);
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
        List<Float> values = new ArrayList<Float>();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);
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
    public List<Float> getAccumValues(List<Long> pfaf_ids, Date beforeDate,
            Date afterDate, long expirationTime, boolean rate) {
        List<Float> values = new ArrayList<Float>();
        for (Long pfaf : pfaf_ids) {
            FFMPBasin basin = getBasins().get(pfaf);
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
        for (FFMPBasin basin : getBasins().values()) {
            basin.purgeData(date);
        }
    }

    /**
     * deserialize data from the aggregate record
     * 
     * @param times
     */
    public void populate(List<Long> times) {
        long[] timesArr = new long[times.size()];
        for (int i = 0; i < timesArr.length; i += 1) {
            timesArr[i] = times.get(i);
        }
        for (FFMPBasin basin : getBasins().values()) {
            basin.deserialize(timesArr);
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
        boolean guidance = source.getSourceType().equals(
                SOURCE_TYPE.GUIDANCE.getSourceType());
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
                            basin = new FFMPBasin(pfaf, aggregate);
                        }
                        this.basins.put(pfaf, basin);
                    }
                    basins[j++] = basin;
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
    public void addVirtualBasins(File datastoreFile, String uri,
            String dataKey, String cwa, Date date,
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
                    basins[j++] = basin;
                }
                this.orderedBasinsCache.put(basinsKey, basins);
            }
            tasks.add(new LoadVirtualMapTask(datastoreFile, datasetGroupPath,
                    basins, date));
        }
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
            for (int j = 0; j < values.length; j += 1) {
                applyValue(basins[j], values[j]);
            }
        }

        /**
         * Apply the value to the basin/
         */
        protected void applyValue(FFMPBasin basin, float value) {
            if (basin.contains(date)) {
                float curval = basin.getValue(date);
                if (curval >= 0.0f && value >= 0.0f) {
                    basin.setValue(date, (curval + value) / 2);
                } else if (value >= 0.0f) {
                    basin.setValue(date, value);
                } // do not overwrite original value
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

            if (curval != FFMPUtils.MISSING || !curval.isNaN()) {

                if (curval >= 0.0f && value >= 0.0f) {
                    gBasin.setValue(sourceName, date, (curval + value) / 2);
                } else if (value >= 0.0f) {
                    gBasin.setValue(sourceName, date, value);
                }
                // do not overwrite original value
            } else {
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
