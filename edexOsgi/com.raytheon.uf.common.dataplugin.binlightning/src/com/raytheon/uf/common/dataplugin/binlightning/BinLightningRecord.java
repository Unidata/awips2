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
package com.raytheon.uf.common.dataplugin.binlightning;

import java.io.FileNotFoundException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningPulsePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Record implementation for Binary Lightning plugin.
 *
 * <pre>
 *
 *  SOFTWARE HISTORY
 *
 *  Date        Ticket#     Engineer    Description
 *  ----------  ----------  ----------- --------------------------
 *  Aug 10, 2007 379        jkorman     Initial Coding from prototype.
 *  Aug 17, 2007 379        jkorman     Fixed convert(). Was writing day into
 *                                      hours field of new calendar.
 *  Sep 20, 2007 379        jkorman     Modified getPersistenceTime.
 *  Sep 21, 2007 379        jkorman     Modified get/set start/stop times due
 *                                      to JiBX problems.
 *  Sep 24, 2007 379        jkorman     Removed Group, added insert_time and
 *                                      (set/get)ers to make DataURI work.
 *  Nov 29, 2007 472        jkorman     Added IDecoderGettable interface.
 *  Jan 07, 2008 720        jkorman     remove default assignments from
 *                                      attributes.
 *  Jul 08, 2008 1174       jkorman     Added persistenceTime handling.
 *  Feb 06, 2009 1990       bphillip    Removed populateDataStore method
 *  Feb 27, 2013 DCS 152    jgerth/elau Support for WWLLN and multiple sources
 *  Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                      forecastTime
 *  Apr 08, 2013 1293       bkowal      Removed references to hdffileid.
 *  Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 *  May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 *  Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 *  Oct 22, 2013 2361       njensen     Removed XML annotations
 *  Jan 21, 2014 2667       bclement    renamed record's lightSource field to source
 *  May 14, 2014 2536       bclement    removed TimeTools usage
 *  Jun 05, 2014 3226       bclement    moved data arrays into map for easier management
 *                                      replaced addStrike() with List constructor, added pulses
 *  Jun 10, 2014 3226       bclement    collections instead of lists, made data source logic public
 *  Jun 19, 2014 3214       bclement    populated pulse index array with -1 when missing pulse data
 *  Jan 22, 2014 3949       nabowle     refactor out default and unknown source constants.
 *
 * </pre>
 *
 * @author jkorman
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "binlightningseq")
@Table(name = BinLightningRecord.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = BinLightningRecord.PLUGIN_NAME, indexes = { @Index(name = "binlightning_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class BinLightningRecord extends PersistablePluginDataObject implements
        IPersistable {

    /** Serializable id * */
    private static final long serialVersionUID = 1L;

    public static final String PLUGIN_NAME = "binlightning";

    public static final int MISSING_PULSE_INDEX_VALUE = -1;

    // Data store data items
    @Transient
    private final Map<String, Object> strikeDataArrays = new TreeMap<String, Object>();

    @Transient
    private final Map<String, Object> pulseDataArrays = new TreeMap<String, Object>();

    // Persisted value - Earliest strike time in the collection.
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    private Calendar startTime;

    // Persisted value - Latest strike time in the collection.
    @DataURI(position = 2)
    @Column
    @DynamicSerializeElement
    private Calendar stopTime;

    // JJG - source of lightning data
    @Column(length = 5)
    @DataURI(position = 3)
    @DynamicSerializeElement
    private String source;

    // Used to track
    @Transient
    long persistTime = 0L;

    /**
     * Required empty constructor.
     */
    public BinLightningRecord() {
    }

    /**
     * Constructs a grib record from a dataURI
     *
     * @param uri
     *            The dataURI
     */
    public BinLightningRecord(String uri) {
        super(uri);
    }

    /**
     * Construct a lightning record from a list of strikes
     *
     * @param strikes
     */
    public BinLightningRecord(final Collection<LightningStrikePoint> strikes) {
        final int arraysSize = strikes.size();
        long[] obsTimes = new long[arraysSize];
        float[] latitudes = new float[arraysSize];
        float[] longitudes = new float[arraysSize];
        int[] intensities = new int[arraysSize];
        byte[] msgTypes = new byte[arraysSize];
        byte[] strikeTypes = new byte[arraysSize];
        byte[] pulseCounts = new byte[arraysSize];
        int[] pulseIndexes = new int[arraysSize];
        int[] heights = new int[arraysSize];
        int[] sensorCounts = new int[arraysSize];

        strikeDataArrays.put(LightningConstants.TIME_DATASET, obsTimes);
        strikeDataArrays.put(LightningConstants.LAT_DATASET, latitudes);
        strikeDataArrays.put(LightningConstants.LON_DATASET, longitudes);
        strikeDataArrays.put(LightningConstants.INTENSITY_DATASET, intensities);
        strikeDataArrays.put(LightningConstants.MSG_TYPE_DATASET, msgTypes);
        strikeDataArrays.put(LightningConstants.STRIKE_TYPE_DATASET,
                strikeTypes);
        strikeDataArrays
                .put(LightningConstants.PULSE_COUNT_DATSET, pulseCounts);
        strikeDataArrays.put(LightningConstants.PULSE_INDEX_DATASET,
                pulseIndexes);
        strikeDataArrays.put(LightningConstants.HEIGHT_DATASET, heights);
        strikeDataArrays.put(LightningConstants.SENSOR_COUNT_DATASET,
                sensorCounts);

        if (arraysSize > 0) {
            LightningStrikePoint sample = strikes.iterator().next();
            this.source = getDataSource(sample);
        }

        long startTimeMillis = Long.MAX_VALUE;
        long stopTimeMillis = Long.MIN_VALUE;

        int pulseDataCount = 0;

        final Iterator<LightningStrikePoint> iter = strikes.iterator();
        for (int i = 0; i < arraysSize; ++i) {
            LightningStrikePoint strike = iter.next();
            Calendar c = strike.getTime();

            long obsTimeMillis = c.getTimeInMillis();

            startTimeMillis = Math.min(startTimeMillis, obsTimeMillis);
            stopTimeMillis = Math.max(stopTimeMillis, obsTimeMillis);

            obsTimes[i] = obsTimeMillis;
            latitudes[i] = (float) strike.getLatitude();
            longitudes[i] = (float) strike.getLongitude();

            intensities[i] = (int) Math.round(strike.getStrikeStrength());
            msgTypes[i] = (byte) strike.getMsgType().getId();
            strikeTypes[i] = (byte) strike.getType().getId();
            /* some types have pulse counts but no pulse data */
            pulseCounts[i] = (byte) strike.getPulseCount();
            heights[i] = (int) Math.round(strike.getElevation());
            sensorCounts[i] = strike.getSensorCount();

            List<LightningPulsePoint> pulses = strike.getPulses();
            if (pulses != null && !pulses.isEmpty()) {
                pulseIndexes[i] = pulseDataCount;
                pulseDataCount += pulses.size();
                if (pulseCounts[i] != pulses.size()) {
                    pulseCounts[i] = (byte) pulses.size();
                }
            } else {
                pulseIndexes[i] = MISSING_PULSE_INDEX_VALUE;
            }
        }

        if (pulseDataCount > 0) {
            /* at least one of the strikes had pulse data */
            setPulseData(strikes, pulseDataCount);
        }
        startTime = TimeUtil.newGmtCalendar(new Date(startTimeMillis));
        stopTime = TimeUtil.newGmtCalendar(new Date(stopTimeMillis));

        TimeRange range = new TimeRange(startTime, stopTime);
        setDataTime(new DataTime(startTime, range));
    }

    /**
     * @param strikes
     * @param pulseDataCount
     *            total number of pulses for all strikes
     */
    private void setPulseData(final Collection<LightningStrikePoint> strikes,
            final int pulseDataCount) {
        long[] pulseTimes = new long[pulseDataCount];
        float[] pulseLats = new float[pulseDataCount];
        float[] pulseLons = new float[pulseDataCount];
        int[] pulseIntensities = new int[pulseDataCount];
        byte[] pulseTypes = new byte[pulseDataCount];
        int[] pulseHeights = new int[pulseDataCount];
        int[] pulseSensorCounts = new int[pulseDataCount];

        pulseDataArrays.put(LightningConstants.TIME_DATASET, pulseTimes);
        pulseDataArrays.put(LightningConstants.LAT_DATASET, pulseLats);
        pulseDataArrays.put(LightningConstants.LON_DATASET, pulseLons);
        pulseDataArrays.put(LightningConstants.INTENSITY_DATASET,
                pulseIntensities);
        pulseDataArrays.put(LightningConstants.PULSE_TYPE_DATASET, pulseTypes);
        pulseDataArrays.put(LightningConstants.HEIGHT_DATASET, pulseHeights);
        pulseDataArrays.put(LightningConstants.SENSOR_COUNT_DATASET,
                pulseSensorCounts);

        int index = 0;
        for (LightningStrikePoint strike : strikes) {
            List<LightningPulsePoint> pulses = strike.getPulses();
            if (pulses != null && !pulses.isEmpty()) {
                for (LightningPulsePoint pulse : pulses) {
                    pulseTimes[index] = pulse.getTime().getTimeInMillis();
                    pulseLats[index] = (float) pulse.getLatitude();
                    pulseLons[index] = (float) pulse.getLongitude();
                    pulseIntensities[index] = (int) Math.round(pulse
                            .getStrikeStrength());
                    pulseTypes[index] = pulse.getType().getId();
                    pulseHeights[index] = (int) Math
                            .round(pulse.getElevation());
                    pulseSensorCounts[index] = pulse.getSensorCount();
                    ++index;
                }
            }
        }
    }

    /**
     * Extract data source from strike
     *
     * @param strike
     * @return
     */
    public static String getDataSource(LightningStrikePoint strike) {
        if (strike.getLightSource() == null) {
            return LightningConstants.DEFAULT_SOURCE;
        } else if (strike.getLightSource().isEmpty()) {
            return LightningConstants.UNKNOWN_SOURCE;
        } else {
            return strike.getLightSource();
        }
    }

    /**
     * Get the data start time time. This is the date/time of the oldest item.
     *
     * @return the startTime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * Get the data start time time. This is the date/time of the oldest item.
     *
     * @param startTime
     *            the startTime to set
     */
    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    /**
     * Get the data stop time time. This is the date/time of the newest item.
     *
     * @return the stopTime
     */
    public Calendar getStopTime() {
        return stopTime;
    }

    /**
     * Set the data stop time time. This is the date/time of the newest item.
     *
     * @param stopTime
     *            the stopTime to set
     */
    public void setStopTime(Calendar stopTime) {
        this.stopTime = stopTime;
    }

    /**
     * Get the data insert time.
     *
     * @return the insert_time
     */
    public Calendar getInsert_time() {
        return getInsertTime();
    }

    /**
     * Set the data insert time.
     *
     * @param insert_time
     *            the insert_time to set
     */
    public void setInsert_time(Calendar insert_time) {
        setInsertTime(insert_time);
    }

    /**
     * gets the obsTimes
     *
     * @return
     */
    public long[] getObsTimes() {
        Object obj = strikeDataArrays.get(LightningConstants.TIME_DATASET);
        return obj != null ? (long[]) obj : new long[0];
    }

    /**
     * Gets the latitudes
     *
     * @return
     */
    public float[] getLatitudes() {
        Object obj = strikeDataArrays.get(LightningConstants.LAT_DATASET);
        return obj != null ? (float[]) obj : new float[0];
    }

    /**
     * Gets the latitudes
     *
     * @return
     */
    public float[] getLongitudes() {
        Object obj = strikeDataArrays.get(LightningConstants.LON_DATASET);
        return obj != null ? (float[]) obj : new float[0];
    }

    /**
     * Gets the instensities
     *
     * @return
     */
    public int[] getIntensities() {
        Object obj = strikeDataArrays.get(LightningConstants.INTENSITY_DATASET);
        return obj != null ? (int[]) obj : new int[0];
    }

    /**
     * Gets the msgTypes
     *
     * @return
     */
    public byte[] getMsgTypes() {
        Object obj = strikeDataArrays.get(LightningConstants.MSG_TYPE_DATASET);
        return obj != null ? (byte[]) obj : new byte[0];
    }

    /**
     * Gets the strikeTypes
     *
     * @return
     */
    public byte[] getStrikeTypes() {
        Object obj = strikeDataArrays
                .get(LightningConstants.STRIKE_TYPE_DATASET);
        return obj != null ? (byte[]) obj : new byte[0];
    }

    /**
     * Gets the strikeCounts
     *
     * @return
     */
    public byte[] getPulseCounts() {
        Object obj = strikeDataArrays
                .get(LightningConstants.PULSE_COUNT_DATSET);
        return obj != null ? (byte[]) obj : new byte[0];
    }

    /**
     * JJG - Get the lightning source
     *
     * @return
     */
    public String getSource() {
        return source;
    }

    /**
     * JJG - Set the lightning source
     *
     * @param lightSource
     */
    public void setSource(String lightSource) {
        this.source = lightSource;
    }

    /**
     * Sets the data arrays from the store.
     *
     * @param dataStore
     */
    public void retrieveFromDataStore(IDataStore dataStore)
            throws StorageException {
        retrieveFromDataStore(dataStore, false);
    }

    /**
     * Sets the data arrays from the store.
     *
     * @param dataStore
     * @param includePulses
     *            extract pulse data if true
     */
    public void retrieveFromDataStore(IDataStore dataStore,
            boolean includePulses) throws StorageException {
        try {
            IDataRecord[] dataRecs = dataStore.retrieve(getDataURI());
            for (IDataRecord record : dataRecs) {
                strikeDataArrays.put(record.getName(), record.getDataObject());
            }
            if (includePulses) {
                String pulseGroup = getDataURI() + DataURI.SEPARATOR
                        + LightningConstants.PULSE_HDF5_GROUP_SUFFIX;
                try {
                    IDataRecord[] pulseRecords = dataStore.retrieve(pulseGroup);
                    for (IDataRecord record : pulseRecords) {
                        pulseDataArrays.put(record.getName(),
                                record.getDataObject());
                    }
                } catch (Exception e) {
                    /* FIXME better way to find out if group doesn't exist */
                }
            }
        } catch (FileNotFoundException e) {
            throw new StorageException(e.getLocalizedMessage(), null, e);
        }
    }

    /**
     * @return the strikeDataArrays
     */
    public Map<String, Object> getStrikeDataArrays() {
        return strikeDataArrays;
    }

    /**
     * @return the pulseDataArrays
     */
    public Map<String, Object> getPulseDataArrays() {
        return pulseDataArrays;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }
}
