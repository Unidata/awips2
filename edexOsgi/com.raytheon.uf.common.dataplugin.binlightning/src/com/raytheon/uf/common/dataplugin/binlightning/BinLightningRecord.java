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

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Record implementation for Binary Lightning plugin.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date        Ticket#     Engineer    Description
 *  ----------  ----------  ----------- --------------------------
 * 20070810            379  jkorman     Initial Coding from prototype.
 * 20070817            379  jkorman     Fixed convert(). Was writing day into 
 *                                      hours field of new calendar.
 * 20070920            379  jkorman     Modified getPersistenceTime.
 * 20070921            379  jkorman     Modified get/set start/stop times due
 *                                      to JiBX problems.
 * 20070924            379  jkorman     Removed Group, added insert_time and
 *                                      (set/get)ers to make DataURI work.                                                                           
 * 20071129            472  jkorman     Added IDecoderGettable interface.    
 * 20080107            720  jkorman     remove default assignments from attributes.
 * 20080708           1174  jkorman     Added persistenceTime handling.
 * 20090206           1990  bphillip    Removed populateDataStore method
 * 20130227        DCS 152  jgerth/elau Support for WWLLN and multiple sources
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 20130408           1293  bkowal      Removed references to hdffileid.
 * Apr 12, 2013       1857  bgonzale    Added SequenceGenerator annotation.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "binlightningseq")
@Table(name = "binlightning", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "binlightning",
		indexes = {
				@Index(name = "binlightning_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class BinLightningRecord extends
        PersistablePluginDataObject implements IPersistable {

    /** Serializable id * */
    private static final long serialVersionUID = 1L;

    @Transient
    private long[] obsTimes = null;

    @Transient
    private float[] latitudes = null;

    @Transient
    private float[] longitudes = null;

    @Transient
    private int[] intensities = null;

    @Transient
    private byte[] msgTypes = null;

    @Transient
    private byte[] strikeTypes = null;

    @Transient
    private byte[] strikeCounts = null;

    @Transient
    private Object[] dataArrays = null;

    // Data store data item names
    @Transient
    private final String[] dataNames = { "obsTime", "latitude", "longitude",
            "intensity", "msgType", "strikeType", "strikeCount", };

    @Transient
    private int insertIndex = 0;

    @Transient
    private long startTimeMillis = Long.MAX_VALUE;

    @Transient
    private long stopTimeMillis = Long.MIN_VALUE;

    // Persisted value - Earliest strike time in the collection.
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar startTime;

    // Persisted value - Latest strike time in the collection.
    @DataURI(position = 2)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar stopTime;
    
    // JJG - source of lightning data
    @Column(length = 5)
    @DataURI(position = 3)
    @XmlAttribute
    @DynamicSerializeElement
    private String lightSource;

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
     * @param tableDef
     *            The table definition associated with this class
     */
    public BinLightningRecord(String uri) {
        super(uri);
    }

    /**
     * Construct an empty lightning record.
     */
    public BinLightningRecord(int arraysSize) {
        obsTimes = new long[arraysSize];
        latitudes = new float[arraysSize];
        longitudes = new float[arraysSize];

        intensities = new int[arraysSize];
        msgTypes = new byte[arraysSize];
        strikeTypes = new byte[arraysSize];
        strikeCounts = new byte[arraysSize];
        dataArrays = new Object[] { obsTimes, latitudes, longitudes,
                intensities, msgTypes, strikeTypes, strikeCounts, };
        insertIndex = 0;
    }

    // /**
    // * Construct an empty lightning record.
    // *
    // * @param message
    // * Lightning data report.
    // */
    // public BinLightningRecord(String message) {
    // super(message);
    // }

    /**
     * Track the current persistence time for the data set.
     */
    @SuppressWarnings("unused")
    private void updatePersistenceTime() {
        if (startTimeMillis != Long.MAX_VALUE
                && stopTimeMillis != Long.MIN_VALUE) {
            persistTime = (startTimeMillis + stopTimeMillis) / 2;
            setPersistenceTime(TimeTools.newCalendar(persistTime).getTime());
        } else {
            setPersistenceTime(TimeTools.getSystemCalendar().getTime());
            persistTime = getInsertTime().getTimeInMillis();
        }
    }

    /**
     * Add a strike report to the record collection.
     * 
     * @param strike
     *            A strike report to add.
     */
    public void addStrike(LightningStrikePoint strike) {
		// jjg add
		if (lightSource == null) {
			if (strike.getLightSource() == null) {
				lightSource = (String) "NLDN";
			} else if (strike.getLightSource().isEmpty()) {
				lightSource = (String) "UNKN";
			} else {
				lightSource = (String) strike.getLightSource();
			}
		} else {
			if (strike.getLightSource() == null) {
				lightSource = (String) "NLDN";
			} else if (!lightSource.equals(strike.getLightSource()))
				lightSource = (String) "UNKN";
		}
		// end

    	if (insertIndex < obsTimes.length) {
            long t1 = startTimeMillis;

            Calendar c = TimeTools.getBaseCalendar(strike.getYear(),
                    strike.getMonth(), strike.getDay());

            c.set(Calendar.HOUR_OF_DAY, strike.getHour());
            c.set(Calendar.MINUTE, strike.getMinute());
            c.set(Calendar.SECOND, strike.getSecond());
            c.set(Calendar.MILLISECOND, strike.getMillis());

            long obsTimeMillis = c.getTimeInMillis();

            startTimeMillis = Math.min(startTimeMillis, obsTimeMillis);
            stopTimeMillis = Math.max(stopTimeMillis, obsTimeMillis);

            obsTimes[insertIndex] = obsTimeMillis;
            latitudes[insertIndex] = (float) strike.getLatitude();
            longitudes[insertIndex] = (float) strike.getLongitude();

            intensities[insertIndex] = Math.round((float) strike
                    .getStrikeStrength());
            msgTypes[insertIndex] = (byte) strike.getMsgType().ordinal();
            strikeTypes[insertIndex] = (byte) strike.getType().ordinal();
            strikeCounts[insertIndex] = (byte) strike.getStrikeCount();
            insertIndex++;
            // only update the times if they have changed!
            if (t1 != startTimeMillis) {
                startTime = TimeTools.newCalendar(startTimeMillis);
            }
            if (t1 != stopTimeMillis) {
                stopTime = TimeTools.newCalendar(stopTimeMillis);
            }

            // updatePersistenceTime();
        } else {
            throw new ArrayIndexOutOfBoundsException(String.format(
                    "index greater than length [%d]", insertIndex));
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

    public String[] getDataNames() {
        return dataNames;
    }

    public Object[] getDataArrays() {
        return dataArrays;
    }

    public void setDataArrays(Object[] dataArrays) {
        this.dataArrays = dataArrays;
    }

    /**
     * gets the obsTimes
     * 
     * @return
     */
    public long[] getObsTimes() {
        return obsTimes;
    }

    /**
     * Gets the latitudes
     * 
     * @return
     */
    public float[] getLatitudes() {
        return latitudes;
    }

    /**
     * Gets the latitudes
     * 
     * @return
     */
    public float[] getLongitudes() {
        return longitudes;
    }

    /**
     * Gets the instensities
     * 
     * @return
     */
    public int[] getIntensities() {
        return intensities;
    }

    /**
     * Gets the msgTypes
     * 
     * @return
     */
    public byte[] getMsgTypes() {
        return msgTypes;
    }

    /**
     * Gets the strikeTypes
     * 
     * @return
     */
    public byte[] getStrikeTypes() {
        return strikeTypes;
    }

    /**
     * Gets the strikeCounts
     * 
     * @return
     */
    public byte[] getStrikeCounts() {
        return strikeCounts;
    }

    /**
     * JJG - Get the lightning source
     * 
     * @return
     */
    public String getLightSource() {
        return lightSource;
    }

    /**
     * JJG - Set the lightning source
     * 
     * @param lightSource
     */
    public void setLightSource(String lightSource) {
        this.lightSource = lightSource;
    }
    
    /**
     * Get the IDecoderGettable reference for this record.
     * 
     * @return The IDecoderGettable reference for this record. Null for this
     *         class.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    /**
     * Sets the data arrays from the store.
     * 
     * @param dataStore
     */
    public void retrieveFromDataStore(IDataStore dataStore)
            throws StorageException {

        try {
            IDataRecord[] dataRec = dataStore.retrieve(getDataURI());
            dataArrays = new Object[dataRec.length];
            for (int i = 0; i < dataRec.length; i++) {
                if (dataRec[i].getName().equals("obsTime")) {
                    obsTimes = ((LongDataRecord) dataRec[i]).getLongData();
                    dataArrays[i] = obsTimes;
                }
                if (dataRec[i].getName().equals("latitude")) {
                    latitudes = ((FloatDataRecord) dataRec[i]).getFloatData();
                    dataArrays[i] = latitudes;
                } else if (dataRec[i].getName().equals("longitude")) {
                    longitudes = ((FloatDataRecord) dataRec[i]).getFloatData();
                    dataArrays[i] = longitudes;
                } else if (dataRec[i].getName().equals("intensity")) {
                    intensities = ((IntegerDataRecord) dataRec[i]).getIntData();
                    dataArrays[i] = intensities;
                } else if (dataRec[i].getName().equals("msgType")) {
                    msgTypes = ((ByteDataRecord) dataRec[i]).getByteData();
                    dataArrays[i] = msgTypes;
                } else if (dataRec[i].getName().equals("strikeType")) {
                    strikeTypes = ((ByteDataRecord) dataRec[i]).getByteData();
                    dataArrays[i] = strikeTypes;
                } else if (dataRec[i].getName().equals("strikeCount")) {
                    strikeCounts = ((ByteDataRecord) dataRec[i]).getByteData();
                    dataArrays[i] = strikeCounts;
                }
            }
            setDataArrays(dataArrays);

        } catch (Exception se) {
            se.printStackTrace();
        }
    }

}
