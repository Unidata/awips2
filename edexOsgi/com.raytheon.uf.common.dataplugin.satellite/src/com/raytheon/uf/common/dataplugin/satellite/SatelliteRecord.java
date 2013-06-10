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

package com.raytheon.uf.common.dataplugin.satellite;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for satellite plugin.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02/14/07     139         bphillip    Initial Creation    
 * 20070914            379  jkorman     Added populateDataStore() and
 *                                      getPersistenceTime() from new IPersistable
 * 20071129            472  jkorman     Added IDecoderGettable interface.
 * 20081106           1515  jkorman     Changed units length from 16 to 26
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * - AWIPS2 Baseline Repository --------
 * 07/30/2012    798        jkorman     Support for common satellite data.
 * 03/25/2013   1823        dgilling    Replace underscores with spaces in URI
 *                                      constructor.
 * 04/08/2013   1293        bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "satelliteseq")
@Table(name = "satellite", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "satellite",
		indexes = {
				@Index(name = "satellite_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SatelliteRecord extends PersistablePluginDataObject
        implements ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    /**
     * The default dataset name to use for persisted satellite data.
     */
    public static final String SAT_DATASET_NAME = DataStoreFactory.DEF_DATASET_NAME;

    /**
     * The attribute name for a value that will be used to "fill" undefined
     * data.
     */
    public static final String SAT_FILL_VALUE = "_FillValue";

    /**
     * The attribute name for the data additive offset value.
     */
    public static final String SAT_ADD_OFFSET = "add_offset";

    /**
     * The attribute name for the data scale factor value..
     */
    public static final String SAT_SCALE_FACTOR = "scale_factor";

    /**
     * The source of the data - NESDIS
     */
    @Column(length = 31)
    @DataURI(position = 1)
    @XmlAttribute
    @DynamicSerializeElement
    private String source;

    /** The creating entity. See table 4.5 of GINI satellite ICD */
    @Column(length = 63)
    @DataURI(position = 2)
    @XmlAttribute
    @DynamicSerializeElement
    private String creatingEntity;

    /** The sector ID. See table 4.6 of the GINI satellite ICD */
    @Column(length = 63)
    @DataURI(position = 3)
    @XmlAttribute
    @DynamicSerializeElement
    private String sectorID;

    /** The physical Element. See table 4.7 of the GINI satellite ICD */
    @Column(length = 63)
    @DataURI(position = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String physicalElement;

    /**
     * Number of logical records in the product. See tables 4.9, 4.11, 4.12,
     * 4.13, 4.14, 4.16 of the GINI satellite ICD
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer numRecords;

    /**
     * Size of logical records in bytes for product. See tables 4.9, 4.11, 4.12,
     * 4.13, 4.14, 4.16 of the GINI satellite ICD
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer sizeRecords;

    /** The latitude directly beneath the satellite */
    @Column
    @DynamicSerializeElement
    private Float satSubPointLat;

    /** The longitude directly beneath the satellite */
    @Column
    @DynamicSerializeElement
    private Float satSubPointLon;

    /** The upper right hand latitude */
    @Column
    @DynamicSerializeElement
    private Float upperRightLat;

    /** The upper right hand longitude */
    @Column
    @DynamicSerializeElement
    private Float upperRightLon;

    /** Height of the satellite in km */
    @Column
    @DynamicSerializeElement
    private Integer satHeight;

    /** Units of the satellite data * */
    @Column(length = 26)
    @XmlAttribute
    @DynamicSerializeElement
    private String units;

    /** Number of interpolation levels in the data store */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer interpolationLevels;

    @DataURI(position = 5, embedded = true)
    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    private SatMapCoverage coverage;

    @Override
    public SatMapCoverage getSpatialObject() {
        return coverage;
    }

    public SatMapCoverage getCoverage() {
        return coverage;
    }

    public void setCoverage(SatMapCoverage coverage) {
        this.coverage = coverage;
    }

    public Float getSatSubPointLat() {
        return satSubPointLat;
    }

    public void setSatSubPointLat(Float satSubPointLat) {
        this.satSubPointLat = satSubPointLat;
    }

    public Float getSatSubPointLon() {
        return satSubPointLon;
    }

    public void setSatSubPointLon(Float satSubPointLon) {
        this.satSubPointLon = satSubPointLon;
    }

    public Float getUpperRightLat() {
        return upperRightLat;
    }

    public void setUpperRightLat(Float upperRightLat) {
        this.upperRightLat = upperRightLat;
    }

    public Float getUpperRightLon() {
        return upperRightLon;
    }

    public void setUpperRightLon(Float upperRightLon) {
        this.upperRightLon = upperRightLon;
    }

    /**
     * No-arg constructor.
     */
    public SatelliteRecord() {

    }

    /**
     * Constructs a satellite record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public SatelliteRecord(String uri) {
        super(uri.replace('_', ' '));
    }

    public Integer getNumRecords() {
        return numRecords;
    }

    public void setNumRecords(Integer numRecords) {
        this.numRecords = numRecords;
    }

    public Integer getSizeRecords() {
        return sizeRecords;
    }

    public void setSizeRecords(Integer sizeRecords) {
        this.sizeRecords = sizeRecords;
    }

    public Integer getSatHeight() {
        return satHeight;
    }

    public void setSatHeight(Integer satHeight) {
        this.satHeight = satHeight;
    }

    /**
     * @return the units
     */
    public String getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(String units) {
        this.units = units;
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

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getCreatingEntity() {
        return creatingEntity;
    }

    public void setCreatingEntity(String creatingEntity) {
        this.creatingEntity = creatingEntity;
    }

    public String getSectorID() {
        return sectorID;
    }

    public void setSectorID(String sectorID) {
        this.sectorID = sectorID;
    }

    public String getPhysicalElement() {
        return physicalElement;
    }

    public void setPhysicalElement(String physicalElement) {
        this.physicalElement = physicalElement;
    }

    /**
     * Get the number of interpolation levels in the data store.
     * 
     * @return The number of interpolation levels. Data that is not interpolated
     *         should return a value of 0.
     */
    public Integer getInterpolationLevels() {
        return interpolationLevels;
    }

    /**
     * Set the number of interpolation levels in the data store. If the data are
     * not interpolated a value of 0 should be used.
     * 
     * @param levels
     *            The number of interpolation levels in the data. Any value less
     *            than zero is set to zero.
     */
    public void setInterpolationLevels(Integer levels) {
        if (!DataStoreFactory.isInterpolated(levels)) {
            levels = 0;
        }
        interpolationLevels = levels;
    }

    /**
     * Construct the data record from decoded data. Note this must not be called
     * until after the record datauri has been created!
     * 
     * @param record
     *            The SatelliteRecord containing a constructed dataURI, the
     *            binary satellite data, and geospatial coverage information.
     * @return The IDataRecord associated with this decoded data.
     */
    public static final IDataRecord getDataRecord(SatelliteRecord record) {
        IDataRecord dataRec = null;
        if (record != null) {
            SatMapCoverage coverage = record.getCoverage();
            byte[] image = (byte[]) record.getMessageData();
            if ((coverage != null) && (image != null)) {
                SatelliteMessageData messageData = new SatelliteMessageData(
                        image, coverage.getNx(), coverage.getNy());
                dataRec = messageData.getStorageRecord(record,
                        SatelliteRecord.SAT_DATASET_NAME);
            }
        }
        return dataRec;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
