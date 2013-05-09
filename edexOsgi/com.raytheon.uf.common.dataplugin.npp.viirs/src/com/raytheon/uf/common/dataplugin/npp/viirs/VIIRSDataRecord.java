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
package com.raytheon.uf.common.dataplugin.npp.viirs;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * VIIRS Data record object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            mschenke     Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "viirsseq")
@Table(name = "viirs", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "viirs",
		indexes = {
				@Index(name = "viirs_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@DynamicSerialize
public class VIIRSDataRecord extends PersistablePluginDataObject implements
        ISpatialEnabled {

    public static final String MISSING_VALUE_ID = "missing_value";

    public static final String SCALE_ID = "scale_factor";

    public static final String OFFSET_ID = "add_offset";

    public static final String UNIT_ID = "unit";

    private static final long serialVersionUID = 4920123282595760202L;

    @Column(length = 30)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String region = "Unknown Region";

    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    private String channelType;

    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    private int levels;

    @Column
    @DataURI(position = 4)
    @DynamicSerializeElement
    private Integer channel;

    @Column
    @DataURI(position = 5)
    @DynamicSerializeElement
    private Double wavelength;

    @Column
    @DataURI(position = 6)
    @DynamicSerializeElement
    private String parameter;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    private VIIRSSpatialCoverage coverage;

    public VIIRSDataRecord() {
        setPluginName(VIIRSDataRecord.class.getAnnotation(Table.class).name());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.geospatial.ISpatialEnabled#getSpatialObject()
     */
    @Override
    public ISpatialObject getSpatialObject() {
        return coverage;
    }

    /**
     * @return the coverage
     */
    public VIIRSSpatialCoverage getCoverage() {
        return coverage;
    }

    /**
     * @param coverage
     *            the coverage to set
     */
    public void setCoverage(VIIRSSpatialCoverage coverage) {
        this.coverage = coverage;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the channel
     */
    public Integer getChannel() {
        return channel;
    }

    /**
     * @param channel
     *            the channel to set
     */
    public void setChannel(Integer channel) {
        this.channel = channel;
    }

    /**
     * @return the wavelength
     */
    public Double getWavelength() {
        return wavelength;
    }

    /**
     * @param wavelength
     *            the wavelength to set
     */
    public void setWavelength(Double wavelength) {
        this.wavelength = wavelength;
    }

    /**
     * @return
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#getLevels()
     */
    public int getLevels() {
        return levels;
    }

    /**
     * @param levels
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#setLevels(int)
     */
    public void setLevels(int levels) {
        this.levels = levels;
    }

    /**
     * @return the region
     */
    public String getRegion() {
        return region;
    }

    /**
     * @param region
     *            the region to set
     */
    public void setRegion(String region) {
        this.region = region;
    }

    /**
     * @return the channelType
     */
    public String getChannelType() {
        return channelType;
    }

    /**
     * @param channelType
     *            the channelType to set
     */
    public void setChannelType(String channelType) {
        this.channelType = channelType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.persist.IPersistable#getPersistenceTime
     * ()
     */
    @Override
    public Date getPersistenceTime() {
        Calendar c = getInsertTime();
        if (c == null)
            return null;

        return c.getTime();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.persist.IPersistable#setPersistenceTime
     * (java.util.Date)
     */
    @Override
    public void setPersistenceTime(Date persistTime) {
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTime(persistTime);
        setInsertTime(c);
    }

    /**
     * Get the name of the dataset for the level
     * 
     * @param level
     * @return
     */
    public static String getDataSet(int level) {
        return "Data-" + level;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.PluginDataObject#getDecoderGettable()
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }
    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
