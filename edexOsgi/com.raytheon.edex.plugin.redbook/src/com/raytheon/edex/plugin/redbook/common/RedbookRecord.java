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
package com.raytheon.edex.plugin.redbook.common;

import java.util.Calendar;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * The RedbookRecord class holds the data to be stored to the database. In
 * addition it also implements the IPersistable methods to enable storage of the
 * binary Redbook data to the HDF5 repository. Note that due to the nature of
 * Redbook data, there is no geospatial information associated with a Redbook
 * data record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080512           1131 jkorman     Initial implementation.
 * 20080529           1131 jkorman     getPersistenceTime now returns system time.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 20130408           1293 bkowal      Removed references to hdffileid.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@Table(name = "redbook", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "redbook",
		indexes = {
				@Index(name = "redbook_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RedbookRecord extends PersistablePluginDataObject
        implements IPersistable, Cloneable {

    private static final long serialVersionUID = 1L;

    public static final String REDBOOK_DATA = "redbookData";

    // Time of the observation.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar timeObs;

    @DataURI(position = 1)
    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private String wmoTTAAii;

    // Text of the WMO header
    @Column(length = 16)
    @XmlAttribute
    @DynamicSerializeElement
    private String wmoCCCCdt;

    // Correction indicator from wmo header
    @DataURI(position = 3)
    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private String corIndicator;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer retentionHours;

    @DataURI(position = 5)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer fcstHours;

    // varchar(15)
    @DataURI(position = 2)
    @Column(length = 15)
    @XmlAttribute
    @DynamicSerializeElement
    private String productId;

    @DataURI(position = 6)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer fileId;

    // varchar(4)
    @DataURI(position = 4)
    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String originatorId;

    @Transient
    private byte[] redBookData;

    /**
     * 
     */
    public RedbookRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     * @param tableDef
     *            The table definitions for this class.
     */
    public RedbookRecord(String uri) {
        super(uri);
    }

    /**
     * @return the ttAAII
     */
    public String getWmoTTAAii() {
        return wmoTTAAii;
    }

    /**
     * @param ttAAII
     *            the ttAAII to set
     */
    public void setWmoTTAAii(String ttAAII) {
        wmoTTAAii = ttAAII;
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoCCCCdt() {
        return wmoCCCCdt;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoCCCCdt(String wmoCCCCdt) {
        this.wmoCCCCdt = wmoCCCCdt;
    }

    /**
     * Get the report correction indicator.
     * 
     * @return The corIndicator
     */
    public String getCorIndicator() {
        return corIndicator;
    }

    /**
     * Set the report correction indicator.
     * 
     * @param corIndicator
     *            The corIndicator.
     */
    public void setCorIndicator(String corIndicator) {
        this.corIndicator = corIndicator;
    }

    /**
     * @return the timeObs
     */
    public Calendar getTimeObs() {
        return timeObs;
    }

    /**
     * @param timeObs
     *            the timeObs to set
     */
    public void setTimeObs(Calendar timeObs) {
        this.timeObs = timeObs;
    }

    @Override
    public void setDataURI(String dataURI) {
        super.setDataURI(dataURI);
        identifier = dataURI;
    }

    /**
     * @return the retentionHours
     */
    public Integer getRetentionHours() {
        return retentionHours;
    }

    /**
     * @param retentionHours
     *            the retentionHours to set
     */
    public void setRetentionHours(Integer retentionHours) {
        this.retentionHours = retentionHours;
    }

    /**
     * @return the fileId
     */
    public Integer getFileId() {
        return fileId;
    }

    /**
     * @param fileId
     *            the fileId to set
     */
    public void setFileId(Integer fileId) {
        this.fileId = fileId;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * @return the fcstHours
     */
    public Integer getFcstHours() {
        return fcstHours;
    }

    /**
     * @param fcstHours
     *            the fcstHours to set
     */
    public void setFcstHours(Integer fcstHours) {
        this.fcstHours = fcstHours;
    }

    /**
     * @return the originatorId
     */
    public String getOriginatorId() {
        return originatorId;
    }

    /**
     * @param originatorId
     *            the originatorId to set
     */
    public void setOriginatorId(String originatorId) {
        this.originatorId = originatorId;
    }

    /**
     * @return the redBookData
     */
    public byte[] getRedBookData() {
        return redBookData;
    }

    /**
     * @param redBookData
     *            the redBookData to set
     */
    public void setRedBookData(byte[] redBookData) {
        this.redBookData = redBookData;
    }

    /**
     * Get the IDecoderGettable reference for this record.
     * 
     * @return This class does not implement IDecoderGettable, return null.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    /**
     * @return a mostly shallow copy of the RedbookRecord with the reference
     *         time set back one minute. Clears the id and dataURI fields of the
     *         copy.
     */
    public RedbookRecord createBackdatedVersion() throws PluginException {
        RedbookRecord other;
        try {
            other = (RedbookRecord) clone();
        } catch (CloneNotSupportedException e) {
            throw new UnsupportedOperationException(e);
        }
        other.id = 0;
        other.dataURI = null;

        Date newRefTime = new Date(dataTime.getRefTime().getTime() - 60 * 1000);
        if (dataTime.getUtilityFlags().contains(DataTime.FLAG.FCST_USED)) {
            other.dataTime = new DataTime(newRefTime, dataTime.getFcstTime());
        } else {
            other.dataTime = new DataTime(newRefTime);
        }

        return other;
    }
}