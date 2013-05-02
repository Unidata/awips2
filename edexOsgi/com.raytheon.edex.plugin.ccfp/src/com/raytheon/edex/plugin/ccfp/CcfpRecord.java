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

package com.raytheon.edex.plugin.ccfp;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
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
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * CCFP Record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/03/2007   908         bwoodle     initial creation
 * 09/15/2009   3027       njensen      Use dates for times
 * 09/21/2009   3072        bsteffen    Removed times because they are stored in DataTime
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * 
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ccfpseq")
@Table(name = "ccfp", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "ccfp",
		indexes = {
				@Index(name = "ccfp_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CcfpRecord extends PluginDataObject implements ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    @DataURI(position = 1)
    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private String producttype;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer coverage;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer conf;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer growth;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer tops;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer speed;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer direction;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Boolean canadaflag;

    @DataURI(position = 2, embedded = true)
    @Embedded
    @XmlElement
    @DynamicSerializeElement
    private CcfpLocation location;

    /**
     * Default Constructor
     */
    public CcfpRecord() {
    }

    // /**
    // * Constructor.
    // *
    // * @param message
    // * The text of the message
    // */
    // public CcfpRecord(String message) {
    // super(message);
    // }

    /**
     * Constructs a ccfp record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public CcfpRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    public java.lang.String getProducttype() {
        return producttype;
    }

    public void setProducttype(java.lang.String producttype) {
        this.producttype = producttype;
    }

    public double getBoxLat() {
        return location.getBoxLat();
    }

    public double getBoxLong() {
        return location.getBoxLong();
    }

    public Boolean getCanadaflag() {
        return canadaflag;
    }

    public void setCanadaflag(Boolean canadaflag) {
        this.canadaflag = canadaflag;
    }

    /**
     * @return the coverage
     */
    public Integer getCoverage() {
        return coverage;
    }

    /**
     * @param coverage
     *            the coverage to set
     */
    public void setCoverage(Integer coverage) {
        this.coverage = coverage;
    }

    /**
     * @return the conf
     */
    public Integer getConf() {
        return conf;
    }

    /**
     * @param conf
     *            the conf to set
     */
    public void setConf(Integer conf) {
        this.conf = conf;
    }

    /**
     * @return the growth
     */
    public Integer getGrowth() {
        return growth;
    }

    /**
     * @param growth
     *            the growth to set
     */
    public void setGrowth(Integer growth) {
        this.growth = growth;
    }

    /**
     * @return the tops
     */
    public Integer getTops() {
        return tops;
    }

    /**
     * @param tops
     *            the tops to set
     */
    public void setTops(Integer tops) {
        this.tops = tops;
    }

    /**
     * @return the speed
     */
    public Integer getSpeed() {
        return speed;
    }

    /**
     * @param speed
     *            the speed to set
     */
    public void setSpeed(Integer speed) {
        this.speed = speed;
    }

    /**
     * @return the direction
     */
    public Integer getDirection() {
        return direction;
    }

    /**
     * @param direction
     *            the direction to set
     */
    public void setDirection(Integer direction) {
        this.direction = direction;
    }

    @Override
    public CcfpLocation getSpatialObject() {
        return location;
    }

    public CcfpLocation getLocation() {
        return location;
    }

    public void setLocation(CcfpLocation location) {
        this.location = location;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
