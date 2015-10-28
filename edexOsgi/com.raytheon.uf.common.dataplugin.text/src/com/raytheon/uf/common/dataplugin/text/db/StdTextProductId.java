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
package com.raytheon.uf.common.dataplugin.text.db;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/10/2009   2191       rjpeter     Initial Implementation
 * 04/06/10     4734       mhuang      Moved from edex server
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@Embeddable
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StdTextProductId extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Column(nullable = false, length = 6)
    @DynamicSerializeElement
    @XmlAttribute
    private String wmoid;

    @Column(nullable = false, length = 4)
    @DynamicSerializeElement
    @XmlAttribute
    private String site;

    @Column(nullable = false, length = 3)
    @DynamicSerializeElement
    @XmlAttribute
    private String cccid;

    @Column(nullable = false, length = 3)
    @DynamicSerializeElement
    @XmlAttribute
    private String nnnid;

    @Column(nullable = false, length = 3)
    @DynamicSerializeElement
    @XmlAttribute
    private String xxxid;

    @Column(nullable = false, length = 6)
    @DynamicSerializeElement
    @XmlAttribute
    private String hdrtime;

    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    private Long dataCrc;

    /** partial constructor */
    public StdTextProductId(String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime) {
        this();
        this.wmoid = wmoid;
        this.site = site;
        this.cccid = cccid;
        this.nnnid = nnnid;
        this.xxxid = xxxid;
        this.hdrtime = hdrtime;
    }

    /** full constructor */
    public StdTextProductId(String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime, Long dataCrc) {
        this(wmoid, site, cccid, nnnid, xxxid, hdrtime);
        this.dataCrc = dataCrc;
    }

    /** default constructor */
    public StdTextProductId() {
    }

    public String getWmoid() {
        return this.wmoid;
    }

    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    public String getSite() {
        return this.site;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public String getCccid() {
        return this.cccid;
    }

    public void setCccid(String cccid) {
        this.cccid = cccid;
    }

    public String getNnnid() {
        return this.nnnid;
    }

    public void setNnnid(String nnnid) {
        this.nnnid = nnnid;
    }

    public String getXxxid() {
        return this.xxxid;
    }

    public void setXxxid(String xxxid) {
        this.xxxid = xxxid;
    }

    public String getHdrtime() {
        return hdrtime;
    }

    public void setHdrtime(String hdrtime) {
        this.hdrtime = hdrtime;
    }

    public Long getDataCrc() {
        return this.dataCrc;
    }

    public void setDataCrc(Long dataCrc) {
        this.dataCrc = dataCrc;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("wmoid", getWmoid())
                .append("site", getSite()).append("cccid", getCccid())
                .append("nnnid", getNnnid()).append("xxxid", getXxxid())
                .append("hdrtime", getHdrtime())
                .append("dataCrc", getDataCrc()).toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof StdTextProductId)) {
            return false;
        }
        StdTextProductId castOther = (StdTextProductId) other;
        return new EqualsBuilder()
                .append(this.getWmoid(), castOther.getWmoid())
                .append(this.getSite(), castOther.getSite())
                .append(this.getCccid(), castOther.getCccid())
                .append(this.getNnnid(), castOther.getNnnid())
                .append(this.getXxxid(), castOther.getXxxid())
                .append(this.getHdrtime(), castOther.getHdrtime())
                .append(this.getDataCrc(), castOther.getDataCrc()).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(getWmoid()).append(getSite())
                .append(getCccid()).append(getNnnid()).append(getXxxid())
                .append(getHdrtime()).append(getDataCrc()).toHashCode();
    }

}
