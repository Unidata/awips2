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

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Embeddable
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class TextProductInfoPK extends PersistableDataObject {

    private static final long serialVersionUID = 1L;

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

    /** full constructor */
    public TextProductInfoPK(String cccid, String nnnid, String xxxid) {
        this.cccid = cccid;
        this.nnnid = nnnid;
        this.xxxid = xxxid;
    }

    /** default constructor */
    public TextProductInfoPK() {
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

    public String toString() {
        return new StringBuilder(cccid).append(":").append(nnnid).append(":")
                .append(xxxid).toString();
    }

    public boolean equals(Object other) {
        if (!(other instanceof TextProductInfoPK))
            return false;
        TextProductInfoPK castOther = (TextProductInfoPK) other;
        return new EqualsBuilder()
                .append(this.getCccid(), castOther.getCccid()).append(
                        this.getNnnid(), castOther.getNnnid()).append(
                        this.getXxxid(), castOther.getXxxid()).isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder().append(getCccid()).append(getNnnid())
                .append(getXxxid()).toHashCode();
    }

}
