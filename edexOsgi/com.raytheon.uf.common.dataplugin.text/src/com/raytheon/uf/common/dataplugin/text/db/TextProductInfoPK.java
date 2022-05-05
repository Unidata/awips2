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

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Primary key for TextProductInfo table
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------
 * Apr 26, 2018  7278     randerso  Pad fields to max length
 *
 * </pre>
 *
 * @author ???
 */
@Embeddable
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class TextProductInfoPK implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final int MAX_FIELD_LENGTH = 3;

    @Column(nullable = false, length = MAX_FIELD_LENGTH)
    @DynamicSerializeElement
    @XmlAttribute
    private String cccid;

    @Column(nullable = false, length = MAX_FIELD_LENGTH)
    @DynamicSerializeElement
    @XmlAttribute
    private String nnnid;

    @Column(nullable = false, length = MAX_FIELD_LENGTH)
    @DynamicSerializeElement
    @XmlAttribute
    private String xxxid;

    /**
     * full constructor
     *
     * @param cccid
     * @param nnnid
     * @param xxxid
     */
    public TextProductInfoPK(String cccid, String nnnid, String xxxid) {
        this.cccid = StringUtils.rightPad(cccid, MAX_FIELD_LENGTH);
        this.nnnid = StringUtils.rightPad(nnnid, MAX_FIELD_LENGTH);
        this.xxxid = StringUtils.rightPad(xxxid, MAX_FIELD_LENGTH);
    }

    /** default constructor */
    public TextProductInfoPK() {
    }

    /**
     * @return the cccid
     */
    public String getCccid() {
        return this.cccid;
    }

    /**
     * Pads to max length and sets cccid
     *
     * @param cccid
     */
    public void setCccid(String cccid) {
        this.cccid = StringUtils.rightPad(cccid, MAX_FIELD_LENGTH);
    }

    /**
     * @return the nnnid
     */
    public String getNnnid() {
        return this.nnnid;
    }

    /**
     * Pads to max length and sets nnnid
     *
     * @param nnnid
     */
    public void setNnnid(String nnnid) {
        this.nnnid = StringUtils.rightPad(nnnid, MAX_FIELD_LENGTH);
    }

    /**
     * @return the xxxid
     */
    public String getXxxid() {
        return this.xxxid;
    }

    /**
     * * Pads to max length and sets xxxid
     *
     * @param xxxid
     */
    public void setXxxid(String xxxid) {
        this.xxxid = StringUtils.rightPad(xxxid, MAX_FIELD_LENGTH);
    }

    @Override
    public String toString() {
        return new StringBuilder(cccid).append(":").append(nnnid).append(":")
                .append(xxxid).toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other.getClass() != this.getClass())) {
            return false;
        }
        TextProductInfoPK castOther = (TextProductInfoPK) other;
        return new EqualsBuilder().append(this.getCccid(), castOther.getCccid())
                .append(this.getNnnid(), castOther.getNnnid())
                .append(this.getXxxid(), castOther.getXxxid()).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(getCccid()).append(getNnnid())
                .append(getXxxid()).toHashCode();
    }

}
