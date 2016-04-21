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
import org.apache.commons.lang.builder.ToStringBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Embeddable
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class WatchWarnPK extends PersistableDataObject {

    private static final long serialVersionUID = 1L;

    @Column(nullable = false, length = 9)
    @DynamicSerializeElement
    @XmlAttribute
    private String productid;

    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    private String script;

    public WatchWarnPK(String productid, String script) {
        this.productid = productid;
        this.script = script;
    }

    public WatchWarnPK() {
    }

    public String getProductid() {
        return this.productid;
    }

    public void setProductid(String productid) {
        this.productid = productid;
    }

    public String getScript() {
        return this.script;
    }

    public void setScript(String script) {
        this.script = script;
    }

    public String toString() {
        return new ToStringBuilder(this).append("productid", getProductid())
                .append("script", getScript()).toString();
    }

    public boolean equals(Object other) {
        if (!(other instanceof WatchWarnPK))
            return false;
        WatchWarnPK castOther = (WatchWarnPK) other;
        return new EqualsBuilder().append(this.getProductid(),
                castOther.getProductid()).append(this.getScript(),
                castOther.getScript()).isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder().append(getProductid()).append(getScript())
                .toHashCode();
    }

}
