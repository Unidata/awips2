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
public class StateMatchPK extends PersistableDataObject {

    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    private String state;

    @Column(nullable = false, length = 3)
    @DynamicSerializeElement
    @XmlAttribute
    private String xxx;

    @Column(nullable = false, length = 3)
    @DynamicSerializeElement
    @XmlAttribute
    private String ccc;

    public StateMatchPK(String state, String xxx, String ccc) {
        this.state = state;
        this.xxx = xxx;
        this.ccc = ccc;
    }

    public StateMatchPK() {
    }

    public String getState() {
        return this.state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getXxx() {
        return this.xxx;
    }

    public void setXxx(String xxx) {
        this.xxx = xxx;
    }

    public String getCcc() {
        return this.ccc;
    }

    public void setCcc(String ccc) {
        this.ccc = ccc;
    }

    public String toString() {
        return new ToStringBuilder(this).append("state", getState()).append(
                "xxx", getXxx()).append("ccc", getCcc()).toString();
    }

    public boolean equals(Object other) {
        if (!(other instanceof StateMatchPK))
            return false;
        StateMatchPK castOther = (StateMatchPK) other;
        return new EqualsBuilder()
                .append(this.getState(), castOther.getState()).append(
                        this.getXxx(), castOther.getXxx()).append(
                        this.getCcc(), castOther.getCcc()).isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder().append(getState()).append(getXxx())
                .append(getCcc()).toHashCode();
    }

}
