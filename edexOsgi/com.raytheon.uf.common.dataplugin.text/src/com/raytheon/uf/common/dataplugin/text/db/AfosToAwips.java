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

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.hibernate.annotations.Index;

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
 * 08/28/2009   2924       rjpeter     Initial Implementation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@Entity
@Table(name = "afos_to_awips")
@org.hibernate.annotations.Table(appliesTo = "afos_to_awips", indexes = {
        @Index(name = "idx_afosid", columnNames = { "afosid" }),
        @Index(name = "idx_wmottaaii_wmocccc", columnNames = { "wmottaaii",
                "wmocccc" }) })
@DynamicSerialize
public class AfosToAwips extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    @EmbeddedId
    @DynamicSerializeElement
    private AfosToAwipsId id;

    public AfosToAwips(AfosToAwipsId id) {
        super();
        this.id = id;
    }

    /** default constructor */
    public AfosToAwips() {
        id = new AfosToAwipsId();
    }

    public AfosToAwipsId getId() {
        return id;
    }

    public void setId(AfosToAwipsId id) {
        this.id = id;
    }

    public String getWmottaaii() {
        return id.getWmottaaii();
    }

    public void setWmottaaii(String wmottaaii) {
        id.setWmottaaii(wmottaaii);
    }

    public String getWmocccc() {
        return id.getWmocccc();
    }

    public void setWmocccc(String wmocccc) {
        id.setWmocccc(wmocccc);
    }

    public String getAfosid() {
        return id.getAfosid();
    }

    public void setAfosid(String afosid) {
        id.setAfosid(afosid);
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("afosid", getAfosid())
                .append("wmottaaii", getWmottaaii())
                .append("wmocccc", getWmocccc()).toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof AfosToAwips)) {
            return false;
        }
        AfosToAwips castOther = (AfosToAwips) other;
        return new EqualsBuilder()
                .append(this.getAfosid(), castOther.getAfosid())
                .append(this.getWmottaaii(), castOther.getWmottaaii())
                .append(this.getWmocccc(), castOther.getWmocccc()).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(getAfosid()).append(getWmottaaii())
                .append(getWmocccc()).toHashCode();
    }
}
