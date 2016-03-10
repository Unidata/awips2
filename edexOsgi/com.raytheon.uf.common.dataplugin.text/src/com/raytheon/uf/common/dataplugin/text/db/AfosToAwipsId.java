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
 * 08/28/2009   2924       rjpeter     Initial Implementation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@Embeddable
@DynamicSerialize
public class AfosToAwipsId extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Column(nullable = false, length = 9)
    @DynamicSerializeElement
    private String afosid;

    @Column(nullable = false, length = 6)
    @DynamicSerializeElement
    private String wmottaaii;

    @Column(nullable = false, length = 4)
    @DynamicSerializeElement
    private String wmocccc;

    /** default constructor */
    public AfosToAwipsId() {
    }

    public AfosToAwipsId(String afosid, String wmottaaii, String wmocccc) {
        super();
        this.afosid = afosid;
        this.wmottaaii = wmottaaii;
        this.wmocccc = wmocccc;
    }

    public String getWmottaaii() {
        return wmottaaii;
    }

    public void setWmottaaii(String wmottaaii) {
        this.wmottaaii = wmottaaii;
    }

    public String getWmocccc() {
        return wmocccc;
    }

    public void setWmocccc(String wmocccc) {
        this.wmocccc = wmocccc;
    }

    public String getAfosid() {
        return afosid;
    }

    public void setAfosid(String afosid) {
        this.afosid = afosid;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("afosid", getAfosid())
                .append("wmottaaii", getWmottaaii())
                .append("wmocccc", getWmocccc()).toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof AfosToAwipsId)) {
            return false;
        }
        AfosToAwipsId castOther = (AfosToAwipsId) other;
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
