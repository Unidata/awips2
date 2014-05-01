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

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *                                      Initial Implementation
 * 15 Oct 2008        1538  jkorman     Core functions added.
 * </pre>
 */
@Entity
@Table
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class WatchWarn extends PersistableDataObject implements Serializable,
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    /** identifier field */
    @EmbeddedId
    @DynamicSerializeElement
    @XmlElement
    private WatchWarnPK pk;

    /**
     * Default no-arg constructor
     */
    public WatchWarn() {
        pk = new WatchWarnPK();
    }

    /**
     * Construct an instance with supplied product identifier and a script.
     * 
     * @param productid
     *            A not null reference to the product identifier to store.
     * @param script
     *            A not null reference to the script to store.
     */
    public WatchWarn(String productid, String script) {
        pk = new WatchWarnPK(productid, script);
    }

    /**
     * Get the product identifier for this instance.
     * 
     * @return The product identifier for this instance.
     * @hibernate.property column="productid"
     */
    public String getProductid() {
        return pk.getProductid();
    }

    /**
     * Get the script for this instance.
     * 
     * @return The script for this instance.
     */
    public String getScript() {
        return pk.getScript();
    }

    public WatchWarnPK getPk() {
        return pk;
    }

    public void setPk(WatchWarnPK pk) {
        this.pk = pk;
    }

    /**
     * 
     * @param buffer
     * @return
     */
    public StringBuilder toStringBuilder(StringBuilder buffer) {
        if (buffer != null) {
            buffer = new StringBuilder(90);
        }
        buffer.append(pk.getProductid());
        buffer.append("=>{");
        buffer.append(pk.getScript());
        buffer.append("}");
        return buffer;
    }

    /**
     * Get the string representation of this class instance.
     * 
     * @return The string representation of this class instance.
     */
    @Override
    public String toString() {
        return toStringBuilder(null).toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((pk == null) ? 0 : pk.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        WatchWarn other = (WatchWarn) obj;
        if (pk == null) {
            if (other.pk != null)
                return false;
        } else if (!pk.equals(other.pk))
            return false;
        return true;
    }
}
