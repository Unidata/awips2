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
package com.raytheon.uf.common.datadelivery.event.status;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Key for the Data Delivery System Status Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2013    1655     mpduff      Initial creation.
 * Jan 17, 2014 2125       rjpeter     EmbeddedId classes need hashCode/equals.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Embeddable
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class DataDeliverySystemStatusId implements Serializable,
        ISerializableObject {

    private static final long serialVersionUID = -6970802086185781078L;

    @XmlElement
    @DynamicSerializeElement
    @Column
    private String systemType;

    @XmlElement
    @DynamicSerializeElement
    @Column
    private String name;

    /**
     * @return the systemType
     */
    public String getSystemType() {
        return systemType;
    }

    /**
     * @param systemType
     *            the systemType to set
     */
    public void setSystemType(String systemType) {
        this.systemType = systemType;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result) + ((name == null) ? 0 : name.hashCode());
        result = (prime * result)
                + ((systemType == null) ? 0 : systemType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        DataDeliverySystemStatusId other = (DataDeliverySystemStatusId) obj;
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        if (systemType == null) {
            if (other.systemType != null) {
                return false;
            }
        } else if (!systemType.equals(other.systemType)) {
            return false;
        }
        return true;
    }
}
