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

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Data Delivery System Status.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2013   1655     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Entity
@Table(name = "DDSystemStatus")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataDeliverySystemStatus extends
        PersistableDataObject<DataDeliverySystemStatusId> {

    private static final long serialVersionUID = -8864011415705803837L;

    @DynamicSerializeElement
    @EmbeddedId
    private DataDeliverySystemStatusId key;

    @XmlElement
    @DynamicSerializeElement
    @Column
    private String status;

    /**
     * @return the status
     */
    public String getStatus() {
        return status;
    }

    /**
     * @param status
     *            the status to set
     */
    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * @return the key
     */
    public DataDeliverySystemStatusId getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(DataDeliverySystemStatusId key) {
        this.key = key;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder buffer = new StringBuilder();
        if (identifier != null) {
            buffer.append("SystemType: ").append(identifier.getSystemType())
                    .append("\n");
            buffer.append("Name:       ").append(identifier.getName())
                    .append("\n");
            buffer.append("Status:     ").append(status);
        }

        return this.toString();
    }
}
