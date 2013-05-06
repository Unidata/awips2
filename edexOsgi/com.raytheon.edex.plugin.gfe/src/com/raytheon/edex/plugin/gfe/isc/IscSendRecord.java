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
package com.raytheon.edex.plugin.gfe.isc;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * ISC send job record that represents a send job for a single ParmID and its
 * state.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling     Initial creation
 * May 08, 2012  #600      dgilling     Restructure.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@Entity
@Table(name = "gfe_iscsend", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "parmID", "rangeStart", "rangeEnd", "state", "xmlDest" }) })
@DynamicSerialize
public class IscSendRecord implements IPersistableDataObject, Serializable,
        ISerializableObject, Cloneable {

    private static final long serialVersionUID = 1L;

    public enum IscSendState {
        QUEUED, PENDING, RUNNING
    };

    @Id
    @GeneratedValue()
    private int key;

    // TODO: Normalize with parmId table
    @DynamicSerializeElement
    @Column(nullable = false)
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.ParmIdType")
    @Index(name = "iscParmIdIndex")
    private ParmID parmID;

    @DynamicSerializeElement
    @Embedded
    private TimeRange timeRange;

    @DynamicSerializeElement
    @Column(nullable = false, length = 10)
    @Enumerated(EnumType.STRING)
    private IscSendState state;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    private String xmlDest;

    @Column
    @DynamicSerializeElement
    @Index(name = "iscSendInsertTimeIndex")
    private Date insertTime;

    public IscSendRecord() {
        // no-op, needed for dynamic serialize/hibernate
    }

    public IscSendRecord(ParmID parmId, TimeRange sendTR, boolean sendNow) {
        this(parmId, sendTR, "", sendNow);
    }

    public IscSendRecord(ParmID parmId, TimeRange sendTR, String xmlDest,
            boolean sendNow) {
        this(parmId, sendTR, xmlDest, (sendNow ? IscSendState.QUEUED
                : IscSendState.PENDING));
    }

    public IscSendRecord(ParmID parmId, TimeRange sendTR, String xmlDest,
            IscSendState state) {
        this.parmID = parmId;
        this.timeRange = sendTR;
        this.xmlDest = xmlDest;
        this.state = state;
        this.insertTime = new Date();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject#
     * getIdentifier()
     */
    @Override
    public Object getIdentifier() {
        return key;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public IscSendRecord clone() throws CloneNotSupportedException {
        IscSendRecord rval = new IscSendRecord(this.parmID,
                this.timeRange.clone(), this.xmlDest, this.state);
        rval.setInsertTime((Date) this.insertTime.clone());
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((insertTime == null) ? 0 : insertTime.hashCode());
        result = prime * result + ((parmID == null) ? 0 : parmID.hashCode());
        result = prime * result + ((state == null) ? 0 : state.hashCode());
        result = prime * result
                + ((timeRange == null) ? 0 : timeRange.hashCode());
        result = prime * result + ((xmlDest == null) ? 0 : xmlDest.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
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
        IscSendRecord other = (IscSendRecord) obj;
        if (insertTime == null) {
            if (other.insertTime != null) {
                return false;
            }
        } else if (!insertTime.equals(other.insertTime)) {
            return false;
        }
        if (parmID == null) {
            if (other.parmID != null) {
                return false;
            }
        } else if (!parmID.equals(other.parmID)) {
            return false;
        }
        if (state != other.state) {
            return false;
        }
        if (timeRange == null) {
            if (other.timeRange != null) {
                return false;
            }
        } else if (!timeRange.equals(other.timeRange)) {
            return false;
        }
        if (xmlDest == null) {
            if (other.xmlDest != null) {
                return false;
            }
        } else if (!xmlDest.equals(other.xmlDest)) {
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("IscSendRecord [parmID=");
        builder.append(parmID);
        builder.append(", timeRange=");
        builder.append(timeRange);
        builder.append(", state=");
        builder.append(state);
        builder.append("]");
        return builder.toString();
    }

    public Date getInsertTime() {
        return insertTime;
    }

    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    public ParmID getParmID() {
        return parmID;
    }

    public void setParmID(ParmID parmID) {
        this.parmID = parmID;
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    public IscSendState getState() {
        return state;
    }

    public void setState(IscSendState state) {
        this.state = state;
    }

    public String getXmlDest() {
        return xmlDest;
    }

    public void setXmlDest(String xmlDest) {
        this.xmlDest = xmlDest;
    }

    public int getKey() {
        return key;
    }
}
