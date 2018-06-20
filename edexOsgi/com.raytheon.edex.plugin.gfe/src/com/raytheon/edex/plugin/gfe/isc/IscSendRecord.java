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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
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
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------
 * Oct 20, 2011           dgilling  Initial creation
 * May 08, 2012  600      dgilling  Restructure.
 * Feb 02, 2017  3847     randerso  Change parmId to link to parmId table
 *
 * </pre>
 *
 * @author dgilling
 */

@Entity
@Table(name = "gfe_iscsend", uniqueConstraints = {
        @UniqueConstraint(name = "uk_gfe_iscsend", columnNames = { "parmId_id",
                "rangeStart", "rangeEnd", "state", "xmlDest" }) })
@DynamicSerialize
public class IscSendRecord
        implements IPersistableDataObject<Integer>, Cloneable {

    /**
     * ISC Send State
     */
    public enum IscSendState {
        /**
         * Send request queued and ready to be sent
         */
        QUEUED,

        /**
         * Send request held waiting for manual send
         */
        PENDING,

        /**
         * Send request actively in process
         */
        RUNNING
    };

    @Id
    @GeneratedValue()
    private int key;

    @ManyToOne(fetch = FetchType.EAGER, optional = false)
    @OnDelete(action = OnDeleteAction.CASCADE)
    @ForeignKey(name = "fk_gfe_iscsend_to_gfe_parmid")
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    private ParmID parmId;

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

    /**
     * Default constructor for serialization
     */
    public IscSendRecord() {
        // no-op, needed for dynamic serialize/hibernate
    }

    /**
     * Constructor
     *
     * @param parmId
     *            ParmID of grids to be sent
     * @param sendTR
     *            TimeRange of grids to be sent
     * @param xmlDest
     *            Destination for send. If empty will lookup in IRT
     * @param sendNow
     *            true to send now, false to send later
     */
    public IscSendRecord(ParmID parmId, TimeRange sendTR, String xmlDest,
            boolean sendNow) {
        this(parmId, sendTR, xmlDest,
                (sendNow ? IscSendState.QUEUED : IscSendState.PENDING));
    }

    private IscSendRecord(ParmID parmId, TimeRange sendTR, String xmlDest,
            IscSendState state) {
        this.parmId = parmId;
        this.timeRange = sendTR;
        this.xmlDest = xmlDest;
        this.state = state;
        this.insertTime = new Date();
    }

    @Override
    public Integer getIdentifier() {
        return key;
    }

    @Override
    public IscSendRecord clone() {
        IscSendRecord rval = new IscSendRecord(this.parmId,
                this.timeRange.clone(), this.xmlDest, this.state);
        rval.setInsertTime((Date) this.insertTime.clone());
        return rval;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((insertTime == null) ? 0 : insertTime.hashCode());
        result = (prime * result) + ((parmId == null) ? 0 : parmId.hashCode());
        result = (prime * result) + ((state == null) ? 0 : state.hashCode());
        result = (prime * result)
                + ((timeRange == null) ? 0 : timeRange.hashCode());
        result = (prime * result)
                + ((xmlDest == null) ? 0 : xmlDest.hashCode());
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
        IscSendRecord other = (IscSendRecord) obj;
        if (insertTime == null) {
            if (other.insertTime != null) {
                return false;
            }
        } else if (!insertTime.equals(other.insertTime)) {
            return false;
        }
        if (parmId == null) {
            if (other.parmId != null) {
                return false;
            }
        } else if (!parmId.equals(other.parmId)) {
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

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("IscSendRecord [parmID=");
        builder.append(parmId);
        builder.append(", timeRange=");
        builder.append(timeRange);
        builder.append(", state=");
        builder.append(state);
        builder.append("]");
        return builder.toString();
    }

    /**
     * @return the parmId
     */
    public ParmID getParmId() {
        return parmId;
    }

    /**
     * @param parmId
     *            the parmId to set
     */
    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    /**
     * @return the timeRange
     */
    public TimeRange getTimeRange() {
        return timeRange;
    }

    /**
     * @param timeRange
     *            the timeRange to set
     */
    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    /**
     * @return the state
     */
    public IscSendState getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(IscSendState state) {
        this.state = state;
    }

    /**
     * @return the xmlDest
     */
    public String getXmlDest() {
        return xmlDest;
    }

    /**
     * @param xmlDest
     *            the xmlDest to set
     */
    public void setXmlDest(String xmlDest) {
        this.xmlDest = xmlDest;
    }

    /**
     * @return the insertTime
     */
    public Date getInsertTime() {
        return insertTime;
    }

    /**
     * @param insertTime
     *            the insertTime to set
     */
    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    /**
     * @return the key
     */
    public int getKey() {
        return key;
    }
}
