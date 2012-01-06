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

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.Embedded;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Primary key for a ISCSendRecord.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@Embeddable
@DynamicSerialize
public class IscSendRecordPK implements ISerializableObject, Serializable,
        Cloneable {

    private static final long serialVersionUID = 1L;

    public enum IscSendState {
        QUEUED, PENDING, RUNNING
    };

    @DynamicSerializeElement
    @Column(nullable = false)
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.ParmIdType")
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

    @Transient
    private transient int hashCode;

    public IscSendRecordPK() {
        this.state = IscSendState.QUEUED;
        this.setXmlDest("");
    }

    public IscSendRecordPK(ParmID parmID, TimeRange tr) {
        this();
        this.parmID = parmID;
        this.timeRange = tr;
        generateHashCode();
    }

    public IscSendRecordPK(ParmID parmID, TimeRange tr, IscSendState state,
            String xmlDest) {
        this.state = state;
        this.parmID = parmID;
        this.timeRange = tr;
        this.xmlDest = xmlDest;
        generateHashCode();
    }

    @Override
    public int hashCode() {
        return hashCode;
    }

    private void generateHashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((parmID == null) ? 0 : parmID.hashCode());
        result = prime * result + ((state == null) ? 0 : state.hashCode());
        result = prime * result
                + ((timeRange == null) ? 0 : timeRange.hashCode());
        result = prime * result + ((xmlDest == null) ? 0 : xmlDest.hashCode());
        hashCode = result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        IscSendRecordPK other = (IscSendRecordPK) obj;
        if (parmID == null) {
            if (other.parmID != null)
                return false;
        } else if (!parmID.equals(other.parmID))
            return false;
        if (state == null) {
            if (other.state != null)
                return false;
        } else if (!state.equals(other.state))
            return false;
        if (timeRange == null) {
            if (other.timeRange != null)
                return false;
        } else if (!timeRange.equals(other.timeRange))
            return false;
        if (xmlDest == null) {
            if (other.xmlDest != null)
                return false;
        } else if (!xmlDest.equals(other.xmlDest))
            return false;
        return true;
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        return new IscSendRecordPK(parmID, timeRange.clone(), state,
                new String(xmlDest));
    }

    @Override
    public String toString() {
        StringBuilder tmp = new StringBuilder();
        tmp.append(parmID.toString());
        tmp.append(" TimeRange: ");
        if (timeRange != null) {
            tmp.append(timeRange.toString());
        } else {
            tmp.append("null");
        }
        return tmp.toString();
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

    public void setXmlDest(String xmlDest) {
        this.xmlDest = xmlDest;
    }

    public String getXmlDest() {
        return xmlDest;
    }

    /**
     * Determines whether a given record can be merged with this record. Records
     * are mergeable if and only if:
     * <ul>
     * <li>The ParmIDs is the same.
     * <li>The send state is the same.
     * <li>The XML destinations data is the same.
     * <li>The time range is adjacent to or overlaps our time range.
     * </ul>
     * 
     * @param other
     *            The object to compare with.
     * @return Whether or not the given record can be merged together with this
     *         record.
     */
    public boolean isMergeableWith(IscSendRecordPK other) {
        if ((parmID.equals(other.parmID)) && (state.equals(other.state))
                && xmlDest.equals(other.xmlDest)) {
            return (timeRange.isAdjacentTo(other.timeRange) || timeRange
                    .overlaps(other.timeRange));
        }
        return false;
    }
}
