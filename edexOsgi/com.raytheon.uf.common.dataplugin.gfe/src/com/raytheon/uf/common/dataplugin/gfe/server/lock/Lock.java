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

package com.raytheon.uf.common.dataplugin.gfe.server.lock;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Represents a lock on a record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@Table(name = "gfelocktable")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Lock extends PersistableDataObject implements Cloneable,
        ISerializableObject {

    private static final long serialVersionUID = -7839912817664285509L;

    /** The key for the database */
    @Id
    @GeneratedValue
    private int key;

    /** The parmID of the lock */
    @Column
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.ParmIdType")
    @XmlElement
    @Index(name = "lock_parmId_idx")
    @DynamicSerializeElement
    private ParmID parmId;

    /** The workstationID of the lock holder */
    @Column
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.WsIdType")
    @XmlElement
    @DynamicSerializeElement
    private WsId wsId;

    /** The start time of the lock */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private long startTime;

    /** The end time of the lock */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private long endTime;

    /**
     * Creates a new Lock. Use of this constructor is discouraged. It is used by
     * JiBX
     */
    public Lock() {

    }

    /**
     * Creates a new Lock
     * 
     * @param timeRange
     *            The time range over which the lock applies
     * @param wsId
     *            The workstation ID of the lock owner
     */
    public Lock(TimeRange timeRange, WsId wsId) {
        this.startTime = timeRange.getStart().getTime();
        this.endTime = timeRange.getEnd().getTime();
        this.wsId = wsId;
    }

    public WsId getWsId() {
        return wsId;
    }

    public void setWsId(WsId wsId) {
        this.wsId = wsId;
    }

    public TimeRange getTimeRange() {
        return new TimeRange(startTime, endTime);
    }

    public void setTimeRange(TimeRange timeRange) {
        this.startTime = timeRange.getStart().getTime();
        this.endTime = timeRange.getEnd().getTime();
    }

    public ParmID getParmId() {
        return parmId;
    }

    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    public int getKey() {
        return key;
    }

    public void setKey(int key) {
        this.key = key;
    }

    public long getStartTime() {
        return startTime;
    }

    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    public long getEndTime() {
        return endTime;
    }

    public void setEndTime(long endTime) {
        this.endTime = endTime;
    }

    @Override
    public Lock clone() {
        Lock newLock = new Lock(this.getTimeRange(), wsId);
        return newLock;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("TR: ");
        buffer.append(this.getTimeRange().toString());
        buffer.append(" WsId: ");
        buffer.append(this.wsId.toPrettyString());
        return buffer.toString();
    }

}
