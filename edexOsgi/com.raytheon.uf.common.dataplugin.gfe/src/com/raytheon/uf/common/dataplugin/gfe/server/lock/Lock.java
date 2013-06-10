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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Represents a lock on a record. Class is Immutable and is only serializable as
 * part of a LockTable.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 03/28/13     1949       rjpeter     Normalized database structure, made immutable.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@Table(name = "gfe_locks", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "parmId_id", "startTime", "endTime" }) })
public class Lock {

    private static final long serialVersionUID = -7839912817664285509L;

    /**
     * Auto-generated surrogate key
     */
    @Id
    @SequenceGenerator(name = "GFE_DBID_GENERATOR", sequenceName = "gfe_lock_seq")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GFE_DBID_GENERATOR")
    private int id;

    /**
     * The parmID of the lock.
     */
    @ManyToOne(fetch = FetchType.EAGER, optional = false)
    @PrimaryKeyJoinColumn
    private ParmID parmId;

    /** The workstationID of the lock holder */
    @Column(nullable = false)
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.WsIdType")
    private WsId wsId;

    /**
     * Used as the hibernate field so that Database has a human readable field.
     */
    @Column(name = "startTime", nullable = false)
    private Date startDate;

    /**
     * Used as the hibernate field so that Database has a human readable field.
     */
    @Column(name = "endTime", nullable = false)
    private Date endDate;

    @Transient
    private transient TimeRange tr;

    /**
     * Creates a new Lock. Use of this constructor is discouraged.
     */
    public Lock() {

    }

    /**
     * Creates a new Lock
     * 
     * @param parmId
     *            The parmID of the lock.
     * @param timeRange
     *            The time range over which the lock applies
     * @param wsId
     *            The workstation ID of the lock owner
     */
    public Lock(ParmID parmId, TimeRange timeRange, WsId wsId) {
        this.parmId = parmId;
        this.startDate = new Date(timeRange.getStart().getTime());
        this.endDate = new Date(timeRange.getEnd().getTime());
        this.wsId = wsId;
    }

    /**
     * Creates a new Lock
     * 
     * @param parmId
     *            The parmID of the lock.
     * @param timeRange
     *            The time range over which the lock applies
     * @param wsId
     *            The workstation ID of the lock owner
     */
    public Lock(ParmID parmId, WsId wsId, long startTime, long endTime) {
        this.parmId = parmId;
        this.wsId = wsId;
        this.startDate = new Date(startTime);
        this.endDate = new Date(endTime);
    }

    public WsId getWsId() {
        return wsId;
    }

    public TimeRange getTimeRange() {
        if (tr == null) {
            tr = new TimeRange(startDate, endDate);
        }

        return tr;
    }

    public ParmID getParmId() {
        return parmId;
    }

    public int getId() {
        return id;
    }

    public long getStartTime() {
        return startDate.getTime();
    }

    public long getEndTime() {
        return endDate.getTime();
    }

    public Date getStartDate() {
        return startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("TR: ");
        buffer.append(this.getTimeRange().toString());
        buffer.append(" WsId: ");
        buffer.append(this.wsId.toPrettyString());
        return buffer.toString();
    }
}
