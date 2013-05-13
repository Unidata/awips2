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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.serialize.LockTableAdapter;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Structure for encapsulating lock information for a parmID
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 04/23/13     #1949      rjpeter     Added serialization adapter
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = LockTableAdapter.class)
public class LockTable implements Cloneable, ISerializableObject {

    /** Enumeration denoting status of the lock */
    public enum LockStatus {
        LOCKABLE, LOCKED_BY_ME, LOCKED_BY_OTHER
    };

    /** Enumeration denoting Lock Modes */
    public enum LockMode {
        LOCK, UNLOCK, BREAK_LOCK
    };

    /** List of locks contained in this lock table */
    private List<Lock> locks;

    /** The workstation ID of the owner of this lock table */
    private WsId wsId;

    /** The parm ID for which this lock table holds lock information for */
    private ParmID parmId;

    /**
     * Creates a new LockTable. Used solely by JiBX
     */
    public LockTable() {
        locks = new ArrayList<Lock>();
    }

    /**
     * Creates a new LockTable
     * 
     * @param id
     *            The parmID associated with this lock table
     * @param locks
     *            The locks
     * @param wsId
     *            The workstation ID of the owner of the lock table
     */
    public LockTable(ParmID id, List<Lock> locks, WsId wsId) {
        this.parmId = id;
        this.locks = locks;
        this.wsId = wsId;
    }

    /**
     * Translates the LockMode into a String
     * 
     * @param mode
     *            The lock mode
     * @return The String representation of the LockMode
     */
    public static String modeAsTextString(LockMode mode) {
        if (mode.equals(LockMode.LOCK)) {
            return "LOCK";
        } else if (mode.equals(LockMode.UNLOCK)) {
            return "UNLOCK";
        } else if (mode.equals(LockMode.BREAK_LOCK)) {
            return "BREAK_LOCK";
        } else {
            return "";
        }
    }

    /**
     * Checks if any of the locks contained in this lock table are locked by
     * this workstation ID
     * 
     * @return True if one or more locks are owned by this workstation ID, else
     *         false
     */
    public boolean anyLockedByMe() {

        for (Lock lock : locks) {
            if (lock.equals(wsId)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if any locks contained in this lock table fall within the given
     * time range
     * 
     * @param timeRange
     *            The time range over which to check
     * @return True if any locks contained in this lock table fall within the
     *         given time range
     */
    public boolean anyLocked(TimeRange timeRange) {

        for (Lock lock : locks) {
            if (lock.getTimeRange().overlaps(timeRange)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets the list of locks owned by me
     * 
     * @return The list of locks owned by me
     */
    public List<TimeRange> lockedByMe() {
        List<TimeRange> lbm = new ArrayList<TimeRange>();

        for (Lock lock : locks) {
            if (lock.getWsId().equals(wsId)) {
                lbm.add(lock.getTimeRange());
            }
        }
        return lbm;
    }

    /**
     * Gets the list of locks owned by others
     * 
     * @return
     */
    public List<TimeRange> lockedByOther() {

        List<TimeRange> lbo = new ArrayList<TimeRange>();

        for (Lock lock : locks) {
            if (!lock.getWsId().equals(wsId)) {
                lbo.add(lock.getTimeRange());
            }
        }

        return lbo;
    }

    /**
     * Checks the status of the lock against the provided timeRange. The
     * workstation ID is assumed to be my workstation ID.
     * 
     * @param timeRange
     *            The time range over which to check
     * @return The lock status of the specified time range
     */
    public LockStatus checkLock(TimeRange timeRange) {
        return checkLock(timeRange, wsId);
    }

    /**
     * Checks the status of the lock against the provided requestorID and
     * timeRange
     * 
     * @param timeRange
     *            The time range over which to check
     * @param requestorId
     *            The requestor ID to check for
     * @return The lock status of the specified time range
     */
    public LockStatus checkLock(TimeRange timeRange, WsId requestorId) {
        for (Lock lock : locks) {
            TimeRange tr = lock.getTimeRange();
            if (timeRange.overlaps(tr)) {
                if (!requestorId.equals(lock.getWsId())) {
                    return LockStatus.LOCKED_BY_OTHER;
                } else if (tr.contains(timeRange.getStart())
                        && (tr.getEnd().after(timeRange.getEnd()) || tr
                                .getEnd().equals(timeRange.getEnd()))) {
                    return LockStatus.LOCKED_BY_ME;
                }
            }
        }

        return LockStatus.LOCKABLE;
    }

    public void removeLocks(Collection<Lock> locksToRemove) {
        this.locks.removeAll(locksToRemove);
    }

    public void resetWsId(WsId wsId) {
        setWsId(wsId);
    }

    public List<Lock> getLocks() {
        return locks;
    }

    public void setLocks(List<Lock> locks) {
        this.locks = locks;
    }

    public void addLock(Lock lock) {
        if (this.locks == null) {
            this.locks = new ArrayList<Lock>();
        }
        this.locks.add(lock);
    }

    public void addLocks(Collection<Lock> locks) {
        if (this.locks == null) {
            this.locks = new ArrayList<Lock>();
        }
        this.locks.addAll(locks);
    }

    public WsId getWsId() {
        return wsId;
    }

    public void setWsId(WsId wsId) {
        this.wsId = wsId;
    }

    public ParmID getParmId() {
        return parmId;
    }

    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    @Override
    public LockTable clone() {
        // locks are immutable so this is safe
        List<Lock> lockList = new ArrayList<Lock>(locks);
        return new LockTable(this.parmId, lockList, this.wsId);
    }

    public static LockMode getLockMode(String modeName) {
        return Enum.valueOf(LockMode.class, modeName);
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("ParmID:");
        buffer.append(parmId.toString());
        buffer.append(" LockTable WsId: ");
        if (wsId == null) {
            buffer.append("null");
        } else {
            buffer.append(wsId.toPrettyString());
        }
        for (int i = 0; i < locks.size(); i++) {
            buffer.append("  Lock: ");
            buffer.append(locks.get(i).toString()).append("\n");
        }
        return buffer.toString();
    }
}
