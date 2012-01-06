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
package com.raytheon.uf.edex.database.cluster;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.handler.IClusterLockHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@Entity
@Table(name = "cluster_task", schema = "awips")
public class ClusterTask implements ISerializableObject, Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    protected ClusterTaskPK id;

    @Column
    protected long lastExecution;

    @Column
    protected boolean running;

    @Column(nullable = true, length = 256)
    protected String extraInfo;

    @Transient
    protected LockState lockState;

    @Transient
    protected IClusterLockHandler lockHandler;

    public ClusterTaskPK getId() {
        return id;
    }

    public void setId(ClusterTaskPK id) {
        this.id = id;
    }

    public long getLastExecution() {
        return lastExecution;
    }

    public void setLastExecution(long lastExecution) {
        this.lastExecution = lastExecution;
    }

    /**
     * @return the isRunning
     */
    public boolean isRunning() {
        return running;
    }

    /**
     * @param isRunning
     *            the isRunning to set
     */
    public void setRunning(boolean isRunning) {
        this.running = isRunning;
    }

    public String getExtraInfo() {
        return extraInfo;
    }

    public void setExtraInfo(String extraInfo) {
        this.extraInfo = extraInfo;
    }

    /**
     * Returns the lock state
     * 
     * @return LockState
     */
    public LockState getLockState() {
        return lockState;
    }

    /**
     * Sets the lock state.
     * 
     * @param lockState
     */
    protected void setLockState(LockState lockState) {
        this.lockState = lockState;
    }

    protected IClusterLockHandler getLockHandler() {
        return lockHandler;
    }

    protected void setLockHandler(IClusterLockHandler lockHandler) {
        this.lockHandler = lockHandler;
    }
}
