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

package com.raytheon.uf.common.dataplugin.gfe.server.notify;

import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Notification that a lock table has been changed
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #940       bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class LockNotification extends GfeNotification implements
        ISerializableObject {

    /** The modified LockTable */

    @DynamicSerializeElement
    private LockTable lockTable;

    /**
     * Constructs an empty LockNotification
     */
    public LockNotification() {

    }

    /**
     * Constructs a new LockNotification with a specified LockTable
     * 
     * @param lockTable
     *            The modified LockTable
     */
    public LockNotification(LockTable lockTable, String siteID) {
        this.lockTable = lockTable;
        this.siteID = siteID;
    }

    /**
     * Gets the modified lock table
     * 
     * @return The modified lock table
     */
    public LockTable getLockTable() {
        return lockTable;
    }

    /**
     * Sets the modified lock table
     * 
     * @param lockTable
     *            The modified lock table
     */
    public void setLockTable(LockTable lockTable) {
        this.lockTable = lockTable;
    }

}
