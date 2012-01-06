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

package com.raytheon.edex.plugin.gfe.server.lock;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.db.dao.GFELockDao;
import com.raytheon.edex.plugin.gfe.exception.GfeLockException;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Port of the existing LockDatabase. This class maintains the locks.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class LockDatabase {

    /** The logger */
    protected Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static LockDatabase instance;

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public synchronized static LockDatabase getInstance() {
        if (instance == null) {
            instance = new LockDatabase();
        }
        return instance;
    }

    /**
     * Returns all the locks currently held
     * 
     * @return All locks
     * @throws GfeLockException
     *             If errors occur during querying
     */
    public synchronized ServerResponse<List<LockTable>> getDatabase() {
        ServerResponse<List<LockTable>> sr = new ServerResponse<List<LockTable>>();
        List<LockTable> tables = new ArrayList<LockTable>();
        GFELockDao dao = new GFELockDao();

        List<Lock> locks = null;
        try {
            locks = dao.getAllLocks();
        } catch (DataAccessLayerException e) {
            sr.addMessage("Error get locked records from the database");
            logger.error("Error get locked records from the database", e);
        }

        for (Lock lock : locks) {
            addEntry(tables, lock);
        }
        sr.setPayload(tables);
        return sr;
    }

    /**
     * Adds an entry to the lock database
     * 
     * @param tables
     *            The list of tables
     * @param pid
     *            The parm ID of the lock table to add
     * @param locker
     *            The workstation ID of the lock owner
     * @param timeRange
     *            The time range of the lock
     */
    private void addEntry(List<LockTable> tables, Lock lock) {

        int index = -1;
        // find the lock table. If not available, create one
        for (int i = 0; i < tables.size(); i++) {
            if (tables.get(i).getParmId().equals(lock.getParmId())) {
                index = i;
                break;
            }
        }

        // need a new LockTable
        if (index == -1) {
            tables.add(new LockTable(lock.getParmId(), new ArrayList<Lock>(),
                    null));
            index = tables.size() - 1;
        }
        LockTable lockTable = tables.get(index);
        lockTable.addLock(lock);
    }
}
