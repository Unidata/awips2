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
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.esb.camel.jms.IBrokerConnectionsProvider;

/**
 * GFE task to clear orphaned locks from the database table.  Orphaned
 * locks are locks whose session ID is not in the list of current Qpid
 * sessions.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2015  629    mgamazaychikov Initial creation
 *
 * </pre>
 *
 * @author mgamazaychikov
 * @version 1.0
 */

public class ClearGfeOrphanedLocks {
    private static IBrokerConnectionsProvider provider;
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClearGfeOrphanedLocks.class);
    public final String CAVE = "CAVE";

    public static void setProvider(IBrokerConnectionsProvider provider) {
        ClearGfeOrphanedLocks.provider = provider;
    }

    private Set<String> breakAllLocks(List<LockTable> lockTables,
            LockManager lockMgr) {
        Set<String> inactives = new HashSet<String>();
        for (LockTable lockTable : lockTables) {
            for (Lock lock : lockTable.getLocks()) {
                TimeRange tr = lock.getTimeRange();
                List<LockRequest> lreq = new ArrayList<LockRequest>();
                lreq.add(new LockRequest(lock.getParmId(), tr,
                        LockMode.BREAK_LOCK));
                lockMgr.requestLockChange(lreq, lock.getWsId());
                if (!inactives.contains(lock.getWsId().toPrettyString())
                        && !inactives.contains(lock.getParmId().toString())) {
                    String message = " Breaking orphaned lock on "
                            + lock.getParmId().toString() + " owned by "
                            + lock.getWsId().toPrettyString() + ".";
                    inactives.add(message);
                }

            }
        }
        return inactives;
    }

    private Set<String> breakLocks(Set<String> clients,
            List<LockTable> lockTables, LockManager lockMgr) {
        Set<String> inactives = new HashSet<String>();
        for (LockTable lockTable : lockTables) {
            for (Lock lock : lockTable.getLocks()) {
                String lockedWsid = lock.getWsId().toString();
                for (String client : clients) {
                    if (!lockedWsid.equals(client)) {
                        // Inactive CAVE clients found - break its lock
                        List<LockRequest> lreq = new ArrayList<LockRequest>();
                        lreq.add(new LockRequest(lock.getParmId(), lock
                                .getTimeRange(), LockMode.BREAK_LOCK));
                        lockMgr.requestLockChange(lreq, lock.getWsId());
                        if (!inactives
                                .contains(lock.getWsId().toPrettyString())
                                && !inactives.contains(lock.getParmId()
                                        .toString())) {
                            String message = " Breaking orphaned lock on "
                                    + lock.getParmId().toString()
                                    + " owned by "
                                    + lock.getWsId().toPrettyString() + ".";
                            inactives.add(message);
                        }

                    }
                }
            }
        }
        return inactives;
    }

    @SuppressWarnings("unchecked")
    public void clearLocksCron() throws Exception {

        Date executionTime = new Date(System.currentTimeMillis());
        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            String msg = "Started at " + executionTime;
            statusHandler.info(msg);
        }

        Set<String> clients = new HashSet<String>(provider.getConnections());
        Set<String> inactives = new HashSet<String>();

        String siteId = EDEXUtil.getEdexSite();
        IFPServer ifpServer = IFPServer.getActiveServer(siteId);
        if (ifpServer == null) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                String msg = "No active IFPServer for site " + siteId;
                statusHandler.info(msg);
                return;
            }
        }
        LockManager lockMgr = ifpServer.getLockMgr();

        List<LockTable> lockTables = (List<LockTable>) lockMgr.getAllLocks()
                .getPayload();

        /*
         * There are no locks in the db.
         */
        if (lockTables.size() == 0) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                String msg = "No locks found for site " + siteId;
                statusHandler.info(msg);
                return;
            }
        }

        /*
         * Filter out non-CAVE clients.
         */
        for (Iterator<String> iterator = clients.iterator(); iterator.hasNext();) {
            String client = iterator.next();
            if (!client.contains(CAVE)) {
                iterator.remove();
            }
        }

        /*
         * If there are no active CAVE clients but the locks exist, they all
         * must be orphaned -> break the locks.
         */
        if (clients.isEmpty() && lockTables.size() > 0) {
            inactives = breakAllLocks(lockTables, lockMgr);
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                StringBuilder sb = new StringBuilder();
                for (String in : inactives) {
                    sb.append(in);
                }
                statusHandler.info(sb.toString());
                return;
            }
        }

        /*
         * There are active CAVE clients, find orphaned locks and break the
         * locks.
         */
        inactives = breakLocks(clients, lockTables, lockMgr);
        if (inactives.isEmpty()) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                String msg = "No orphaned locks found for site " + siteId;
                statusHandler.info(msg);
                return;
            }
        } else {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                StringBuilder sb = new StringBuilder();
                for (String in : inactives) {
                    sb.append(in);
                }
                statusHandler.info(sb.toString());
                return;
            }
        }
    }
}
