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
import java.util.List;
import java.util.Set;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
 * Mar 03, 2015  629    mgamazaychikov Initial creation
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

    public static void setProvider(IBrokerConnectionsProvider provider) {
        ClearGfeOrphanedLocks.provider = provider;
    }

    private void breakLocks(Set<String> clients, List<Lock> lockList,
            LockManager lockMgr, String siteId) {
        boolean foundOrpanedLocks = false;
        List<LockRequest> lreq = new ArrayList<LockRequest>();
        StringBuilder sb = new StringBuilder();
        for (Lock lock : lockList) {
            String lockWsid = lock.getWsId().toString();
            if (!clients.contains(lockWsid)) {
                foundOrpanedLocks = true;
                List<Lock> lst = new ArrayList<Lock>();
                lst.add(lock);
                // Inactive clients found
                lreq.add(new LockRequest(lock.getParmId(), lock.getTimeRange(),
                        LockMode.BREAK_LOCK));
                sb.append(" Breaking orphaned lock for site " + siteId + " on "
                        + lock.getParmId().toString() + " owned by "
                        + lock.getWsId().toPrettyString() + ".");

            }
        }
        if (foundOrpanedLocks) {
            statusHandler.info(sb.toString());
            WsId requestor = new WsId(null, null, "ClearGfeOrphanedLocks");
            ServerResponse<List<LockTable>> sr = lockMgr.requestLockChange(
                    lreq, requestor);
            if (sr.isOkay()) {
                try {
                    List<LockTable> lockTables = sr.getPayload();
                    List<GfeNotification> notes = new ArrayList<GfeNotification>(
                            lockTables.size());

                    for (LockTable table : lockTables) {
                        notes.add(new LockNotification(table, siteId));
                    }
                    ServerResponse<?> notifyResponse = SendNotifications
                            .send(notes);
                    if (!notifyResponse.isOkay()) {
                        statusHandler.error(notifyResponse.message());
                    }

                    // send out grid update notifications
                    notifyResponse = SendNotifications.send(sr.getNotifications());
                    if (!notifyResponse.isOkay()) {
                        statusHandler.error(notifyResponse.message());
                    }
                } catch (Exception e) {
                    statusHandler.error("Error sending lock notification", e);
                }
            } else {
                statusHandler.error(sr.message());
            } 
            return;
        } else {
            statusHandler.info(" No orphaned locks found for site " + siteId
                    + ".");
            return;
        }
    }

    public void clearLocksCron() throws Exception {
        statusHandler
                .info("Started at " + new Date(System.currentTimeMillis()));
        Set<String> clients = new HashSet<String>(provider.getConnections());
        if (IFPServer.getActiveServers().size() == 0) {
            statusHandler.info("No active IFPServer found.");
            return;
        }
        List<IFPServer> ifpServers = IFPServer.getActiveServers();
        for (IFPServer ifps : ifpServers) {
            LockManager lockMgr = ifps.getLockMgr();
            String siteId = ifps.getSiteId();
            List<Lock> lockList;
            try {
                lockList = (List<Lock>) lockMgr.getAllLocks(siteId);
                // find orphaned locks and break them
                breakLocks(clients, lockList, lockMgr, siteId);
            } catch (GfeException e) {
                statusHandler.error("Error retrieving all locks", e);
            }
        }
        return;
    }
}
