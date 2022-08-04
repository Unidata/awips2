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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.request.ClearPracticeGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Clear Practice Grids request handler
 *
 * This handler will remove all grids from the specified site's Prac_Fcst
 * database.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Dec 10, 2020  8302     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class ClearPracticeGridsHandler extends BaseGfeRequestHandler
        implements IRequestHandler<ClearPracticeGridsRequest> {
    protected final transient Logger logger = LoggerFactory
            .getLogger(getClass());

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    @Override
    public ServerResponse<?> handleRequest(ClearPracticeGridsRequest request)
            throws Exception {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        ServerResponse<?> clearResponse = new ServerResponse<>();
        try {
            String s = request.getSiteID() + "_GRID_Prac_Fcst_00000000_0000";
            DatabaseID dbId = new DatabaseID(s);
            IFPServer ifpServer = getIfpServer(request);
            LockManager lockMgr = ifpServer.getLockMgr();

            /* break and then re-lock the entire database */
            TimeRange allTimes = TimeRange.allTimes();
            List<LockRequest> lockRequests = Arrays.asList(
                    new LockRequest(dbId, allTimes, LockMode.BREAK_LOCK),
                    new LockRequest(dbId, allTimes, LockMode.LOCK));
            ServerResponse<List<LockTable>> lockResponse = lockMgr
                    .requestLockChange(lockRequests,
                            request.getWorkstationID());
            if (!lockResponse.isOkay()) {
                clearResponse.addMessage("Error locking " + dbId);
                clearResponse.addMessage(lockResponse.message());
                return clearResponse;
            }

            /* create lock notifications and save requests */
            List<LockTable> lockTables = lockResponse.getPayload();
            List<GfeNotification> notes = new ArrayList<>(lockTables.size());
            List<GFERecord> empty = Collections.emptyList();
            List<SaveGridRequest> saveRequests = new ArrayList<>(
                    lockTables.size());

            for (LockTable table : lockTables) {
                notes.add(new LockNotification(table, request.getSiteID()));

                List<Lock> locks = table.getLocks();
                if (!locks.isEmpty()) {
                    TimeRange tr = new TimeRange(locks.get(0).getStartDate(),
                            locks.get(locks.size() - 1).getEndDate());
                    saveRequests.add(
                            new SaveGridRequest(table.getParmId(), tr, empty));
                }
            }

            /* send lock notifications */
            ServerResponse<?> notifyResponse = SendNotifications.send(notes);
            if (!notifyResponse.isOkay()) {
                logger.error(notifyResponse.message());
            }

            /* send the requests to the GridParmManager to be processed */
            GridParmManager gpm = ifpServer.getGridParmMgr();
            ServerResponse<?> saveResponse = gpm.saveGridData(saveRequests,
                    request.getWorkstationID());
            clearResponse.addMessages(saveResponse);

            /* send grid update notifications */
            notifyResponse = SendNotifications
                    .send(saveResponse.getNotifications());
            if (!notifyResponse.isOkay()) {
                logger.error(notifyResponse.message());
            }

            /* release all the locks */
            lockRequests = Arrays
                    .asList(new LockRequest(dbId, allTimes, LockMode.UNLOCK));
            lockResponse = lockMgr.requestLockChange(lockRequests,
                    request.getWorkstationID());
            if (!lockResponse.isOkay()) {
                clearResponse.addMessage("Error unlocking " + dbId);
                clearResponse.addMessage(lockResponse.message());
                return clearResponse;
            }

            /* create lock notifications */
            lockTables = lockResponse.getPayload();
            notes = new ArrayList<>(lockTables.size());

            for (LockTable table : lockTables) {
                notes.add(new LockNotification(table, request.getSiteID()));
            }

            /* send lock notifications */
            notifyResponse = SendNotifications.send(notes);
            if (!notifyResponse.isOkay()) {
                logger.error(notifyResponse.message());
            }

        } finally {
            timer.stop();
            perfLog.logDuration(
                    "Clear Practice Grids for " + request.getSiteID(),
                    timer.getElapsedTime());
        }

        return clearResponse;
    }

}
