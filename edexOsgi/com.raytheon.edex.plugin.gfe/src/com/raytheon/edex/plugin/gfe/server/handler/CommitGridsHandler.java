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
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.isc.IscSendQueue;
import com.raytheon.edex.plugin.gfe.isc.IscSendRecord;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.request.CommitGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.CommitGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockTableRequest;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task for commit grids to the official database
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/16/09                 njensen    Send notifications
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class CommitGridsHandler implements IRequestHandler<CommitGridsRequest> {
    protected final transient Log logger = LogFactory.getLog(getClass());

    @SuppressWarnings("unchecked")
    @Override
    public ServerResponse<List<GridUpdateNotification>> handleRequest(
            CommitGridsRequest request) throws Exception {
        ServerResponse<List<GridUpdateNotification>> sr = new ServerResponse<List<GridUpdateNotification>>();
        List<CommitGridRequest> commits = request.getCommits();
        WsId workstationID = request.getWorkstationID();
        String siteID = request.getSiteID();
        boolean clientSendStatus = false;
        if (!commits.isEmpty()) {
            clientSendStatus = commits.get(0).isClientSendStatus();
        }

        if (logger.isDebugEnabled()) {
            logger.debug(" Request: requestor="
                    + request.getWorkstationID().toPrettyString() + " request="
                    + commits.toString());
        }

        // check that there are not locks for each commit request
        for (CommitGridRequest commitRequest : commits) {
            sr.addMessages(lockCheckForCommit(commitRequest, workstationID,
                    siteID));
        }

        if (sr.isOkay()) {
            List<GridUpdateNotification> changes = new ArrayList<GridUpdateNotification>();
            ServerResponse<?> ssr = GridParmManager.commitGrid(commits,
                    workstationID, changes, siteID);
            sr.addMessages(ssr);
            sr.setPayload((List<GridUpdateNotification>) ssr.getPayload());
            for (GridUpdateNotification notification : changes) {
                sr.addNotifications(notification);
            }

            try {
                // check for sending to ISC
                IFPServerConfig serverConfig = IFPServerConfigManager
                        .getServerConfig(siteID);
                String iscrta = serverConfig.iscRoutingTableAddress().get(
                        "ANCF");
                if (sr.isOkay() && iscrta != null
                        && serverConfig.sendiscOnPublish() && clientSendStatus
                        && serverConfig.requestISC()) {
                    for (GridUpdateNotification change : changes) {
                        // ensure Official database
                        if (change.getParmId().getDbId().getModelName()
                                .equals("Official")
                                && change.getParmId().getDbId().getDbType()
                                        .isEmpty()) {
                            // queue them
                            IscSendRecord sendReq = new IscSendRecord(
                                    change.getParmId(),
                                    change.getReplacementTimeRange(), "",
                                    serverConfig.sendiscOnPublish());
                            IscSendQueue.sendToQueue(Arrays.asList(sendReq));
                        }
                    }
                }
            } catch (GfeConfigurationException e1) {
                logger.error("Unable to get server configuration for site ["
                        + siteID + "]", e1);
            }
        }
        if (sr.isOkay()) {
            try {
                ServerResponse<?> notifyResponse = SendNotifications.send(sr
                        .getNotifications());
                if (!notifyResponse.isOkay()) {
                    for (ServerMsg msg : notifyResponse.getMessages()) {
                        sr.addMessage(msg.getMessage());
                    }
                }
            } catch (Exception e) {
                logger.error("Error sending commit notification", e);
                sr.addMessage("Error sending commit notification - "
                        + e.getMessage());
            }
        }

        // log result
        if (logger.isDebugEnabled()) {
            logger.debug("COMMIT GRID Response: " + sr);
        }

        if (!sr.isOkay()) {
            logger.error("COMMIT GRID Request: requestor="
                    + workstationID.toPrettyString() + " request="
                    + commits.toString());
            logger.error("COMMIT GRID Problem: " + sr);
        }

        return sr;
    }

    private ServerResponse<?> lockCheckForCommit(CommitGridRequest request,
            WsId workstationID, String siteID) {
        ServerResponse<Object> sr = new ServerResponse<Object>();
        List<LockTable> lockTables = new ArrayList<LockTable>();
        LockTableRequest lockTableRequest = null;

        // make the request and get the lock tables
        if (request.isParmRequest()) {
            lockTableRequest = new LockTableRequest(request.getParmId());
        } else {
            lockTableRequest = new LockTableRequest(request.getDbId());
        }

        lockTables = LockManager.getInstance()
                .getLockTables(lockTableRequest, workstationID, siteID)
                .getPayload();
        if (sr.isOkay()) {
            for (int j = 0; j < lockTables.size(); j++) {
                if (lockTables.get(j).anyLocked(request.getTimeRange())) {
                    sr.addMessage("Data locked in commitTimeRange - commit failed commitTR="
                            + request.getTimeRange()
                            + " locks="
                            + lockTables.get(j));
                }
            }
        } else {
            sr.addMessage("Cannot verify there are no locks - commit failed");
        }

        return sr;
    }
}
