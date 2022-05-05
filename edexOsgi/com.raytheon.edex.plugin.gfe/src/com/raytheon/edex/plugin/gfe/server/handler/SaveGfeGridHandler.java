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
import java.util.List;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.isc.IscSendQueue;
import com.raytheon.edex.plugin.gfe.isc.IscSendRecord;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.SaveGfeGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * GFE task for saving grids
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 30, 2008  875      bphillip  Initial Creation
 * Jan 29, 2009  1271     njensen   Rewrote for thrift capabilities
 * Jun 24, 2009           njensen   Added sending notifications
 * Sep 22, 2009  3058     rjpeter   Converted to IRequestHandler
 * Feb 12, 2013  1597     randerso  Added logging to support GFE Performance
 *                                  investigation
 * Jun 13, 2013  2044     randerso  Refactored to use IFPServer
 * Apr 03, 2014  2737     randerso  Changed to send ISC even when no grids are
 *                                  saved (i.e. on grid deletes)
 * Jan 13, 2015  3955     randerso  Enabled sending ISC grids for Topo
 * Oct 13, 2015  4961     randerso  Change modelName for NewTopo ISC.
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 * Feb 02, 2017  3847     randerso  Converted ISCSendQueue to proper singleton
 *
 * </pre>
 *
 * @author bphillip
 */
public class SaveGfeGridHandler extends BaseGfeRequestHandler
        implements IRequestHandler<SaveGfeGridRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveGfeGridHandler.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    @Override
    public ServerResponse<?> handleRequest(SaveGfeGridRequest request)
            throws Exception {
        ServerResponse<?> sr = null;
        List<SaveGridRequest> saveRequest = request.getSaveRequests();
        WsId workstationID = request.getWorkstationID();
        boolean clientSendStatus = request.isClientSendStatus();

        try {
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            sr = getIfpServer(request).getGridParmMgr()
                    .saveGridData(saveRequest, workstationID);
            timer.stop();
            perfLog.logDuration("Save Grids: GridParmManager.saveGridData",
                    timer.getElapsedTime());

            // TODO: move this post processing into GridParmManager
            // check for sending to ISC
            timer.reset();
            timer.start();
            IFPServerConfig serverConfig = getIfpServer(request).getConfig();
            String iscrta = serverConfig.iscRoutingTableAddress().get("ANCF");
            if (serverConfig.requestISC() && clientSendStatus
                    && (iscrta != null)) {
                List<IscSendRecord> iscSendRequests = new ArrayList<>(
                        saveRequest.size());
                for (SaveGridRequest save : saveRequest) {
                    DatabaseID dbid = save.getParmId().getDbId();

                    // ensure Fcst database or Topo database
                    if (("Fcst".equals(dbid.getModelName())
                            && dbid.getDbType().isEmpty())
                            || ("NewTerrain".equals(dbid.getModelName())
                                    && "EditTopo".equals(dbid.getDbType()))) {
                        IscSendRecord sendReq = new IscSendRecord(
                                save.getParmId(),
                                save.getReplacementTimeRange(), "",
                                serverConfig.sendiscOnSave());
                        iscSendRequests.add(sendReq);
                    }
                }
                IscSendQueue.getInstance().sendToQueue(iscSendRequests);

                timer.stop();
                perfLog.logDuration("Save Grids: Queueing ISC send requests",
                        timer.getElapsedTime());
            }

        } catch (GfeException e) {
            statusHandler.error("Error getting discrete or wx definition", e);
            sr = new ServerResponse<>();
            sr.addMessage("Error getting discrete or wx definition on server");
        }

        if (sr.isOkay()) {
            try {
                ITimer timer = TimeUtil.getTimer();
                timer.start();
                ServerResponse<?> notifyResponse = SendNotifications
                        .send(sr.getNotifications());
                if (!notifyResponse.isOkay()) {
                    for (ServerMsg msg : notifyResponse.getMessages()) {
                        sr.addMessage(msg.getMessage());
                    }
                }
                timer.stop();
                perfLog.logDuration("Save Grids: Sending notifications",
                        timer.getElapsedTime());
            } catch (Exception e) {
                statusHandler.error("Error sending save notification", e);
                sr.addMessage(
                        "Error sending save notification - " + e.getMessage());
            }
        }

        return sr;
    }
}
