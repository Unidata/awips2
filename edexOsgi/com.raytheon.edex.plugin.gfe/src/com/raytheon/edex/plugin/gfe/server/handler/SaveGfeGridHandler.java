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

import java.util.Arrays;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.isc.IscSendQueue;
import com.raytheon.edex.plugin.gfe.isc.IscSendRecord;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.SaveGfeGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task for saving grids
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/30/08     #875       bphillip    Initial Creation
 * 01/29/09     #1271      njensen     Rewrote for thrift capabilities
 * 06/24/09                njensen     Added sending notifications
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SaveGfeGridHandler implements IRequestHandler<SaveGfeGridRequest> {
    protected final transient Log logger = LogFactory.getLog(getClass());

    @Override
    public ServerResponse<?> handleRequest(SaveGfeGridRequest request)
            throws Exception {
        ServerResponse<?> sr = null;
        String siteID = request.getSiteID();
        List<SaveGridRequest> saveRequest = request.getSaveRequest();

        try {
            sr = GridParmManager.saveGridData(saveRequest,
                    request.getWorkstationID(), siteID);

            IFPServerConfig serverConfig = IFPServerConfigManager
                    .getServerConfig(siteID);
            if (serverConfig.requestISC()) {
                for (SaveGridRequest save : saveRequest) {
                    DatabaseID dbid = save.getParmId().getDbId();
                    // ensure Fcst database
                    if (dbid.getModelName().equals("Fcst")
                            && dbid.getDbType().isEmpty()
                            && !save.getGridSlices().isEmpty()
                            && save.isClientSendStatus()) {
                        IscSendRecord sendReq = new IscSendRecord(
                                save.getParmId(),
                                save.getReplacementTimeRange(), "",
                                serverConfig.sendiscOnSave());
                        IscSendQueue.sendToQueue(Arrays.asList(sendReq));
                    }
                }
            }

        } catch (GfeException e) {
            logger.error("Error getting discrete or wx definition", e);
            sr = new ServerResponse<Object>();
            sr.addMessage("Error getting discrete or wx definition on server");
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
                logger.error("Error sending save notification", e);
                sr.addMessage("Error sending save notification - "
                        + e.getMessage());
            }
        }

        return sr;
    }
}
