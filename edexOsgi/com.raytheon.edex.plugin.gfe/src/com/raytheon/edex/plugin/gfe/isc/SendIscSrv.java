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
package com.raytheon.edex.plugin.gfe.isc;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridHistoryUpdateNotification;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.core.EdexTimerBasedThread;

/**
 * Service that runs ISC send jobs. Along with IscSendQueue, this class roughly
 * replaces AWIPS1's SendISCMgr.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling    Initial creation
 * May 19, 2014 2726       rjpeter     Integrate IscSendJob for graceful shutdown.
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SendIscSrv extends EdexTimerBasedThread {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SendIscSrv.class);

    protected int runningTimeOutMillis;

    /** Date format for formatting dates for use with iscExtract script */
    protected static final SimpleDateFormat ISC_EXTRACT_DATE = new SimpleDateFormat(
            "yyyyMMdd_HHmm");

    protected final ThreadLocal<Map<String, IscSendScript>> scripts = new ThreadLocal<Map<String, IscSendScript>>() {

        @Override
        protected Map<String, IscSendScript> initialValue() {
            return new HashMap<String, IscSendScript>();
        }

    };

    public SendIscSrv() {
    }

    public int getRunningTimeOutMillis() {
        return runningTimeOutMillis;
    }

    public void setRunningTimeOutMillis(int runningTimeOutMillis) {
        this.runningTimeOutMillis = runningTimeOutMillis;
    }

    @Override
    public String getThreadGroupName() {
        return "iscSendThreadPool";
    }

    @Override
    public void process() throws Exception {
        IscSendRecord record = null;
        do {
            record = SendIscTransactions.getNextSendJob(runningTimeOutMillis);
            if (record != null) {
                runIscSend(record);
                SendIscTransactions.removeSendJob(record);
            }
        } while (record != null);
    }

    @Override
    public void dispose() {
        super.dispose();
        // Make sure OS resources are released at thread death
        for (IscSendScript script : scripts.get().values()) {
            script.dispose();
        }
    }

    private void runIscSend(IscSendRecord request) {
        try {
            ParmID id = request.getParmID();
            TimeRange tr = request.getTimeRange();
            String xmlDest = request.getXmlDest();
            String siteId = id.getDbId().getSiteId();

            if (!GFESiteActivation.getInstance().getActiveSites()
                    .contains(siteId)) {
                statusHandler.warn("Attempted to send " + id
                        + " for deactivated site " + siteId + ".");
                return;
            }

            statusHandler.info("Starting isc for " + id.toString() + " "
                    + tr.toString() + " " + xmlDest);

            Map<String, Object> cmd = new HashMap<String, Object>();
            cmd.put("parmNames", Arrays.asList(id.getParmName()));
            cmd.put("databaseName", id.getDbId().getModelId());
            cmd.put("startTime", ISC_EXTRACT_DATE.format(tr.getStart()));
            cmd.put("endTime", ISC_EXTRACT_DATE.format(tr.getEnd()));

            // destination server XML, might be empty
            // the -D switch is a file location containing the xml
            // information
            if (!xmlDest.isEmpty()) {
                cmd.put("destinations", xmlDest);
            }

            try {
                IFPServerConfig config = IFPServerConfigManager
                        .getServerConfig(siteId);
                // IRT table address
                cmd.put("irtTableAddressA", config.iscRoutingTableAddress()
                        .get("ANCF"));
                cmd.put("irtTableAddressB", config.iscRoutingTableAddress()
                        .get("BNCF"));
                // xmt script
                cmd.put("transmitScript", config.transmitScript());
                // our server host, port, protocol, our mhs id, and our site id
                cmd.put("ourServerHost", config.getServerHost());
                cmd.put("ourServerPort", String.valueOf(config.getRpcPort()));
                cmd.put("ourServerProtocol",
                        String.valueOf(config.getProtocolVersion()));
                cmd.put("ourMHSid", config.getMhsid());
                cmd.put("ourSiteID", config.getSiteID().get(0));
            } catch (GfeConfigurationException e) {
                statusHandler.error(
                        "Unable to retrieve site configuration for site "
                                + siteId, e);
                return;
            }

            try {
                IscSendScript script = scripts.get().get(siteId);
                if (script == null) {
                    script = IscSendScriptFactory
                            .constructIscSendScript(siteId);
                    scripts.get().put(siteId, script);
                }
                script.execute(cmd);
            } catch (JepException e) {
                statusHandler.error("Error executing iscExtract.", e);
                return;
            }

            try {
                DatabaseID dbId = id.getDbId();
                IFPServer ifpServer = IFPServer.getActiveServer(dbId
                        .getSiteId());
                if (ifpServer != null) {
                    GridDatabase gridDb = ifpServer.getGridParmMgr()
                            .getDatabase(dbId);
                    if (gridDb != null) {
                        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = gridDb
                                .updateSentTime(id, tr, new Date());
                        if (sr.isOkay()) {
                            WsId wsId = new WsId(null, "ISC", "ISC");
                            List<GridHistoryUpdateNotification> notifications = new ArrayList<GridHistoryUpdateNotification>(
                                    1);
                            Map<TimeRange, List<GridDataHistory>> histories = sr
                                    .getPayload();
                            notifications
                                    .add(new GridHistoryUpdateNotification(id,
                                            histories, wsId, siteId));
                            SendNotifications.send(notifications);

                        } else {
                            statusHandler
                                    .error("Error updating last sent times in GFERecords: "
                                            + sr.getMessages());
                        }
                    } else {
                        // no such database exists
                        statusHandler
                                .error("Error processing ISC send request for :"
                                        + dbId
                                        + ", the database does not exist.");
                    }
                } else {
                    // no active server for request
                    statusHandler
                            .error("Error processing ISC send request for :"
                                    + dbId + ", no active IFPServer for site.");
                }
            } catch (Exception e) {
                statusHandler.error(
                        "Error updating last sent times in GFERecords.", e);
            }

        } catch (Throwable t) {
            statusHandler.error("Exception in SendIscSrv: ", t);
        }
    }
}
