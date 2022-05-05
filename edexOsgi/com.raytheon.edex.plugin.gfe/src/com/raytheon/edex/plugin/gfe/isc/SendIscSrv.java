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

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridHistoryUpdateNotification;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.core.EdexTimerBasedThread;

import jep.JepException;

/**
 * Service that runs ISC send jobs. Along with IscSendQueue, this class roughly
 * replaces AWIPS1's SendISCMgr.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 20, 2011           dgilling  Initial creation
 * May 19, 2014  2726     rjpeter   Integrate IscSendJob for graceful shutdown.
 * Feb 26, 2015  4128     dgilling  Switch to IFPServer.getActiveSites().
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 * Feb 02, 2017  3847     randerso  Renamed parmId field
 * Mar 30, 2017  5937     rjpeter   Use EdexTimerBasedThread logger.
 * Jun 03, 2019  7852     dgilling  Update code for jep 3.8.
 * Sep 05, 2019  7852     randerso  Remove caching of ISCScript objects which
 *                                  was breaking ISC when multiple sites were
 *                                  activated.
 * Nov 09, 2020  8259     randerso  Stop processing ISC send jobs when EDEX is
 *                                  shutting down
 *
 * </pre>
 *
 * @author dgilling
 */

public class SendIscSrv extends EdexTimerBasedThread {
    protected int runningTimeOutMillis;

    /** Date format for formatting dates for use with iscExtract script */
    protected static final SimpleDateFormat ISC_EXTRACT_DATE = new SimpleDateFormat(
            "yyyyMMdd_HHmm");

    /**
     * Default Constructor
     */
    public SendIscSrv() {
    }

    /**
     * @return the runningTimeOutMillis
     */
    public int getRunningTimeOutMillis() {
        return runningTimeOutMillis;
    }

    /**
     * @param runningTimeOutMillis
     *            the runningTimeOutMillis to set
     */
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
        } while (!running && record != null);
    }

    private void runIscSend(IscSendRecord request) {
        try {
            ParmID id = request.getParmId();
            TimeRange tr = request.getTimeRange();
            String xmlDest = request.getXmlDest();
            String siteId = id.getDbId().getSiteId();

            if (!IFPServer.getActiveSites().contains(siteId)) {
                logger.warn("Attempted to send " + id + " for deactivated site "
                        + siteId + ".");
                return;
            }

            logger.info("Starting isc for " + id.toString() + " "
                    + tr.toString() + " " + xmlDest);

            Map<String, Object> cmd = new HashMap<>();
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

            IFPServer ifpServer = IFPServer.getActiveServer(siteId);
            if (ifpServer == null) {
                logger.error("No active IFPServer for site: " + siteId);
                return;
            }
            IFPServerConfig config = ifpServer.getConfig();
            // IRT table address
            cmd.put("irtTableAddressA",
                    config.iscRoutingTableAddress().get("ANCF"));
            cmd.put("irtTableAddressB",
                    config.iscRoutingTableAddress().get("BNCF"));
            // xmt script
            cmd.put("transmitScript", config.transmitScript());
            // our server host, port, protocol, our mhs id, and our site id
            cmd.put("ourServerHost", config.getServerHost());
            cmd.put("ourServerPort", String.valueOf(config.getRpcPort()));
            cmd.put("ourServerProtocol",
                    String.valueOf(config.getProtocolVersion()));
            cmd.put("ourMHSid", config.getMhsid());
            cmd.put("ourSiteID", config.getSiteID());

            try (IscSendScript script = IscSendScriptFactory
                    .constructIscSendScript(siteId)) {
                script.execute(cmd);
            } catch (JepException e) {
                logger.error("Error executing iscExtract.", e);
                return;
            }

            try {
                DatabaseID dbId = id.getDbId();
                GridDatabase gridDb = ifpServer.getGridParmMgr()
                        .getDatabase(dbId);
                if (gridDb != null) {
                    ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = gridDb
                            .updateSentTime(id, tr, new Date());
                    if (sr.isOkay()) {
                        WsId wsId = new WsId(null, "ISC", "ISC");
                        List<GridHistoryUpdateNotification> notifications = new ArrayList<>(
                                1);
                        Map<TimeRange, List<GridDataHistory>> histories = sr
                                .getPayload();
                        notifications.add(new GridHistoryUpdateNotification(id,
                                histories, wsId, siteId));
                        SendNotifications.send(notifications);

                    } else {
                        logger.error(
                                "Error updating last sent times in GFERecords: "
                                        + sr.getMessages());
                    }
                } else {
                    // no such database exists
                    logger.error("Error processing ISC send request for :"
                            + dbId + ", the database does not exist.");
                }
            } catch (Exception e) {
                logger.error("Error updating last sent times in GFERecords.",
                        e);
            }

        } catch (Throwable t) {
            logger.error("Exception in SendIscSrv: ", t);
        }
    }
}
