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
package com.raytheon.edex.plugin.gfe.watch;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.UserMessageNotification;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Base class for a bean that accepts a {@code List} of
 * {@code AbstractWarningRecord}s and generates a set of notifications for the
 * GFE user if the storm affects their WFO.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2014  #3157     dgilling     Initial creation
 * Jun 10, 2014  #3268     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public abstract class AbstractWatchNotifierSrv {

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    protected final String watchType;

    protected final String supportedPIL;

    protected AbstractWatchNotifierSrv(String watchType, String supportedPIL) {
        this.watchType = watchType;
        this.supportedPIL = supportedPIL;
    }

    /**
     * Processes the warning records and generates a notification for each
     * currently activated GFE site if the storm affects the site.
     * 
     * @param pdos
     *            A list of {@code PluginDataObject}s that are assumed to be
     *            {@code AbstractWarningRecord}s all decoded from a common
     *            warning product.
     */
    public final void handleWatch(List<PluginDataObject> pdos) {
        List<AbstractWarningRecord> warningRecs = filterIncomingRecordsByPIL(pdos);
        if (warningRecs.isEmpty()) {
            String logMsg = String.format("%s notification:  not %s product",
                    watchType, supportedPIL);
            statusHandler.debug(logMsg);
            return;
        }

        /*
         * We are making an assumption that all PDOs came from the same source
         * product. This is a safe assumption because WarningDecoder processes
         * records one product at a time and the plugin notifier code that sends
         * those records to us does not do any additional grouping or batching.
         * 
         * Hence, any of the remaining records' raw message will be the same as
         * the rest and we can just use the first record's copy.
         */
        String productText = warningRecs.get(0).getRawmessage();
        Collection<String> wfos = WatchProductUtil.findAttnWFOs(productText);

        for (String siteid : IFPServer.getActiveSites()) {
            if (!wfos.contains(siteid)) {
                String logMsg = String.format(
                        "%s notification:  my site %s not in ATTN list",
                        watchType, siteid);
                statusHandler.debug(logMsg);
                continue;
            }

            VTECPartners partnersConfig = VTECPartners.getInstance(siteid);

            String msg = buildNotification(warningRecs, partnersConfig);
            if (msg != null) {
                sendNotification(msg, siteid);
            }
        }
    }

    /**
     * Given a list of {@code PluginDataObject}s that are actually
     * {@code AbstractWarningRecord}s, filters the list for only those records
     * which have the right PIL code.
     * 
     * @param pdos
     *            List of {@code AbstractWarningRecord}s to filter.
     * @return The list of supported {@code AbstractWarningRecord}s as defined
     *         by {@code getSupportedPIL}.
     */
    protected List<AbstractWarningRecord> filterIncomingRecordsByPIL(
            List<PluginDataObject> pdos) {
        List<AbstractWarningRecord> warningRecords = new ArrayList<AbstractWarningRecord>();
        for (PluginDataObject pdo : pdos) {
            AbstractWarningRecord warning = (AbstractWarningRecord) pdo;

            if (warning.getPil().startsWith(supportedPIL)) {
                warningRecords.add(warning);
            }
        }

        return warningRecords;
    }

    /**
     * Takes the specified list of warning records and {@code VTECPartners}
     * configuration data and builds a notification message to send.
     * 
     * @param decodedVTEC
     *            The warning records.
     * @param partnersConfig
     *            The {@code VTECPartners} configuration data.
     * @return The notification message to send to the users for the site, or
     *         {@code null} if no notification applies.
     */
    protected abstract String buildNotification(
            List<AbstractWarningRecord> decodedVTEC, VTECPartners partnersConfig);

    /**
     * Sends the specified notification message as an AletViz alert to all GFE
     * users connected as the specified site.
     * 
     * @param message
     *            The notification message text to send.
     * @param siteId
     *            The site identifier that will receive the message.
     * @return A {@code ServerResponse} containing error message if sending the
     *         notification message failed.
     */
    protected ServerResponse<?> sendNotification(String message, String siteId) {
        GfeNotification notification = new UserMessageNotification(message,
                Priority.CRITICAL, "GFE", siteId);
        return SendNotifications.send(notification);
    }
}
