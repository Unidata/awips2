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
package com.raytheon.edex.plugin.gfe.server.handler.svcbu;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.request.ProcessReceivedConfRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.ServiceBackupJobStatusNotification;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Request handler for {@code ProcessReceivedConfRequest}. Triggered when MHS
 * sends the requested configuration data from {@code ImportConfRequestHandler}
 * to this server. Will call the service backup script process_configuration to
 * validate and install the site's configuration data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            bphillip    Initial creation
 * Feb 13, 2015  #4103     dgilling    Pass site id to process_configuration.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class ProcessReceivedConfRequestHandler implements
        IRequestHandler<ProcessReceivedConfRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcessReceivedConfRequestHandler.class);

    @Override
    public JobProgress handleRequest(final ProcessReceivedConfRequest request)
            throws Exception {
        try {
            SvcBackupUtil.execute("process_configuration",
                    request.getReceivedConfFile(), request.getSiteID());
            statusHandler.info("Import Configuration Complete for site "
                    + request.getSiteID());
        } catch (Exception e) {
            statusHandler.error("Error Processing Configuration Data for site "
                    + request.getSiteID(), e);
            SendNotifications.send(new ServiceBackupJobStatusNotification(
                    "importConfiguration", JobProgress.FAILED, request
                            .getSiteID()));
            return JobProgress.FAILED;
        }

        SendNotifications
                .send(new ServiceBackupJobStatusNotification(
                        "importConfiguration", JobProgress.SUCCESS, request
                                .getSiteID()));

        return JobProgress.SUCCESS;
    }
}
