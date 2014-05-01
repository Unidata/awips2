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
package com.raytheon.edex.plugin.gfe.svcbackup;

import java.util.Date;
import java.util.TimerTask;

import com.raytheon.edex.plugin.gfe.server.handler.svcbu.CleanupSvcBuLogRequestHandler;
import com.raytheon.uf.common.dataplugin.gfe.request.CleanupSvcBuLogRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 3, 2013             jdynina     Initial creation
 * 
 * </pre>
 * 
 * @author jdynina
 * @version 1.0
 */

public class SvcbuCleanupLogsTask extends TimerTask {

    public SvcbuCleanupLogsTask(Date executionTime) {
        ServiceBackupNotificationManager
                .sendMessageNotification("Service backup log cleanup cron scheduled for execution at: "
                        + executionTime);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.TimerTask#run()
     */
    @Override
    public void run() {
        ServiceBackupNotificationManager
                .sendMessageNotification("Cleanup service configuration logs cron started.");
        try {
            new CleanupSvcBuLogRequestHandler()
                    .handleRequest(new CleanupSvcBuLogRequest());
        } catch (Exception e) {
            ServiceBackupNotificationManager
                    .sendErrorMessageNotification(
                            "Cleanup service backup logs cron failed to execute.",
                            e);
        }
    }
}
