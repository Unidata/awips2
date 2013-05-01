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

import com.raytheon.edex.plugin.gfe.server.handler.svcbu.ExportGridsRequestHandler;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest.ExportGridsMode;

/**
 * Cron job that exports GFE's primary sites' grids. Primary sites are
 * determined by a combination of the env. variable AW_SITE_IDENTIFIER and the
 * PRIMARY_SITES entry in svcbu.properties.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 03, 2011            bphillip     Initial creation
 * Apr 30, 2013  #1761     dgilling     Read list of sites to export grids
 *                                      for from svcbu.properties.
 * May 02, 2013  #1762     dgilling     Move code to read PRIMARY_SITES setting
 *                                      to SvcBackupUtil.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ExportGridsTask extends TimerTask {

    public ExportGridsTask(Date executionTime) {
        ServiceBackupNotificationManager
                .sendMessageNotification("Export grids cron scheduled for execution at: "
                        + executionTime);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.TimerTask#run()
     */
    @Override
    public void run() {
        final ExportGridsRequestHandler reqHandler = new ExportGridsRequestHandler();

        for (String site : SvcBackupUtil.getPrimarySites()) {
            ServiceBackupNotificationManager
                    .sendMessageNotification("Export Grids to central server cron started for site "
                            + site + ".");
            try {
                reqHandler.handleRequest(new ExportGridsRequest(site,
                        ExportGridsMode.CRON));
            } catch (Exception e) {
                ServiceBackupNotificationManager.sendErrorMessageNotification(
                        "Export Grids to central server cron failed to execute for site "
                                + site + ".", e);
            }
        }
    }
}
