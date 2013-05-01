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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;
import java.util.Set;
import java.util.TimerTask;

import com.google.common.collect.Sets;
import com.raytheon.edex.plugin.gfe.server.handler.svcbu.ExportGridsRequestHandler;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest.ExportGridsMode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * TODO Add Description
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ExportGridsTask extends TimerTask {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportGridsTask.class);

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

        for (String site : getSites()) {
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

    private Collection<String> getSites() {
        Properties svcbuProps = SvcBackupUtil.getSvcBackupProperties();
        String siteList = SiteUtil.getSite();
        if (svcbuProps != null) {
            String propVal = svcbuProps.getProperty("PRIMARY_SITES", "").trim();
            if (!propVal.isEmpty()) {
                siteList = propVal;
            }
        }

        String[] sites = siteList.split(",");
        Collection<String> retVal = new ArrayList<String>(sites.length);
        Set<String> validSites = Sets.newHashSet(SiteAwareRegistry
                .getInstance().getActiveSites());
        for (String site : sites) {
            final String siteId = site.trim().toUpperCase();
            if (!siteId.isEmpty()) {
                if (validSites.contains(siteId)) {
                    retVal.add(siteId);
                } else {
                    statusHandler
                            .warn("Site "
                                    + siteId
                                    + " is not a currently activated site. Grids will not be exported for this site. Check the PRIMARY_SITES setting in svcbu.properties.");
                }
            }
        }

        return retVal;
    }
}
