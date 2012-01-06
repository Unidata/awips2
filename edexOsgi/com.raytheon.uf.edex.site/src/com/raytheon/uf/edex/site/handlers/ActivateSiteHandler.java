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
package com.raytheon.uf.edex.site.handlers;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.site.notify.ClusterActivationNotification;
import com.raytheon.uf.common.site.notify.SendSiteActivationNotifications;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.site.requests.ActivateSiteRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.site.SiteActivationMonitor;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * Activate Site Handler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ActivateSiteHandler implements
        IRequestHandler<ActivateSiteRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActivateSiteHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(ActivateSiteRequest request) throws Exception {
        SiteAwareRegistry.getInstance().activateSite(request.getSiteID());
        new ActivateMonitor(request).start();
        return null;
    }

    private class ActivateMonitor extends Thread {

        private ActivateSiteRequest request;

        public ActivateMonitor(ActivateSiteRequest request) {
            this.request = request;
        }

        public void run() {
            try {
                Thread.sleep(3000);
                while (SiteActivationMonitor.getInstance()
                        .getPendingActivations(request.getPlugin(), request.getSiteID())
                        .size() > 0) {
                    Thread.sleep(1000);
                }
            } catch (InterruptedException e) {
                // ignore
            }
            ClusterActivationNotification notify = new ClusterActivationNotification(
                    SiteUtil.getSite(), request.getSiteID(), request.getPlugin(),
                    SiteActivationNotification.ACTIVATIONTYPE.ACTIVATE,
                    SiteActivationMonitor.getInstance().getStatus(), true);
            SiteActivationMonitor.getInstance().resetFailure();
            try {
                SendSiteActivationNotifications.send(notify);
            } catch (EdexException e) {
                statusHandler.error("Error sending site Activation message", e);
            }
        }
    }
}
