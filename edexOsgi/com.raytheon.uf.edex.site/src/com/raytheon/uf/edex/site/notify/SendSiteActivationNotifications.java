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
package com.raytheon.uf.edex.site.notify;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Send site activation notifications to the SiteActivationMonitor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 09, 2011            bphillip    Initial creation
 * Feb 15, 2013 1638       mschenke    Moved site activation notifier class into edex.site
 * Sep 11, 2014 3622       randerso    Changed to use jms topic instead of vm queue so monitor
 *                                     sees messages from all JVMs.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SendSiteActivationNotifications {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SendSiteActivationNotifications.class);

    public static String SITE_ACTIVATION_URI = "jms-generic:topic:siteActivateNotification?timeToLive=60000";

    public static void send(SiteActivationNotification notification)
            throws EdexException {
        List<SiteActivationNotification> notifications = new ArrayList<SiteActivationNotification>();
        notifications.add(notification);
        send(notifications);
    }

    public static void send(List<SiteActivationNotification> notifications)
            throws EdexException {
        if (!notifications.isEmpty()) {
            try {
                String modeName = System.getProperty("edex.run.mode");
                String host = InetAddress.getLocalHost().getCanonicalHostName();
                for (SiteActivationNotification notify : notifications) {
                    notify.setRunMode(modeName);
                    notify.setServerName(host);
                    try {
                        EDEXUtil.getMessageProducer().sendAsyncUri(
                                SITE_ACTIVATION_URI,
                                SerializationUtil.transformToThrift(notify));
                    } catch (SerializationException e) {
                        statusHandler.error(
                                "Error transforming notification to thrift", e);
                    }
                }
            } catch (EdexException e) {
                statusHandler.error("Error sending gfe notification", e);
                throw e;
            } catch (UnknownHostException e) {
                statusHandler.error("Error resolving localhost name", e);
                throw new EdexException("Error resolving localhost name", e);
            }
        }
    }

}
