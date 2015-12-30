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

import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.ServiceBackupMessageNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.ServiceBackupProgressNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ServiceBackupNotificationManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupNotificationManager.class);

    public static void sendProgressNotification(int progress) {
        ServiceBackupProgressNotification notification = new ServiceBackupProgressNotification(
                progress);
        notification.setSiteID(SiteUtil.getSite());
        SendNotifications.send(notification);
    }

    public static void sendMessageNotification(String message) {
        ServiceBackupMessageNotification notification = new ServiceBackupMessageNotification(
                message);
        notification.setSiteID(SiteUtil.getSite());
        SendNotifications.send(notification);
        statusHandler.info(message);
    }

    public static void sendErrorMessageNotification(String message, Exception e) {
        ServiceBackupMessageNotification notification = new ServiceBackupMessageNotification(
                message + "::" + e.getLocalizedMessage());
        notification.setSiteID(SiteUtil.getSite());
        SendNotifications.send(notification);
        statusHandler.error(message,e);
    }
}
