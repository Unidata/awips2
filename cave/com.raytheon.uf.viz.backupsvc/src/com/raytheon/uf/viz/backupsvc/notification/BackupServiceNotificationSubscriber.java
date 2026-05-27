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
package com.raytheon.uf.viz.backupsvc.notification;

import com.raytheon.uf.common.backupsvc.notification.BackupServiceNotification;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.backupsvc.ui.AbstractBackupServicesDialogTab;
import com.raytheon.uf.viz.backupsvc.ui.IncomingBackupServicesTab;
import com.raytheon.uf.viz.backupsvc.ui.OutgoingBackupServicesTab;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;

/**
 * This subscriber keeps the backup service dialog UI in sync with the database.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * Jun 28, 2021 84643      Gang Chen    Initial creation
 * Sep 22, 2021 96166      Gang Chen    Fix the bug for unable refreshing incoming BSJ status
 *
 * </pre>
 *
 * @author gchen
 */

public class BackupServiceNotificationSubscriber
        implements INotificationObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BackupServiceNotificationSubscriber.class);

    private static BackupServiceNotificationSubscriber instance = null;

    private AbstractBackupServicesDialogTab outgoingBackupSvcTab;

    private AbstractBackupServicesDialogTab incomingBackupSvcTab;

    private BackupServiceNotificationSubscriber(
            AbstractBackupServicesDialogTab outgoingBackupSvcTab,
            AbstractBackupServicesDialogTab incomingBackupSvcTab) {
        this.outgoingBackupSvcTab = outgoingBackupSvcTab;
        this.incomingBackupSvcTab = incomingBackupSvcTab;
    }

    public static synchronized BackupServiceNotificationSubscriber getInstance(
            AbstractBackupServicesDialogTab outgoingBackupSvcTab,
            AbstractBackupServicesDialogTab incomingBackupSvcTab) {
        if (instance == null) {
            instance = new BackupServiceNotificationSubscriber(
                    outgoingBackupSvcTab, incomingBackupSvcTab);
            NotificationManagerJob.addObserver(BackupServiceNotification.TOPIC,
                    instance);
        }
        return instance;
    }

    /**
     * Remove the alert message observer from the Notification Manager Job
     * listener.
     */
    public static synchronized void removeNotificationObserver() {
        if (instance != null) {
            NotificationManagerJob
                    .removeObserver(BackupServiceNotification.TOPIC, instance);
            instance = null;
        }
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage message : messages) {
            try {
                NotificationRun notificationRun = null;

                Object payload = message.getMessagePayload();
                if (payload instanceof BackupServiceNotification) {
                    notificationRun = new NotificationRun(
                            (BackupServiceNotification) payload,
                            this.outgoingBackupSvcTab,
                            this.incomingBackupSvcTab);
                } else {
                    // This line may be useful for other notifications from Edex
                    // to Edex.
                    // notificationRun = new NotificationRun((String) payload);
                    statusHandler
                            .error("Non-BackupServiceNotification message: "
                                    + (String) payload);
                }

                if (notificationRun != null) {
                    Thread thread = new Thread(notificationRun);
                    thread.start();
                }
            } catch (NotificationException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }
    }

    private static class NotificationRun implements Runnable {
        BackupServiceNotification backupSvcNotification;

        AbstractBackupServicesDialogTab outgoingBackupSvcTab,
                incomingBackupSvcTab;

        public NotificationRun(String p) {
        }

        public NotificationRun(BackupServiceNotification backupSvcNotification,
                AbstractBackupServicesDialogTab outgoingBackupSvcTab,
                AbstractBackupServicesDialogTab incomingBackupSvcTab) {
            this.backupSvcNotification = backupSvcNotification;
            this.outgoingBackupSvcTab = outgoingBackupSvcTab;
            this.incomingBackupSvcTab = incomingBackupSvcTab;
        }

        @Override
        public void run() {
            if (this.backupSvcNotification == null) {
                return;
            }

            if (this.backupSvcNotification
                    .getType() == BackupServiceNotification.NotificationType.OUTGOING) {
                if ((this.outgoingBackupSvcTab != null)
                        && (this.outgoingBackupSvcTab instanceof OutgoingBackupServicesTab)) {
                    statusHandler.info(
                            "Receive the backup service notification for outgoing DB table: "
                                    + this.backupSvcNotification.getType());
                    this.outgoingBackupSvcTab.handleRefresh();
                }
            } else if (this.backupSvcNotification
                    .getType() == BackupServiceNotification.NotificationType.INCOMING) {
                if ((this.incomingBackupSvcTab != null)
                        && (this.incomingBackupSvcTab instanceof IncomingBackupServicesTab)) {
                    statusHandler.info(
                            "receive the backup service notification for incoming DB table: "
                                    + this.backupSvcNotification.getType());
                    this.incomingBackupSvcTab.handleRefresh();
                }
            } else if (this.backupSvcNotification
                    .getType() == BackupServiceNotification.NotificationType.ALL) {
                if ((this.outgoingBackupSvcTab != null)
                        && (this.incomingBackupSvcTab != null)
                        && (this.outgoingBackupSvcTab instanceof OutgoingBackupServicesTab)
                        && (this.incomingBackupSvcTab instanceof IncomingBackupServicesTab)) {
                    statusHandler.info(
                            "receive the backup service notification for both outgoing and incoming DB tables: "
                                    + this.backupSvcNotification.getType());
                    this.outgoingBackupSvcTab.handleRefresh();
                    this.incomingBackupSvcTab.handleRefresh();
                }
            }
        }
    }
}