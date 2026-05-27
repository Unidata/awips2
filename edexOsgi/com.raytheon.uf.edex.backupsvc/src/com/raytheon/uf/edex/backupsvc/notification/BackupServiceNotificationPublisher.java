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
package com.raytheon.uf.edex.backupsvc.notification;

import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.raytheon.uf.common.backupsvc.notification.BackupServiceNotification;
import com.raytheon.uf.common.backupsvc.notification.BackupServiceNotification.NotificationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Publishes the notification into edex.alerts.backupsvc topic when a new backup
 * service job is created via localization file browser.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * Jun 28, 2021 84643      Gang Chen    Initial creation
 *
 * </pre>
 *
 * @author gchen
 * @version 1.0
 */
public class BackupServiceNotificationPublisher {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BackupServiceNotificationPublisher.class);

    private static final String URI = "jms-generic:topic:"
            + BackupServiceNotification.TOPIC + "?timeToLive=60000";

    /**
     * Publish a backup service notification on the topic
     *
     * @param backupSvcNotification
     *            The notification message to be published
     */
    protected static void publishNotification(
            BackupServiceNotification backupSvcNotification) {
        try {
            byte[] bytes = SerializationUtil
                    .transformToThrift(backupSvcNotification);
            EDEXUtil.getMessageProducer().sendAsyncUri(URI, bytes);
        } catch (EdexException e) {
            statusHandler.error(
                    "Unable to publish the backup service notification to the topic",
                    e);
        } catch (SerializationException e) {
            statusHandler.error(
                    "Unable to transform the backup service notification to bytes for transfer",
                    e);
        }
    }

    /**
     * @param type
     *            The type of notification
     * @param practice
     *            The practice or operational mode flag
     */
    public void notify(NotificationType type, boolean practice) {
        BackupServiceNotification backupSvcNotification = new BackupServiceNotification(
                type, practice);

        /*
         * If there is a transaction currently active, cache the notifications
         * until the transaction is complete
         */
        if (isTransactionActive()) {
            if (TransactionSynchronizationManager.isSynchronizationActive()) {
                TransactionSynchronizationManager.registerSynchronization(
                        new BackupServiceNotificationSynchronization(
                                backupSvcNotification));
            }
        } else {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug(
                        "Publishing the Backup Service notification from a non-transactional operation.");
            }
            publishNotification(backupSvcNotification);
        }
    }

    /**
     * Check to see if a transaction is active.
     *
     * @return true if a transaction is active
     */
    protected boolean isTransactionActive() {
        return TransactionSynchronizationManager.isActualTransactionActive();
    }
}
