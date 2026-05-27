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

import org.springframework.transaction.support.TransactionSynchronization;

import com.raytheon.uf.common.backupsvc.notification.BackupServiceNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Ensure the backup service notifications are not published until the
 * transaction is completed
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

public class BackupServiceNotificationSynchronization
        implements TransactionSynchronization {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BackupServiceNotificationSynchronization.class);

    /** The backup service message to be published */
    private BackupServiceNotification backupSvcNotification;

    public BackupServiceNotificationSynchronization(
            BackupServiceNotification backupSvcNotification) {
        this.backupSvcNotification = backupSvcNotification;
    }

    @Override
    public void afterCompletion(int status) {
        if (status == TransactionSynchronization.STATUS_COMMITTED) {
            BackupServiceNotificationPublisher
                    .publishNotification(backupSvcNotification);
        } else if (status == TransactionSynchronization.STATUS_ROLLED_BACK) {
            statusHandler.warn(
                    "Transaction rolled back. Discarding the notification: "
                            + backupSvcNotification.toString());
        } else {
            statusHandler.warn(
                    "Transaction encountered an unknown status. Discarding the notification: "
                            + backupSvcNotification.toString());
        }
    }

    @Override
    public void suspend() {
        // No op
    }

    @Override
    public void resume() {
        // No op
    }

    @Override
    public void flush() {
        // No op
    }

    @Override
    public void beforeCommit(boolean readOnly) {
        // No op
    }

    @Override
    public void beforeCompletion() {
        // No op
    }

    @Override
    public void afterCommit() {
        // No op
    }
}
