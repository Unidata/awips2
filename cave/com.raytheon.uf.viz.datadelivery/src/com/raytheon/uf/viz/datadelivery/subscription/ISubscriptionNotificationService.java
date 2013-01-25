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
package com.raytheon.uf.viz.datadelivery.subscription;

import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * Service to notify about subscription events.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2013 1441      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface ISubscriptionNotificationService {

    /**
     * Send a notification that pending subscription was created.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendCreatedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username);

    /**
     * Send a notification that a subscription was created.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendCreatedSubscriptionNotification(Subscription subscription,
            String username);

    /**
     * Send a notification that a subscription was updated.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendUpdatedSubscriptionNotification(Subscription subscription,
            String username);

    /**
     * Send a notification that a pending subscription was updated.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendUpdatedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username);

    /**
     * Send a notification that a subscription update is pending.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendCreatedPendingSubscriptionForSubscriptionNotification(
            InitialPendingSubscription pendingSub, String username);

    /**
     * Send a notification that a pending subscription was approved.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendApprovedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username);

    /**
     * Send a notification that a pending subscription was denied.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     * @param denyMessage
     *            the reason for denying
     */
    void sendDeniedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username,
            String denyMessage);

    /**
     * Send a notification that the subscription was deleted.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendDeletedSubscriptionNotification(Subscription subscription,
            String username);

    /**
     * Send a notification that the subscription was activated.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendSubscriptionActivatedNotification(Subscription subscription,
            String username);

    /**
     * Send a notification that the subscription was deactivated.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    void sendSubscriptionDeactivatedNotification(Subscription subscription,
            String username);
}
