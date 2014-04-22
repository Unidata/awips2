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

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;

/**
 * Services for working with subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 7, 2012  1286      djohnson     Initial creation
 * Nov 20, 2012 1286      djohnson     Use IDisplay to display yes/no prompt.
 * Nov 28, 2012 1286      djohnson     Consolidate more notifications.
 * Jul 18, 2013 1653      mpduff       Added SubscriptionStatusSummary to ISubscriptionServiceResult
 * Oct 25, 2013 2292      mpduff       Move overlap checks to edex.
 * Mar 31, 2014 2889      dhladky      Added username for notification center tracking.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface ISubscriptionService {
    /**
     * Store the subscription.
     * 
     * @param subscription
     *            the subscription to store
     * @param displayTextStrategy
     * @return the result object
     * @throws RegistryHandlerException
     */
    SubscriptionServiceResult store(String username, Subscription subscription,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException;

    /**
     * Update the subscription.
     * 
     * @param subscription
     *            the subscription to update
     * @param displayTextStrategy
     * @return the result object
     */
    SubscriptionServiceResult update(String username, Subscription subscription,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException;

    /**
     * Update the subscriptions.
     * 
     * @param subscriptions
     *            the subscriptions to update
     * @param displayTextStrategy
     * @return the result object
     * @throws RegistryHandlerException
     */
    SubscriptionServiceResult update(String username, List<Subscription> subscriptions,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException;

    /**
     * Update the subscriptions, checking for an existing pending change
     * already.
     * 
     * @param subscriptions
     * @param displayTextStrategy
     * @return the result
     * @throws RegistryHandlerException
     */
    SubscriptionServiceResult updateWithPendingCheck(String username,
            List<Subscription> subscriptions,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException;

    /**
     * Store the adhoc subscription.
     * 
     * @param subscription
     *            the subscription to store
     * @param display
     *            the display to use to prompt the user
     * @param displayTextStrategy
     * @return the result object
     * @throws RegistryHandlerException
     */
    public SubscriptionServiceResult store(String username, AdhocSubscription sub,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException;
}
