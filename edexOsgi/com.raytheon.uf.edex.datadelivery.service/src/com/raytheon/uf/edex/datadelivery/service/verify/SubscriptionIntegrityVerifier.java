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
package com.raytheon.uf.edex.datadelivery.service.verify;

import java.util.List;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.event.EventBus;

/**
 * Performs subscription integrity verification.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2012  1104      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionIntegrityVerifier {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionIntegrityVerifier.class);

    /**
     * Response from a verification.
     */
    static interface IVerificationResponse {
        /**
         * Check whether a notification should be sent.
         * 
         * @return true if a notification should be sent
         */
        boolean hasFailedVerification();

        /**
         * Get the notification message.
         * 
         * @return the message if {@link #hasFailedVerification()} returns true,
         *         results are undetermined it not
         */
        String getNotificationMessage();
    }

    /**
     * Contract for verifying the integrity of subscriptions with datasets.
     */
    static interface IVerificationStrategy {

        /**
         * Verifies that a {@link Subscription} is valid compared to the
         * {@link DataSet}.
         * 
         * @param dataSet
         *            the dataset
         * @param subscription
         *            the subscription
         * @return the verification response
         */
        IVerificationResponse verify(DataSet dataSet, Subscription subscription);
    }

    /**
     * Defines an action that should be performed when a subscription is
     * verified with the dataset.
     */
    static interface IVerificationAction {
        /**
         * Performs an action when a {@link Subscription} has been verified.
         * 
         * @param subscription
         *            the subscription
         * @param message
         */
        void verificationPerformed(Subscription subscription,
                IVerificationResponse response);
    }

    private final IVerificationStrategy verificationStrategy;

    private final List<IVerificationAction> successfulVerificationActions;

    private final List<IVerificationAction> failedVerificationActions;

    /**
     * Constructor.
     * 
     * @param verificationStrategy
     *            the verification strategy to use
     * @param successfulVerificationActions
     *            the actions that should be performed when verification
     *            succeeds
     * @param failedVerificationActions
     *            the actions that should be performed when verification fails
     */
    @VisibleForTesting
    SubscriptionIntegrityVerifier(IVerificationStrategy verificationStrategy,
            List<IVerificationAction> successfulVerificationActions,
            List<IVerificationAction> failedVerificationActions) {
        this.verificationStrategy = verificationStrategy;
        this.successfulVerificationActions = successfulVerificationActions;
        this.failedVerificationActions = failedVerificationActions;
    }

    /**
     * Create a new instance of the verifier.
     * 
     * @param verificationStrategy
     *            the verification strategy
     * @param failedVerificationActions
     *            the failed verification actions
     * @return the instance
     */
    public static SubscriptionIntegrityVerifier newInstance(
            IVerificationStrategy verificationStrategy,
            List<IVerificationAction> successfulVerificationActions,
            List<IVerificationAction> failedVerificationActions) {

        SubscriptionIntegrityVerifier verifier = new SubscriptionIntegrityVerifier(
                verificationStrategy, successfulVerificationActions,
                failedVerificationActions);

        EventBus.getInstance().register(verifier);

        return verifier;
    }

    /**
     * @param dataSet
     */
    public void dataSetUpdated(DataSet dataSet) {
        try {
            final List<Subscription> subscriptions = DataDeliveryHandlers
                    .getSubscriptionHandler()
                    .getActiveByDataSetAndProvider(dataSet.getDataSetName(),
                            dataSet.getProviderName());

            for (Subscription subscription : subscriptions) {

                if (!subscription.isValid()) {
                    continue;
                }

                final IVerificationResponse response = verificationStrategy
                        .verify(dataSet, subscription);

                List<IVerificationAction> verificationActions = response
                        .hasFailedVerification() ? failedVerificationActions
                        : successfulVerificationActions;

                for (IVerificationAction action : verificationActions) {
                    action.verificationPerformed(subscription, response);
                }
            }

        } catch (RegistryHandlerException e) {
            statusHandler
                    .handle(Priority.ERROR,
                            "Unable to retrieve subscriptions to verify them against the dataset update!",
                            e);
        }

    }

    @Subscribe
    @AllowConcurrentEvents
    public void eventListener(InsertRegistryEvent event) {
        String objectType = event.getObjectType();

        if (DataDeliveryRegistryObjectTypes.DATASET.equals(objectType)) {
            try {
                final DataSet dataSet = DataDeliveryHandlers
                        .getDataSetHandler().getById(event.getId());
                dataSetUpdated(dataSet);
            } catch (RegistryHandlerException e) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Unable to lookup the dataset by its id, verification will not be performed.",
                                e);
            }
        }
    }

}
