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

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.swt.widgets.Shell;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.IProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.event.notification.ApprovedPendingSubscriptionNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.BaseSubscriptionNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.DeniedPendingSubscriptionNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.PendingSubscriptionNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.SubscriptionNotificationRequest;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;

/**
 * Basic implementation of the {@link ISubscriptionService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 07, 2012 1286       djohnson     Initial creation
 * Nov 20, 2012 1286       djohnson     Use propose schedule methods to see effects of subscription scheduling.
 * Nov 28, 2012 1286       djohnson     Add more notification methods.
 * Dec 11, 2012 1404       mpduff       Add message to sendDeletedSubscriptionNotification.
 * Dec 11, 2012 1403       djohnson     Adhoc subscriptions no longer go to the registry.
 * Dec 18, 2012 1443       bgonzale     Open force apply prompt pop-up on the UI thread.
 * Dec 20, 2012 1413       bgonzale     Added new pending approve and denied request and responses.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionService implements ISubscriptionService {
    /**
     * Implementation of {@link IDisplayForceApplyPrompt} that uses an SWT
     * dialog.
     */
    public class DisplayForceApplyPrompt implements IDisplayForceApplyPrompt {

        private ForceApplyPromptResponse forceApplyPromptResponse = ForceApplyPromptResponse.CANCEL;

        /**
         * {@inheritDoc}
         * 
         * @param subscription
         */
        @Override
        public ForceApplyPromptResponse displayForceApplyPrompt(String title,
                String message, int requiredLatency,
                IForceApplyPromptDisplayText displayTextStrategy,
                Subscription subscription,
                Set<String> wouldBeUnscheduledSubscriptions) {
            DisplayForceApplyPromptDialog dlg = new DisplayForceApplyPromptDialog(
                    title, message, requiredLatency, displayTextStrategy,
                    subscription, wouldBeUnscheduledSubscriptions);
            forceApplyPromptResponse = (ForceApplyPromptResponse) dlg.open();
            return forceApplyPromptResponse;
        }

        /**
         * get the response from the last call to the displayForceApplyPrompt
         * method.
         */
        @Override
        public ForceApplyPromptResponse getForceApplyPromptResponse() {
            return forceApplyPromptResponse;
        }

    }

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionService.class);

    @VisibleForTesting
    final String TITLE = "Subscription";

    /**
     * Implementation of {@link ISubscriptionServiceResult}.
     */
    private final class SubscriptionServiceResult implements
            ISubscriptionServiceResult {

        private final boolean allowFurtherEditing;

        private final String message;

        private SubscriptionServiceResult(String message) {
            this(false, message);
        }

        private SubscriptionServiceResult(boolean allowFurtherEditing,
                String message) {
            this.allowFurtherEditing = allowFurtherEditing;
            this.message = message;
        }

        /**
         * @param b
         */
        public SubscriptionServiceResult(boolean allowFurtherEditing) {
            this(allowFurtherEditing, null);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isAllowFurtherEditing() {
            return allowFurtherEditing;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean hasMessageToDisplay() {
            return message != null;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String getMessageToDisplay() {
            return message;
        }
    }

    /**
     * Result class used internally to denote whether the user should be
     * prompted, and any result messages.
     */
    private final class ProposeResult {
        private final boolean promptUser;

        private final String message;

        private final int requiredLatency;

        private final Set<String> wouldBeUnscheduledSubscriptions;

        private ProposeResult(boolean promptUser, String message,
                int requiredLatency, Set<String> wouldBeUnscheduledSubscriptions) {
            this.promptUser = promptUser;
            this.message = message;
            this.requiredLatency = requiredLatency;
            this.wouldBeUnscheduledSubscriptions = wouldBeUnscheduledSubscriptions;
        }
    }

    /**
     * Used to represent a runnable that can throw a registry exception.
     */
    private interface RegistryRunnable {
        void run() throws RegistryHandlerException;
    }

    /**
     * Enumeration of force apply responses.
     */
    public static enum ForceApplyPromptResponse {
        CANCEL, INCREASE_LATENCY, FORCE_APPLY;
    }

    /**
     * Interface representing shelling a force apply prompt.
     */
    @VisibleForTesting
    static interface IDisplayForceApplyPrompt {
        /**
         * Display the force apply prompt.
         * 
         * @param title
         * @param message
         * @param requiredLatency
         * @param displayTextStrategy
         * @param wouldBeUnscheduledSubscriptions
         * @return the response
         */
        ForceApplyPromptResponse displayForceApplyPrompt(String title,
                String message, int requiredLatency,
                IForceApplyPromptDisplayText displayTextStrategy,
                Subscription subscription,
                Set<String> wouldBeUnscheduledSubscriptions);

        ForceApplyPromptResponse getForceApplyPromptResponse();
    }

    /**
     * Interface that must be implemented by classes that will be showing a
     * force apply prompt message.
     */
    public static interface IForceApplyPromptDisplayText {
        /**
         * Retrieve the display text that will be displayed for each option.
         * 
         * @param option
         *            the option
         * @param requiredLatency
         *            the required latency that would be required to schedule
         *            the item(s)
         * @param subscription
         *            the subscription that would require the increased latency,
         *            or null if this is a multi-subscription operation
         * @param wouldBeUnscheduledSubscriptions
         *            the subscription names that would be unscheduled
         * @return the display text, or null if the option should not be
         *         displayed
         */
        String getOptionDisplayText(ForceApplyPromptResponse option,
                int requiredLatency, Subscription subscription,
                Set<String> wouldBeUnscheduledSubscriptions);

        /**
         * Get the shell to use.
         * 
         * @return the shell
         */
        Shell getShell();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ISubscriptionServiceResult store(final Subscription subscription,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final List<Subscription> subscriptions = Arrays.asList(subscription);
        final RegistryRunnable action = new RegistryRunnable() {
            @Override
            public void run() throws RegistryHandlerException {
                DataDeliveryHandlers.getSubscriptionHandler().store(
                        subscription);
            }
        };

        final String successMessage = "Subscription " + subscription.getName()
                + " has been created.";

        return performAction(subscriptions, action, successMessage,
                displayTextStrategy);
    }

    /**
     * {@inheritDoc}
     * 
     */
    @Override
    public ISubscriptionServiceResult update(final Subscription subscription,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final List<Subscription> subscriptions = Arrays.asList(subscription);
        final RegistryRunnable action = new RegistryRunnable() {
            @Override
            public void run() throws RegistryHandlerException {
                DataDeliveryHandlers.getSubscriptionHandler().update(
                        subscription);
            }
        };
        final String successMessage = "Subscription " + subscription.getName()
                + " has been updated.";

        return performAction(subscriptions, action, successMessage,
                displayTextStrategy);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ISubscriptionServiceResult update(final List<Subscription> subs,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final RegistryRunnable action = new RegistryRunnable() {
            @Override
            public void run() throws RegistryHandlerException {
                for (Subscription sub : subs) {
                    DataDeliveryHandlers.getSubscriptionHandler().update(sub);
                }
            }
        };

        final String successMessage = "The subscriptions have been updated.";

        return performAction(subs, action, successMessage, displayTextStrategy);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ISubscriptionServiceResult store(final AdhocSubscription sub,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final List<Subscription> subscriptions = Arrays
                .<Subscription> asList(sub);
        final RegistryRunnable action = new RegistryRunnable() {
            @Override
            public void run() throws RegistryHandlerException {
                // Adhoc subscriptions don't interact with the registry any
                // longer, so it gets a blank implementation
            }
        };
        final String successMessage = "The query was successfully stored.";

        SubscriptionServiceResult result = performAction(subscriptions, action,
                successMessage, displayTextStrategy);
        if (!result.allowFurtherEditing) {
            Date date = getBandwidthService().getEstimatedCompletionTime(sub);
            if (date != null) {
                SimpleDateFormat sdf = new SimpleDateFormat(
                        "MM/dd/yyyy HH:mm zzz");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
                result = new SubscriptionServiceResult(
                        result.getMessageToDisplay()
                                + "\n\nEstimated completion time:"
                                + sdf.format(date));
            }
        }

        return result;
    }

    /**
     * Performs the action on the subscriptions. If the action would cause
     * subscriptions to be unscheduled, the user is prompted whether or not they
     * would like to continue with the action forcibly. If so, the action is
     * performed and the affected subscriptions are updated to be in the
     * unscheduled state.
     * 
     * @param subscriptions
     * @param action
     * @param successMessage
     * @param displayTextStrategy
     * @return the result object
     * @throws RegistryHandlerException
     */
    private SubscriptionServiceResult performAction(
            List<Subscription> subscriptions, RegistryRunnable action,
            String successMessage,
            final IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        try {
            final ProposeResult result = proposeScheduleAndAction(
                    subscriptions, action, successMessage);

            if (result.promptUser) {
                final Subscription subscription = (subscriptions.size() == 1) ? subscriptions
                        .get(0) : null;
                final IDisplayForceApplyPrompt forceApplyPrompt = getForceApplyPromptDisplay();

                VizApp.runSync(new Runnable() {
                    @Override
                    public void run() {
                        forceApplyPrompt.displayForceApplyPrompt(TITLE,
                                result.message, result.requiredLatency,
                                displayTextStrategy, subscription,
                                result.wouldBeUnscheduledSubscriptions);
                    }
                });
                switch (forceApplyPrompt.getForceApplyPromptResponse()) {
                case INCREASE_LATENCY:
                    subscription.setLatencyInMinutes(result.requiredLatency);
                    // Intentional fall-through
                case FORCE_APPLY:
                    // Have to make sure we set them to not be unscheduled, let
                    // the bandwidth manager decide they can't be scheduled
                    for (Subscription temp : subscriptions) {
                        temp.setUnscheduled(false);
                    }
                    action.run();

                    final Set<String> unscheduled = getBandwidthService()
                            .schedule(subscriptions);
                    updateSubscriptionsByNameToUnscheduled(unscheduled);

                    StringBuilder sb = new StringBuilder(successMessage);
                    getUnscheduledSubscriptionsPortion(sb, unscheduled);

                    return new SubscriptionServiceResult(sb.toString());
                case CANCEL:
                    return new SubscriptionServiceResult(true);
                default:
                    throw new IllegalArgumentException(
                            "Unknown force apply prompt response!  Did you add a new type that must be handled?");
                }
            }

            return new SubscriptionServiceResult(result.message);
        } catch (RegistryHandlerException e) {
            // The in-memory objects must be corrupted since we schedule first,
            // then store to the registry, so a reinitialize is called for
            getBandwidthService().reinitialize();

            throw e;
        }
    }

    /**
     * Get the force apply prompt for display
     * 
     * @return the force apply prompt display
     */
    IDisplayForceApplyPrompt getForceApplyPromptDisplay() {
        return new DisplayForceApplyPrompt();
    }

    /**
     * Proposes scheduling the subscriptions (with any modifications that have
     * been made) in the bandwidth manager. If subscriptions would be
     * unscheduled as a result, then a message is returned designating such.
     * 
     * @param subscriptions
     * @param registryAction
     * @param successMessage
     * @return the result
     * @throws RegistryHandlerException
     */
    private ProposeResult proposeScheduleAndAction(
            List<Subscription> subscriptions, RegistryRunnable registryAction,
            String successMessage) throws RegistryHandlerException {

        IProposeScheduleResponse proposeScheduleresponse = getBandwidthService()
                .proposeSchedule(subscriptions);
        Set<String> unscheduledSubscriptions = proposeScheduleresponse
                .getUnscheduledSubscriptions();
        boolean wouldUnscheduleSubs = !unscheduledSubscriptions.isEmpty();

        String response = successMessage;
        if (wouldUnscheduleSubs) {
            response = getWouldCauseUnscheduledSubscriptionsPortion(
                    unscheduledSubscriptions, subscriptions);
        } else {
            registryAction.run();
        }

        return new ProposeResult(wouldUnscheduleSubs, response,
                proposeScheduleresponse.getRequiredLatency(),
                unscheduledSubscriptions);
    }

    /**
     * Appends the unscheduled subscriptions portion to the StringBuilder.
     * 
     * @param unscheduledSubscriptions
     *            the unscheduled subscriptions
     * @param subscriptions
     *            the subscriptions which were attempting to schedule
     */
    private String getWouldCauseUnscheduledSubscriptionsPortion(
            Set<String> unscheduledSubscriptions,
            List<Subscription> subscriptions) {
        StringBuilder msg = new StringBuilder();

        // Handle the case where it's just the subscription we're changing
        // itself that would not schedule
        if ((subscriptions.size() == 1 && unscheduledSubscriptions.size() == 1)
                && (subscriptions.get(0).getName()
                        .equals(unscheduledSubscriptions.iterator().next()))) {
            final Subscription subscription = subscriptions.get(0);
            msg.append(
                    (subscription instanceof AdhocSubscription) ? "The query"
                            : "Subscription " + subscription.getName())
                    .append(" would not fully schedule with the bandwidth management system if this action were performed.");
        } else {
            msg.append(StringUtil
                    .createMessage(
                            "The following subscriptions would not fully schedule with the bandwidth management system if this action were performed:",
                            unscheduledSubscriptions));
        }

        msg.append("\n\nWhat would you like to do?");

        return msg.toString();
    }

    /**
     * Appends the unscheduled subscriptions portion to the StringBuilder.
     * 
     * @param unscheduledSubscriptions
     *            the unscheduled subscriptions
     */
    private void getUnscheduledSubscriptionsPortion(StringBuilder msg,
            Set<String> unscheduledSubscriptions) {
        if (unscheduledSubscriptions.isEmpty()) {
            return;
        }
        msg.append(StringUtil
                .createMessage(
                        "\n\nThe following subscriptions did not fully schedule with the bandwidth management system:",
                        unscheduledSubscriptions));
    }

    /**
     * Get the bandwidth service to use.
     * 
     * @return the bandwidth service
     */
    @VisibleForTesting
    IBandwidthService getBandwidthService() {
        return DataDeliveryServices.getBandwidthService();
    }

    /**
     * Send a notification that a subscription has been created.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    @Override
    public void sendCreatedSubscriptionNotification(Subscription subscription,
            String username) {
        BaseSubscriptionNotificationRequest<Subscription> req = new SubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");

        String msg = "Subscription " + subscription.getName()
                + " has been created.";
        req.setMessage(msg);
        req.setSubscription(subscription);
        req.setPriority(3);
        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendCreatedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username) {

        BaseSubscriptionNotificationRequest<InitialPendingSubscription> req = new PendingSubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");

        req.setSubscription(subscription);
        req.setPriority(2);
        req.setMessage("Pending Subscription " + subscription.getName()
                + " has been created.");

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendUpdatedSubscriptionNotification(Subscription subscription,
            String username) {
        BaseSubscriptionNotificationRequest<Subscription> req = new SubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");

        String msg = "Subscription " + subscription.getName()
                + " has been updated.";
        req.setMessage(msg);
        req.setSubscription(subscription);
        req.setPriority(3);

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendUpdatedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username) {

        BaseSubscriptionNotificationRequest<InitialPendingSubscription> req = new PendingSubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");

        req.setMessage("Edited subscription " + subscription.getName());
        req.setSubscription(subscription);
        req.setPriority(2);

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendCreatedPendingSubscriptionForSubscriptionNotification(
            InitialPendingSubscription subscription, String username) {
        BaseSubscriptionNotificationRequest<InitialPendingSubscription> req = new PendingSubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");

        req.setMessage("Edited subscription " + subscription.getName());
        req.setSubscription(subscription);
        req.setPriority(2);

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendApprovedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username) {
        ApprovedPendingSubscriptionNotificationRequest req = new ApprovedPendingSubscriptionNotificationRequest();
        req.setMessage("Approved subscription " + subscription.getName());
        req.setUserId(username);
        req.setCategory("Subscription Approved");
        req.setPriority(2);
        req.setSubscription(subscription);

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendDeniedPendingSubscriptionNotification(
            InitialPendingSubscription subscription, String username,
            String denyMessage) {
        DeniedPendingSubscriptionNotificationRequest req = new DeniedPendingSubscriptionNotificationRequest();
        req.setMessage(denyMessage);
        req.setUserId(username);
        req.setCategory("Subscription Approval Denied");
        req.setPriority(2);
        req.setId(subscription.getId());

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendDeletedSubscriptionNotification(Subscription subscription,
            String username) {
        SubscriptionNotificationRequest req = new SubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");
        req.setPriority(3);
        req.setMessage(subscription.getName() + " Deleted");
        subscription.setDeleted(true);
        req.setSubscription(subscription);

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendSubscriptionActivatedMessage(Subscription subscription,
            String username) {
        SubscriptionNotificationRequest req = new SubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");
        req.setPriority(3);
        req.setMessage(subscription.getName() + " Activated");

        sendRequest(req);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendSubscriptionDeactivatedMessage(Subscription subscription,
            String username) {
        SubscriptionNotificationRequest req = new SubscriptionNotificationRequest();
        req.setUserId(username);
        req.setCategory("Subscription");
        req.setPriority(3);
        req.setMessage(subscription.getName() + " Deactivated");

        sendRequest(req);
    }

    /**
     * Send the notification request.
     * 
     * @param req
     */
    private void sendRequest(BaseSubscriptionNotificationRequest<?> req) {
        try {
            ThriftClient.sendRequest(req);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    private void updateSubscriptionsByNameToUnscheduled(
            java.util.Collection<String> subscriptionNames)
            throws RegistryHandlerException {
        ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        for (String subName : subscriptionNames) {
            Subscription unscheduledSub = subscriptionHandler
                    .getByName(subName);
            if (unscheduledSub == null) {
                continue;
            }
            unscheduledSub.setUnscheduled(true);
            subscriptionHandler.update(unscheduledSub);
        }
    }
}
